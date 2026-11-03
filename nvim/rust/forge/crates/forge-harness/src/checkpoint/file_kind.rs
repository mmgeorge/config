use std::fs;
use std::path::Path;

/// Distinguish restorable filesystem entries before opening their contents.
pub(super) enum FileKind {
    Regular,
    Symlink,
    Excluded,
}

/// Classify special files without opening their socket, pipe, or device contents.
pub(super) fn classify(path: &Path, metadata: &fs::Metadata) -> std::io::Result<FileKind> {
    if metadata.is_symlink() {
        return Ok(FileKind::Symlink);
    }
    if !metadata.is_file() {
        return Ok(FileKind::Excluded);
    }
    #[cfg(windows)]
    {
        use std::os::windows::fs::{MetadataExt, OpenOptionsExt};
        use std::os::windows::io::AsRawHandle;
        use windows::Win32::Foundation::HANDLE;
        use windows::Win32::Storage::FileSystem::{
            FILE_ATTRIBUTE_REPARSE_POINT, FILE_ATTRIBUTE_TAG_INFO, FILE_FLAG_BACKUP_SEMANTICS,
            FILE_FLAG_OPEN_REPARSE_POINT, FILE_READ_ATTRIBUTES, FileAttributeTagInfo,
            GetFileInformationByHandleEx,
        };
        if metadata.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT.0 != 0 {
            let file = fs::OpenOptions::new()
                .access_mode(FILE_READ_ATTRIBUTES.0)
                .custom_flags(FILE_FLAG_OPEN_REPARSE_POINT.0 | FILE_FLAG_BACKUP_SEMANTICS.0)
                .open(path)?;
            let mut tag = FILE_ATTRIBUTE_TAG_INFO::default();
            // SAFETY: the open file owns the handle; tag is writable for the supplied size.
            unsafe {
                GetFileInformationByHandleEx(
                    HANDLE(file.as_raw_handle()),
                    FileAttributeTagInfo,
                    (&raw mut tag).cast(),
                    u32::try_from(std::mem::size_of_val(&tag)).expect("tag information fits u32"),
                )
            }
            .map_err(|error| std::io::Error::from_raw_os_error(error.code().0 & 0xffff))?;
            // IO_REPARSE_TAG_AF_UNIX from the Windows SDK: metadata reports sockets as files.
            if tag.ReparseTag == 0x8000_0023 {
                return Ok(FileKind::Excluded);
            }
        }
    }
    #[cfg(not(windows))]
    let _ = path;
    Ok(FileKind::Regular)
}

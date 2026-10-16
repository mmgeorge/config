use anyhow::Result;
use serde::Serialize;

pub(crate) fn serialized_size(value: &impl Serialize) -> Result<usize> {
    struct ByteCounter {
        bytes: usize,
    }
    impl std::io::Write for ByteCounter {
        fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
            self.bytes = self.bytes.checked_add(bytes.len())
                .ok_or_else(|| std::io::Error::other("serialized byte count overflow"))?;
            Ok(bytes.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }
    let mut writer = ByteCounter { bytes: 0 };
    serde_json::to_writer(&mut writer, value)?;
    Ok(writer.bytes)
}

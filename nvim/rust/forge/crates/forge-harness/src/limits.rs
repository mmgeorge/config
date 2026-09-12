use anyhow::Result;
use serde::Serialize;

pub(crate) fn serialized_size(value: &impl Serialize, limit: usize) -> Result<usize> {
    struct SizeLimit {
        bytes: usize,
        limit: usize,
    }
    impl std::io::Write for SizeLimit {
        fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
            if bytes.len() > self.limit.saturating_sub(self.bytes) {
                return Err(std::io::Error::other("serialized resource limit exceeded"));
            }
            self.bytes += bytes.len();
            Ok(bytes.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }
    let mut writer = SizeLimit { bytes: 0, limit };
    serde_json::to_writer(&mut writer, value)?;
    Ok(writer.bytes)
}

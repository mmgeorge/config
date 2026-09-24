use forge_buffer::block::TextChunk;

#[derive(Clone, Copy, Debug)]
/// The file-level change rendered by status and saved-patch views.
pub enum FileChange {
    Added,
    Deleted,
    Renamed,
    Copied,
    Conflicted,
    Modified,
}

impl FileChange {
    /// Return the stable inventory change name.
    pub fn name(self) -> &'static str {
        match self {
            Self::Added => "added",
            Self::Deleted => "deleted",
            Self::Renamed => "renamed",
            Self::Copied => "copied",
            Self::Conflicted => "conflicted",
            Self::Modified => "modified",
        }
    }

    /// Format one logical file row without applying prose wrapping to its path.
    pub fn header(self, path: &str, counts: Option<(u64, u64)>, untracked: bool) -> Vec<TextChunk> {
        let (label, capture) = match self {
            Self::Added => ("New", "ForgeStatusFileNew"),
            Self::Deleted => ("Deleted", "ForgeStatusFileDeleted"),
            Self::Renamed => ("Renamed", "ForgeStatusFileRenamed"),
            Self::Copied => ("Copied", "ForgeStatusFileNew"),
            Self::Conflicted => ("Conflict", "ForgeStatusFileDeleted"),
            Self::Modified => ("Modified", "ForgeStatusFileModified"),
        };
        let mut chunks = vec![
            TextChunk {
                text: label.into(),
                capture: capture.into(),
            },
            TextChunk {
                text: format!("{}{path}", " ".repeat(9 - label.len())),
                capture: "ForgeStatusPath".into(),
            },
        ];
        if let Some((added, removed)) = counts {
            for (text, capture) in [
                (" ".into(), "ForgeStatusPath"),
                (format!("+{added}"), "ForgeAddRange"),
                (" ".into(), "ForgeStatusPath"),
                (format!("-{removed}"), "ForgeDeleteRange"),
            ] {
                chunks.push(TextChunk {
                    text,
                    capture: capture.into(),
                });
            }
        } else if untracked {
            chunks.push(TextChunk {
                text: " new".into(),
                capture: "ForgeStatusPath".into(),
            });
        }
        chunks
    }
}

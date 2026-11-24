//! Whole-file progress shared by pull-request and plan review.

use std::collections::BTreeSet;

/// The two whole-file review sections, in presentation order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileReviewSection {
    Unviewed,
    Viewed,
}

impl FileReviewSection {
    /// Get the section name used in headings and pull-request block identities.
    pub fn name(self) -> &'static str {
        match self {
            Self::Unviewed => "Unviewed",
            Self::Viewed => "Viewed",
        }
    }

    /// Format a section heading with the number of files currently shown.
    pub fn heading(self, file_count: usize) -> String {
        format!("{} Changes ({file_count}):", self.name())
    }
}

/// Move one file into its requested section. Report whether membership changed.
pub fn set_viewed(files: &mut BTreeSet<String>, file: &str, viewed: bool) -> bool {
    if viewed {
        files.insert(file.to_owned())
    } else {
        files.remove(file)
    }
}

/// Partition file items in display order. Items without a file identity stay unviewed.
pub fn partition<T>(
    items: Vec<T>,
    viewed: &BTreeSet<String>,
    identity: impl Fn(&T) -> Option<&str>,
) -> [(FileReviewSection, Vec<T>); 2] {
    let (unviewed, viewed) = items
        .into_iter()
        .partition(|item| !identity(item).is_some_and(|file| viewed.contains(file)));
    [
        (FileReviewSection::Unviewed, unviewed),
        (FileReviewSection::Viewed, viewed),
    ]
}

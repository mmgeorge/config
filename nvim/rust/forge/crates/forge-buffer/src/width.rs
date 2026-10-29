use serde::{Deserialize, Serialize};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::ContractError;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AmbiguousWidth {
    #[default]
    Single,
    Double,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct CellWidthRange {
    pub first: u32,
    pub last: u32,
    pub cells: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct WidthProfile {
    pub columns: usize,
    pub tabstop: usize,
    #[serde(default)]
    pub variable_tabstop: Vec<usize>,
    #[serde(default)]
    pub ambiguous: AmbiguousWidth,
    #[serde(default)]
    pub cell_width: Vec<CellWidthRange>,
}

impl Default for WidthProfile {
    fn default() -> Self {
        Self {
            columns: 80,
            tabstop: 8,
            variable_tabstop: Vec::new(),
            ambiguous: AmbiguousWidth::Single,
            cell_width: Vec::new(),
        }
    }
}

impl WidthProfile {
    /// Fits a literal line to the viewport, marking omitted text with the supplied ellipsis.
    /// Grapheme clusters and tab widths follow this profile. The caller retains the full source.
    pub fn clip_plain(&self, text: &str, ellipsis: &str) -> Result<String, ContractError> {
        self.validate()?;
        if text.contains(['\r', '\n', '\0']) || ellipsis.contains(['\r', '\n', '\0']) {
            return Err(ContractError("clipped preview requires one text row"));
        }
        let mut suffix_end = 0;
        let mut suffix_cells = 0;
        for (offset, cluster) in ellipsis.grapheme_indices(true) {
            let cells = self.cells(cluster, suffix_cells)?;
            if suffix_cells + cells > self.columns {
                break;
            }
            suffix_cells += cells;
            suffix_end = offset + cluster.len();
        }
        let suffix = &ellipsis[..suffix_end];
        let mut column = 0;
        let mut suffix_offset = 0;
        for (offset, cluster) in text.grapheme_indices(true) {
            let cells = self.cells(cluster, column)?;
            if column + cells > self.columns {
                return Ok(if suffix_cells <= self.columns {
                    format!("{}{suffix}", text[..suffix_offset].trim_end())
                } else {
                    String::new()
                });
            }
            column += cells;
            if suffix_cells <= self.columns.saturating_sub(column) {
                suffix_offset = offset + cluster.len();
            }
        }
        Ok(text.to_owned())
    }

    /// Returns the retained byte count and display rows for a word-wrapped single-line preview.
    /// The limit bounds only the preview, leaving the complete source available to its owner.
    pub fn wrapped_prefix(
        &self,
        text: &str,
        rows: usize,
        continuation_indent: usize,
    ) -> Result<(usize, usize), ContractError> {
        self.validate()?;
        if text.contains(['\n', '\0']) {
            return Err(ContractError("wrapped preview requires one text row"));
        }
        if rows == 0 {
            return Ok((0, 0));
        }
        let indent = continuation_indent.min(self.columns.saturating_sub(1));
        let mut display_rows = 1;
        let mut column = 0;
        let mut offset = 0;
        for part in text.split_inclusive(char::is_whitespace) {
            let base = if display_rows == 1 { 0 } else { indent };
            if column > base && column + self.cells(part, column)? > self.columns {
                if display_rows == rows {
                    return Ok((offset, display_rows));
                }
                display_rows += 1;
                column = indent;
            }
            for cluster in part.graphemes(true) {
                let cells = self.cells(cluster, column)?;
                let base = if display_rows == 1 { 0 } else { indent };
                if column > base && column + cells > self.columns {
                    if display_rows == rows {
                        return Ok((offset, display_rows));
                    }
                    display_rows += 1;
                    column = indent;
                }
                column += self.cells(cluster, column)?;
                offset += cluster.len();
            }
        }
        Ok((offset, display_rows))
    }

    /// Wraps literal presentation text without interpreting Markdown punctuation.
    pub fn wrap_plain(
        &self,
        text: &str,
        continuation_indent_cells: usize,
    ) -> Result<Vec<String>, ContractError> {
        self.validate()?;
        if continuation_indent_cells >= self.columns || text.contains('\0') {
            return Err(ContractError("invalid plain text indentation or NUL"));
        }
        let indentation = " ".repeat(continuation_indent_cells);
        let mut rows = vec![String::new()];
        for (line, physical) in text.split('\n').enumerate() {
            if line > 0 {
                rows.push(indentation.clone());
            }
            for part in physical.split_inclusive(char::is_whitespace) {
                let current = rows.last().unwrap();
                let prefix = if rows.len() == 1 {
                    0
                } else {
                    indentation.len()
                };
                if current.len() > prefix
                    && self.cells(&format!("{current}{part}"), 0)? > self.columns
                {
                    rows.push(indentation.clone());
                }
                if self.cells(&format!("{}{part}", rows.last().unwrap()), 0)? <= self.columns {
                    rows.last_mut().unwrap().push_str(part);
                    continue;
                }
                for cluster in part.graphemes(true) {
                    let current = rows.last().unwrap();
                    let prefix = if rows.len() == 1 {
                        0
                    } else {
                        indentation.len()
                    };
                    if current.len() > prefix
                        && self.cells(&format!("{current}{cluster}"), 0)? > self.columns
                    {
                        rows.push(indentation.clone());
                    }
                    rows.last_mut().unwrap().push_str(cluster);
                }
            }
        }
        Ok(rows)
    }

    pub fn validate(&self) -> Result<(), ContractError> {
        if self.columns == 0 || self.tabstop == 0 || self.variable_tabstop.contains(&0) {
            return Err(ContractError("columns and tab stops must be positive"));
        }
        let mut previous = None;
        for range in &self.cell_width {
            if range.first > range.last
                || range.last > 0x10ffff
                || !(1..=2).contains(&range.cells)
                || previous.is_some_and(|last| range.first <= last)
            {
                return Err(ContractError(
                    "cell width ranges must be ordered and disjoint",
                ));
            }
            previous = Some(range.last);
        }
        Ok(())
    }

    pub fn cells(&self, text: &str, start_column: usize) -> Result<usize, ContractError> {
        self.validate()?;
        if text.contains('\n') {
            return Err(ContractError("display width requires one physical row"));
        }
        let mut column = start_column;
        let mut segment_start = 0;
        let plain = |value: &str, leading: bool| {
            let width = match self.ambiguous {
                AmbiguousWidth::Single => value.width(),
                AmbiguousWidth::Double => value.width_cjk(),
            };
            if !value.is_empty() && leading {
                width.max(1)
            } else {
                width
            }
        };
        for (offset, character) in text.char_indices() {
            let codepoint = u32::from(character);
            let index = self
                .cell_width
                .partition_point(|range| range.last < codepoint);
            let custom = self
                .cell_width
                .get(index)
                .filter(|range| range.first <= codepoint);
            if character == '\t' || character.is_control() || custom.is_some() {
                column = column
                    .checked_add(plain(&text[segment_start..offset], segment_start == 0))
                    .ok_or(ContractError("display width overflow"))?;
                let width = if character == '\t' {
                    self.tab_cells(column)
                } else if let Some(range) = custom {
                    range.cells
                } else if character as u32 <= 0x7f {
                    2
                } else {
                    4
                };
                column = column
                    .checked_add(width)
                    .ok_or(ContractError("display width overflow"))?;
                segment_start = offset + character.len_utf8();
            }
        }
        column = column
            .checked_add(plain(&text[segment_start..], segment_start == 0))
            .ok_or(ContractError("display width overflow"))?;
        Ok(column - start_column)
    }

    fn tab_cells(&self, column: usize) -> usize {
        let mut remaining = column;
        for width in &self.variable_tabstop {
            if *width > remaining {
                return *width - remaining;
            }
            remaining -= width;
        }
        let width = self
            .variable_tabstop
            .last()
            .copied()
            .unwrap_or(self.tabstop);
        width - remaining % width
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn width_profiles_preserve_large_layouts() {
        let source = "x".repeat(8193);
        for columns in [1, 80, 4097, 16384, usize::MAX] {
            let profile = super::WidthProfile {
                columns,
                ..Default::default()
            };
            assert_eq!(profile.cells(&source, 0).unwrap(), source.len());
            let rows = profile.wrap_plain(&source, 0).unwrap();
            assert_eq!(rows.concat(), source);
            assert!(rows.iter().all(|row| row.len() <= columns));
            assert_eq!(
                profile.wrapped_prefix(&source, rows.len(), 0).unwrap(),
                (source.len(), rows.len())
            );
        }

        let profile = super::WidthProfile {
            columns: 8192,
            tabstop: 1024,
            variable_tabstop: vec![512; 1024],
            cell_width: (0..1024)
                .map(|index| super::CellWidthRange {
                    first: 0x400 + index,
                    last: 0x400 + index,
                    cells: 2,
                })
                .collect(),
            ..Default::default()
        };
        let encoded = serde_json::to_vec(&profile).unwrap();
        let restored: super::WidthProfile = serde_json::from_slice(&encoded).unwrap();
        assert_eq!(restored, profile);
        assert_eq!(restored.cells("\t", 0).unwrap(), 512);
        assert_eq!(restored.cells("\t", 512 * 1023 + 1).unwrap(), 511);
        assert_eq!(restored.cells("\t", 512 * 1024 + 1).unwrap(), 511);
        let last = char::from_u32(0x7ff).unwrap().to_string();
        assert_eq!(restored.cells(&last, 0).unwrap(), 2);
        assert_eq!(restored.cells("abc", 0).unwrap(), 3);

        let mut profile = super::WidthProfile {
            columns: usize::MAX,
            tabstop: usize::MAX,
            ..Default::default()
        };
        assert_eq!(profile.cells("\t", 0).unwrap(), usize::MAX);
        assert_eq!(profile.cells("\t", usize::MAX - 1).unwrap(), 1);
        assert_eq!(profile.clip_plain("\t", "…").unwrap(), "\t");
        assert_eq!(profile.wrap_plain("\t", 0).unwrap(), vec!["\t"]);
        assert_eq!(profile.wrapped_prefix("\t", 1, 0).unwrap(), (1, 1));
        assert_eq!(
            profile.cells("\t\t", 0).unwrap_err().0,
            "display width overflow"
        );
        profile.variable_tabstop = vec![usize::MAX - 1, 2];
        assert_eq!(profile.cells("\t", 0).unwrap(), usize::MAX - 1);
        assert_eq!(
            profile.cells("\t", usize::MAX - 1).unwrap_err().0,
            "display width overflow"
        );
    }

    #[test]
    fn width_profiles_reject_invalid_geometry() {
        for profile in [
            super::WidthProfile {
                columns: 0,
                ..Default::default()
            },
            super::WidthProfile {
                tabstop: 0,
                ..Default::default()
            },
            super::WidthProfile {
                variable_tabstop: vec![1, 0],
                ..Default::default()
            },
        ] {
            assert_eq!(
                profile.validate().unwrap_err().0,
                "columns and tab stops must be positive"
            );
        }
        for ranges in [
            vec![(2, 1, 1)],
            vec![(0x110000, 0x110000, 1)],
            vec![(1, 1, 0)],
            vec![(1, 1, 3)],
            vec![(1, 2, 1), (2, 3, 2)],
            vec![(3, 4, 1), (1, 2, 2)],
        ] {
            let profile = super::WidthProfile {
                cell_width: ranges
                    .into_iter()
                    .map(|(first, last, cells)| super::CellWidthRange { first, last, cells })
                    .collect(),
                ..Default::default()
            };
            assert!(profile.validate().is_err());
        }
    }

    #[test]
    fn clipped_previews_preserve_graphemes_tabs_and_unbroken_prefixes() {
        let mut profile = super::WidthProfile {
            columns: 12,
            ..Default::default()
        };
        assert_eq!(
            profile.clip_plain("• abcdefghijklmnop", "…").unwrap(),
            "• abcdefghi…"
        );
        assert_eq!(profile.clip_plain("• short", "…").unwrap(), "• short");
        assert_eq!(profile.clip_plain("a\tb", "…").unwrap(), "a\tb");
        profile.columns = 5;
        assert_eq!(profile.clip_plain("• 🇺🇸🇯🇵", "…").unwrap(), "• 🇺🇸…");
        assert_eq!(profile.clip_plain("• éééé", "…").unwrap(), "• éé…");
        assert_eq!(profile.clip_plain("a\tb", "…").unwrap(), "a…");
        assert_eq!(profile.clip_plain("abcdef", "...").unwrap(), "ab...");
        profile.columns = 2;
        assert_eq!(profile.clip_plain("abcdef", "...").unwrap(), "..");
        profile.columns = 1;
        assert_eq!(profile.clip_plain("界", "…").unwrap(), "…");
        assert_eq!(profile.clip_plain("", "…").unwrap(), "");
        assert!(profile.clip_plain("first\nsecond", "…").is_err());
    }

    #[test]
    fn large_plain_text_preserves_rows_without_size_admission() {
        let line = "x".repeat(257);
        let source = format!("{line}\n").repeat(65_537);
        assert!(source.len() > 16 * 1024 * 1024);
        let profile = super::WidthProfile {
            columns: 512,
            ..Default::default()
        };
        let rows = profile.wrap_plain(&source, 2).unwrap();
        assert_eq!(rows.len(), 65_538);
        assert_eq!(rows[0], line);
        assert_eq!(rows[65_536], format!("  {line}"));
        assert_eq!(rows[65_537], "  ");
    }

    use super::*;

    #[test]
    fn plain_wrapping_preserves_punctuation_and_graphemes() {
        let profile = WidthProfile {
            columns: 4,
            ..WidthProfile::default()
        };
        assert_eq!(
            profile.wrap_plain("**🇺🇸🇯🇵**", 0).unwrap(),
            vec!["**🇺🇸", "🇯🇵**"]
        );
        assert_eq!(profile.wrap_plain("abcde", 2).unwrap(), vec!["abcd", "  e"]);
        assert_eq!(profile.wrap_plain("a\nb", 2).unwrap(), vec!["a", "  b"]);
    }

    #[test]
    fn measured_neovim_display_cells_cover_combining_emoji_and_tabs() {
        let profile = WidthProfile::default();
        for (text, expected) in [
            ("abc", 3),
            ("λ", 1),
            ("界", 2),
            ("é", 1),
            ("́", 1),
            ("👨‍👩‍👧‍👦", 2),
            ("🇺🇸", 2),
            ("👍🏽", 2),
            ("❤️", 2),
            ("☀", 1),
            ("☀️", 2),
            ("لا", 1),
            ("\t", 8),
            ("a\t", 8),
        ] {
            assert_eq!(profile.cells(text, 0).unwrap(), expected, "{text:?}");
        }
        assert_eq!(profile.cells("\t", 3).unwrap(), 5);
        let custom = WidthProfile {
            cell_width: vec![CellWidthRange {
                first: 0x3bb,
                last: 0x3bb,
                cells: 2,
            }],
            ..profile
        };
        assert_eq!(custom.cells("λ界", 0).unwrap(), 4);
    }
}

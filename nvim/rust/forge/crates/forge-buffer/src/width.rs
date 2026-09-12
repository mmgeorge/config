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
    /// Wraps literal presentation text without interpreting Markdown punctuation.
    pub fn wrap_plain(
        &self,
        text: &str,
        continuation_indent_cells: usize,
    ) -> Result<Vec<String>, ContractError> {
        self.validate()?;
        if continuation_indent_cells >= self.columns
            || text.len() > 8 * 1024 * 1024
            || text.contains('\0')
        {
            return Err(ContractError("plain text exceeds wrapping limits"));
        }
        let indentation = " ".repeat(continuation_indent_cells);
        let mut rows = vec![String::new()];
        let mut output_bytes = text.len();
        for (line, physical) in text.split('\n').enumerate() {
            if line > 0 {
                push_continuation(&mut rows, &indentation, &mut output_bytes)?;
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
                    push_continuation(&mut rows, &indentation, &mut output_bytes)?;
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
                        push_continuation(&mut rows, &indentation, &mut output_bytes)?;
                    }
                    rows.last_mut().unwrap().push_str(cluster);
                }
            }
        }
        Ok(rows)
    }

    pub fn validate(&self) -> Result<(), ContractError> {
        if !(1..=4096).contains(&self.columns)
            || !(1..=256).contains(&self.tabstop)
            || self.cell_width.len() > 256
            || self.variable_tabstop.len() > 256
            || self
                .variable_tabstop
                .iter()
                .any(|width| !(1..=256).contains(width))
        {
            return Err(ContractError("width profile exceeds layout limits"));
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
            let custom = self
                .cell_width
                .iter()
                .find(|range| (range.first..=range.last).contains(&(character as u32)));
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
        let mut stop = 0;
        for width in &self.variable_tabstop {
            stop += width;
            if stop > column {
                return stop - column;
            }
        }
        let width = self
            .variable_tabstop
            .last()
            .copied()
            .unwrap_or(self.tabstop);
        width - (column - stop) % width
    }
}

fn push_continuation(
    rows: &mut Vec<String>,
    indentation: &str,
    output_bytes: &mut usize,
) -> Result<(), ContractError> {
    if rows.len() >= 65_536 {
        return Err(ContractError("plain text row budget exceeded"));
    }
    *output_bytes += indentation.len();
    if *output_bytes > 16 * 1024 * 1024 {
        return Err(ContractError("plain text presentation budget exceeded"));
    }
    rows.push(indentation.to_owned());
    Ok(())
}

#[cfg(test)]
mod tests {
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

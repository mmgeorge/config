//! Write `<forge>` elements with one escaping rule, so no element formats markup by hand.
//!
//! Text is written unchanged except for markers that could open or close a Harness element:
//! `<forge`, and the opening or closing tag of any element that is currently open. Text is never
//! indented, so code, diffs, and command output arrive byte for byte.

use std::borrow::Cow;

use super::BLOCK_TAG;

/// Accumulate one rendered block while tracking which elements are open.
#[derive(Debug, Default)]
pub(crate) struct Writer {
    out: String,
    open: Vec<&'static str>,
}

impl Writer {
    /// Open an element with ordered attributes.
    pub(crate) fn open(&mut self, tag: &'static str, attributes: &[(&str, String)]) {
        self.out.push('<');
        self.out.push_str(tag);
        write_attributes(&mut self.out, attributes);
        self.out.push_str(">\n");
        self.open.push(tag);
    }

    /// Close the most recently opened element.
    pub(crate) fn close(&mut self) {
        let tag = self
            .open
            .pop()
            .expect("writer closes only elements it opened");
        self.out.push_str("</");
        self.out.push_str(tag);
        self.out.push_str(">\n");
    }

    /// Write an element without content.
    pub(crate) fn empty(&mut self, tag: &'static str, attributes: &[(&str, String)]) {
        self.out.push('<');
        self.out.push_str(tag);
        write_attributes(&mut self.out, attributes);
        self.out.push_str("/>\n");
    }

    /// Write text inside the current element, escaping only markers that could change structure.
    pub(crate) fn text(&mut self, text: &str) {
        let text = text.trim_end();
        if text.is_empty() {
            return;
        }
        let mut names = self.open.clone();
        names.push(BLOCK_TAG);
        self.out.push_str(&escape(text, &names));
        self.out.push('\n');
    }

    /// Write one element holding only text. Empty text writes nothing.
    pub(crate) fn element(&mut self, tag: &'static str, attributes: &[(&str, String)], text: &str) {
        if text.trim().is_empty() {
            return;
        }
        self.open(tag, attributes);
        self.text(text);
        self.close();
    }

    /// Report whether anything has been written.
    pub(crate) fn is_empty(&self) -> bool {
        self.out.is_empty()
    }

    /// Finish the content, without its trailing newline.
    pub(crate) fn finish(self) -> String {
        debug_assert!(self.open.is_empty(), "every opened element is closed");
        let mut out = self.out;
        out.truncate(out.trim_end().len());
        out
    }
}

/// Render attributes, escaping quotes and folding line breaks so a value stays on its tag line.
fn write_attributes(out: &mut String, attributes: &[(&str, String)]) {
    for (name, value) in attributes {
        out.push(' ');
        out.push_str(name);
        out.push_str("=\"");
        out.push_str(
            &value
                .replace('&', "&amp;")
                .replace('"', "&quot;")
                .replace(['\n', '\r'], " "),
        );
        out.push('"');
    }
}

/// Escape `<name` and `</name` for each structural `name`, leaving every other `<` intact.
///
/// A marker matches only the exact element name followed by `>`, `/`, whitespace, or the end of
/// the text, so tags such as `<forget>` or `<answers>` are left alone.
pub(crate) fn escape<'a>(text: &'a str, names: &[&str]) -> Cow<'a, str> {
    let is_marker = |rest: &str| {
        let rest = rest.strip_prefix('/').unwrap_or(rest);
        names.iter().any(|name| {
            rest.strip_prefix(name).is_some_and(|after| {
                after
                    .chars()
                    .next()
                    .is_none_or(|next| next == '>' || next == '/' || next.is_whitespace())
            })
        })
    };
    if !text
        .match_indices('<')
        .any(|(index, _)| is_marker(&text[index + 1..]))
    {
        return Cow::Borrowed(text);
    }
    let mut output = String::with_capacity(text.len() + 8);
    let mut rest = text;
    while let Some(index) = rest.find('<') {
        output.push_str(&rest[..index]);
        output.push_str(if is_marker(&rest[index + 1..]) {
            "&lt;"
        } else {
            "<"
        });
        rest = &rest[index + 1..];
    }
    output.push_str(rest);
    Cow::Owned(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Escape only structural markers and keep every other byte, including code with `<`.
    #[test]
    fn text_keeps_bytes_except_structural_markers() {
        let mut writer = Writer::default();
        writer.open("answer", &[]);
        writer.text("Vec<String> </answer> <forget> </forge>\n  indented\n");
        writer.close();
        assert_eq!(
            writer.finish(),
            "<answer>\nVec<String> &lt;/answer> <forget> &lt;/forge>\n  indented\n</answer>"
        );
    }

    /// Keep attribute values on one line and quote-safe.
    #[test]
    fn attributes_are_escaped_and_single_line() {
        let mut writer = Writer::default();
        writer.empty("rename", &[("from", "a\"b\nc".into())]);
        assert_eq!(writer.finish(), "<rename from=\"a&quot;b c\"/>");
    }
}

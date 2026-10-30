use crate::{TextError, contract::ByteRange};
use unicode_segmentation::UnicodeSegmentation;

/// Track a lexical word and its original UTF-8 offset.
pub(crate) struct Word<'text> {
    pub offset: usize,
    pub text: &'text str,
}

/// Extract Unicode words without changing any source bytes.
pub(crate) fn words(context: &str) -> Vec<Word<'_>> {
    context
        .unicode_word_indices()
        .map(|(offset, text)| Word { offset, text })
        .collect()
}

/// Restrict the English candidate model to its supported alphabet without transliteration.
pub(crate) fn supports_english_correction(word: &str) -> bool {
    word.bytes().any(|byte| byte.is_ascii_alphabetic())
        && word
            .bytes()
            .all(|byte| byte.is_ascii_alphabetic() || byte == b'\'')
}

/// Validate the target as one complete lexical word outside protected spans.
pub(crate) fn target<'text>(
    context: &'text str,
    range: &ByteRange,
    protected: &[ByteRange],
) -> Result<(Vec<Word<'text>>, usize), TextError> {
    range.validate(context)?;
    for region in protected {
        region.validate(context)?;
        if range.overlaps(region) {
            return Err(TextError::InvalidInput(
                "target intersects protected content".into(),
            ));
        }
    }
    let words = words(context);
    let position = words
        .iter()
        .position(|word| word.offset == range.start && word.offset + word.text.len() == range.end)
        .ok_or_else(|| TextError::InvalidInput("target is not a complete word".into()))?;
    let chunk_start = context[..range.start]
        .rfind(char::is_whitespace)
        .map_or(0, |offset| offset + 1);
    let chunk_end = context[range.end..]
        .find(char::is_whitespace)
        .map_or(context.len(), |offset| range.end + offset);
    let chunk = &context[chunk_start..chunk_end];
    let inner = chunk.trim_matches(|character: char| {
        !character.is_alphanumeric() && character != '/' && character != '\\' && character != '`'
    });
    if inner.contains('/')
        || inner.contains('\\')
        || inner.contains('_')
        || inner.contains('`')
        || inner.contains("::")
        || inner.contains('@')
        || inner.contains("://")
        || inner.split('.').filter(|part| !part.is_empty()).count() > 1
    {
        return Err(TextError::InvalidInput(
            "target belongs to a technical token".into(),
        ));
    }
    Ok((words, position))
}

/// Find a sentence's first content byte without treating abbreviations or decimals as stops.
pub(crate) fn sentence_start(context: &str, target: usize) -> usize {
    let mut start = context
        .split_sentence_bound_indices()
        .map(|(offset, _)| offset)
        .take_while(|offset| *offset <= target)
        .last()
        .unwrap_or(0);
    let prefix = context[..start].trim_end();
    if let Some(prefix) = prefix.strip_suffix('.') {
        let preceding = prefix
            .rsplit(|character: char| !character.is_alphabetic())
            .next()
            .unwrap_or("")
            .to_lowercase();
        if [
            "mr", "mrs", "ms", "dr", "prof", "sr", "jr", "vs", "etc", "e", "g", "i",
        ]
        .contains(&preceding.as_str())
        {
            start = 0;
        }
    }
    for (offset, character) in context[..target].char_indices() {
        if !matches!(character, '.' | '?' | '!') {
            continue;
        }
        if character == '.' {
            let prefix = &context[..offset];
            let preceding = prefix
                .rsplit(|character: char| !character.is_alphabetic())
                .next()
                .unwrap_or("")
                .to_lowercase();
            if [
                "mr", "mrs", "ms", "dr", "prof", "sr", "jr", "vs", "etc", "e", "g", "i",
            ]
            .contains(&preceding.as_str())
            {
                continue;
            }
            if context[offset + 1..]
                .chars()
                .next()
                .is_some_and(char::is_alphanumeric)
            {
                continue;
            }
        }
        start = start.max(offset + character.len_utf8());
    }
    start + context[start..].len() - context[start..].trim_start().len()
}

/// Transfer existing casing without adding a proper-name or acronym rule.
pub(crate) fn preserve_case(original: &str, candidate: &str) -> String {
    let original: Vec<_> = original.chars().collect();
    candidate
        .chars()
        .enumerate()
        .flat_map(|(position, character)| {
            let uppercase = original
                .iter()
                .filter(|character| character.is_alphabetic())
                .all(|character| character.is_uppercase())
                || original
                    .get(position)
                    .is_some_and(|character| character.is_uppercase());
            if uppercase {
                character.to_uppercase().collect::<Vec<_>>()
            } else {
                character.to_lowercase().collect()
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn sentence_boundaries_preserve_abbreviations_and_unicode() {
        for (text, word, expected) in [
            ("Hello。 world", "world", "world"),
            ("Visit Dr. Smith", "Smith", "Visit Dr. Smith"),
            ("Done. next", "next", "next"),
            ("Version 2.5 remains", "remains", "Version 2.5 remains"),
        ] {
            let position = text.find(word).unwrap();
            assert_eq!(&text[sentence_start(text, position)..], expected);
        }
    }
}

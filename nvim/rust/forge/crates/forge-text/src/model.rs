use crate::{
    TextError,
    contract::{CorrectRequest, CorrectionDecision, CorrectionIdentity, CorrectionReason},
    token,
};
use sha2::{Digest, Sha256};
use std::{
    collections::{HashMap, HashSet},
    io::{BufRead, Cursor},
    sync::atomic::{AtomicBool, Ordering},
};
use symspell_rs::{SymSpell, Verbosity};

const UNIGRAM: &[u8] = include_bytes!("../assets/frequency_dictionary_en_82_765.txt");
const BIGRAM: &[u8] = include_bytes!("../assets/frequency_bigramdictionary_en_243_342.txt");
const UNIGRAM_CHECKSUM: &str = "5eb7d9ca29ba3aee23d11b8b3d3aa048d7e94538b468eeffd9e07f76de7d5125";
const BIGRAM_CHECKSUM: &str = "c7775c2d4d25930146ae07de85c228330f9b8b6a81c3cc8f6fa52e10246e3de8";
const MODEL_REVISION: &str = "en-US-bigram-1";

/// Own the validated immutable lexical and contextual base shared by terminology profiles.
pub(crate) struct Model {
    dictionary: SymSpell,
    unigram: HashMap<String, f64>,
    bigram: HashMap<(String, String), f64>,
    predecessor: HashMap<String, f64>,
    total: f64,
    revision: String,
    candidates: crate::cache::Memo<String, Vec<symspell_rs::Suggestion>>,
    contexts: crate::cache::Memo<(String, String), f64>,
}
impl Model {
    /// Validate complete input streams before publishing any candidate dictionary.
    pub(crate) fn load(
        unigram: impl BufRead,
        bigram: impl BufRead,
        closed: &AtomicBool,
    ) -> Result<Self, TextError> {
        let mut dictionary = SymSpell::new(2, None, 7, 1);
        let mut frequencies = HashMap::new();
        let mut contexts = HashMap::new();
        let mut predecessor = HashMap::new();
        let mut digest = Sha256::new();
        let mut total = 0.0;
        for (ordinal, line) in unigram.lines().enumerate() {
            if closed.load(Ordering::Acquire) {
                return Err(TextError::Closed);
            }
            let line = line?;
            digest.update(line.as_bytes());
            digest.update(b"\n");
            let fields: Vec<_> = line.split_whitespace().collect();
            if fields.len() != 2 {
                return Err(TextError::Dictionary {
                    line: ordinal + 1,
                    message: "expected word and count".into(),
                });
            }
            let count: usize = fields[1].parse().map_err(|_| TextError::Dictionary {
                line: ordinal + 1,
                message: "invalid frequency".into(),
            })?;
            if count == 0 || fields[0].to_lowercase() != fields[0] {
                return Err(TextError::Dictionary {
                    line: ordinal + 1,
                    message: "word must be lowercase with a positive frequency".into(),
                });
            }
            dictionary.create_dictionary_entry(fields[0], count);
            *frequencies.entry(fields[0].to_owned()).or_insert(0.0) += count as f64;
            total += count as f64;
        }
        if frequencies.is_empty() {
            return Err(TextError::Dictionary {
                line: 0,
                message: "empty vocabulary".into(),
            });
        }
        for (ordinal, line) in bigram.lines().enumerate() {
            if closed.load(Ordering::Acquire) {
                return Err(TextError::Closed);
            }
            let line = line?;
            digest.update(line.as_bytes());
            digest.update(b"\n");
            let fields: Vec<_> = line.split_whitespace().collect();
            if fields.len() != 3 {
                return Err(TextError::Dictionary {
                    line: ordinal + 1,
                    message: "expected two words and count".into(),
                });
            }
            let count: usize = fields[2].parse().map_err(|_| TextError::Dictionary {
                line: ordinal + 1,
                message: "invalid bigram frequency".into(),
            })?;
            if count == 0 {
                return Err(TextError::Dictionary {
                    line: ordinal + 1,
                    message: "bigram frequency must be positive".into(),
                });
            }
            *contexts
                .entry((fields[0].to_owned(), fields[1].to_owned()))
                .or_insert(0.0) += count as f64;
            *predecessor.entry(fields[0].to_owned()).or_insert(0.0) += count as f64;
        }
        if contexts.is_empty() {
            return Err(TextError::Dictionary {
                line: 0,
                message: "empty contextual model".into(),
            });
        }
        let revision = format!("{:x}", digest.finalize());
        Ok(Self {
            dictionary,
            unigram: frequencies,
            bigram: contexts,
            predecessor,
            total,
            revision,
            candidates: crate::cache::Memo::new(256),
            contexts: crate::cache::Memo::new(1024),
        })
    }

    /// Load the reproducible offline assets bundled with the executable.
    pub(crate) fn bundled(closed: &AtomicBool) -> Result<Self, TextError> {
        if format!("{:x}", Sha256::digest(UNIGRAM)) != UNIGRAM_CHECKSUM
            || format!("{:x}", Sha256::digest(BIGRAM)) != BIGRAM_CHECKSUM
        {
            return Err(TextError::Checksum);
        }
        Self::load(Cursor::new(UNIGRAM), Cursor::new(BIGRAM), closed)
    }

    /// Get the content identity of both validated dictionaries.
    pub(crate) fn revision(&self) -> &str {
        &self.revision
    }

    /// Evaluate every candidate, including identity, and return only the target replacement.
    pub(crate) fn correct(
        &self,
        request: CorrectRequest,
        terminology: &HashSet<String>,
        overlay: Option<&SymSpell>,
        closed: &AtomicBool,
    ) -> Result<CorrectionDecision, TextError> {
        let (words, position) =
            token::target(&request.context, &request.target, &request.protected)?;
        let original = words[position].text;
        let normalized = original.to_lowercase();
        let mut candidates = self.candidates.get(&normalized).unwrap_or_else(|| {
            let candidates =
                self.dictionary
                    .lookup(&normalized, Verbosity::All, 2, &None, None, false);
            self.candidates
                .insert(normalized.clone(), candidates.clone());
            candidates
        });
        if let Some(overlay) = overlay {
            for candidate in overlay.lookup(&normalized, Verbosity::All, 2, &None, None, false) {
                if !candidates
                    .iter()
                    .any(|existing| existing.term == candidate.term)
                {
                    candidates.push(candidate);
                }
            }
        }
        if !candidates
            .iter()
            .any(|candidate| candidate.term == normalized)
        {
            candidates.push(symspell_rs::Suggestion {
                term: normalized.clone(),
                distance: 0,
                count: 0,
            });
        }
        let start = token::sentence_start(&request.context, request.target.start);
        let left = position
            .checked_sub(1)
            .filter(|previous| words[*previous].offset >= start)
            .map(|previous| words[previous].text.to_lowercase());
        let right = words
            .get(position + 1)
            .filter(|next| {
                !request.context[request.target.end..next.offset].contains(['.', '!', '?'])
            })
            .map(|next| next.text.to_lowercase());
        let score = |candidate: &symspell_rs::Suggestion| {
            if terminology.contains(&normalized) && candidate.term != normalized {
                return f64::NEG_INFINITY;
            }
            let lexical = if let Some(left) = &left {
                self.conditional(left, &candidate.term).ln()
            } else {
                0.3 * self.probability(&candidate.term).ln()
            };
            let lexical = lexical
                + right
                    .as_ref()
                    .map_or(0.0, |right| self.conditional(&candidate.term, right).ln());
            let prior = if candidate.term == normalized {
                0.0
            } else {
                -self.edit_prior(&normalized, &candidate.term, candidate.distance)
            };
            lexical + prior
        };
        let mut selected = &candidates[0];
        let mut selected_score = score(selected);
        for candidate in &candidates {
            if closed.load(Ordering::Acquire) {
                return Err(TextError::Closed);
            }
            let value = score(candidate);
            if value > selected_score || value == selected_score && candidate.term == normalized {
                selected = candidate;
                selected_score = value;
            }
        }
        let mut replacement = token::preserve_case(original, &selected.term);
        let mut reasons = Vec::new();
        if selected.term != normalized {
            reasons.push(if self.unigram.contains_key(&normalized) {
                CorrectionReason::ContextualChoice
            } else {
                CorrectionReason::Spelling
            });
        }
        if original == "i" {
            replacement = "I".into();
            reasons.push(CorrectionReason::StandaloneI);
        }
        if request.target.start == start
            && let Some(first) = replacement.chars().next()
            && first.is_alphabetic()
        {
            let capitalized = first
                .to_uppercase()
                .chain(replacement[first.len_utf8()..].chars())
                .collect::<String>();
            if capitalized != replacement {
                reasons.push(CorrectionReason::SentenceStart);
                replacement = capitalized;
            }
        }
        let identity = CorrectionIdentity {
            profile: request.profile,
            profile_revision: request.profile_revision,
            occurrence: request.occurrence,
            model_revision: MODEL_REVISION.into(),
            dictionary_revision: self.revision.clone(),
        };
        if replacement == original {
            return Ok(CorrectionDecision::Unchanged { identity });
        }
        Ok(CorrectionDecision::Replace {
            identity,
            original: original.into(),
            replacement,
            reasons,
        })
    }

    /// Smooth unseen words without excluding the user's original spelling.
    fn probability(&self, word: &str) -> f64 {
        (self.unigram.get(word).copied().unwrap_or(1.0) + 1.0) / self.total
    }

    /// Interpolate observed adjacency and unigram probability for sparse contexts.
    fn conditional(&self, left: &str, right: &str) -> f64 {
        let key = (left.to_owned(), right.to_owned());
        if let Some(probability) = self.contexts.get(&key) {
            return probability;
        }
        let count = self
            .bigram
            .get(&(left.to_owned(), right.to_owned()))
            .copied()
            .unwrap_or(0.0);
        let total = self.predecessor.get(left).copied().unwrap_or(0.0).max(1.0);
        let probability = 0.5 * count / total + 0.5 * self.probability(right);
        self.contexts.insert(key, probability);
        probability
    }

    /// Weight edit types independently from corpus frequency and retain valid-word identity.
    fn edit_prior(&self, original: &str, candidate: &str, distance: usize) -> f64 {
        let original_chars: Vec<_> = original.chars().collect();
        let candidate_chars: Vec<_> = candidate.chars().collect();
        let transposed = original_chars.len() == candidate_chars.len()
            && (0..original_chars.len().saturating_sub(1)).any(|position| {
                let mut changed = original_chars.clone();
                changed.swap(position, position + 1);
                changed == candidate_chars
            });
        let vowel_change = original_chars.len() == candidate_chars.len()
            && original_chars
                .iter()
                .zip(&candidate_chars)
                .filter(|(first, second)| first != second)
                .all(|(first, second)| "aeiouy".contains(*first) && "aeiouy".contains(*second));
        let channel = if transposed {
            0.5
        } else if original_chars.len() != candidate_chars.len() {
            2.0
        } else if vowel_change {
            1.2
        } else {
            6.8
        };
        (if self.unigram.contains_key(original) {
            2.5
        } else {
            0.0
        }) + channel
            + distance.saturating_sub(1) as f64 * 5.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::contract::{ByteRange, OccurrenceId, ProfileId};
    use std::sync::OnceLock;

    /// Reuse one immutable dictionary while preserving independent request inputs.
    fn model() -> &'static Model {
        static MODEL: OnceLock<Model> = OnceLock::new();
        MODEL.get_or_init(|| Model::bundled(&AtomicBool::new(false)).unwrap())
    }

    /// Evaluate the chosen occurrence without rewriting its surrounding sentence.
    fn decide(context: &str, target: &str) -> String {
        let start = token::words(context)
            .into_iter()
            .find(|word| word.text == target)
            .unwrap()
            .offset;
        let request = CorrectRequest {
            profile: ProfileId::new("test".into()).unwrap(),
            profile_revision: "revision".into(),
            occurrence: OccurrenceId::try_from("occurrence".to_owned()).unwrap(),
            context: context.into(),
            target: ByteRange {
                start,
                end: start + target.len(),
            },
            protected: Vec::new(),
        };
        match model()
            .correct(request, &HashSet::new(), None, &AtomicBool::new(false))
            .unwrap()
        {
            CorrectionDecision::Unchanged { .. } => target.into(),
            CorrectionDecision::Replace { replacement, .. } => replacement,
        }
    }

    #[test]
    fn known_word_candidates_include_neighbors() {
        let candidates = model()
            .dictionary
            .lookup("form", Verbosity::All, 2, &None, None, false);
        assert!(candidates.iter().any(|candidate| candidate.term == "form"));
        assert!(candidates.iter().any(|candidate| candidate.term == "from"));
    }

    #[test]
    fn contextual_quality_gate() {
        for (context, target, expected) in [
            ("Please recieve the package.", "recieve", "receive"),
            ("The update should releive pressure.", "releive", "relieve"),
            ("Please receive the package.", "receive", "receive"),
            ("The update should relieve pressure.", "relieve", "relieve"),
            ("Read the response form the server.", "form", "from"),
            ("Fill out the form before submitting.", "form", "form"),
            ("The value is greater then zero.", "then", "than"),
            ("Save the file then exit.", "then", "then"),
            ("Do not loose your work.", "loose", "lose"),
            ("The cable is loose.", "loose", "loose"),
            (
                "This should definately fix the issue.",
                "definately",
                "definitely",
            ),
            (
                "We need to seperate configuration from state.",
                "seperate",
                "separate",
            ),
            ("The problem occured during startup.", "occured", "occurred"),
            (
                "Check the dependancies before building.",
                "dependancies",
                "dependencies",
            ),
            ("The service recieved a request.", "recieved", "received"),
            ("Please recieve", "recieve", "receive"),
            ("The value is greater then", "then", "than"),
            ("Fill out the form", "form", "form"),
        ] {
            assert_eq!(decide(context, target), expected, "{context}");
        }
    }

    #[test]
    fn capitalization_has_only_two_rules() {
        assert_eq!(decide("  hello there.", "hello"), "Hello");
        assert_eq!(decide("Say i can help.", "i"), "I");
        assert_eq!(decide("\"hello there.\"", "hello"), "hello");
        assert_eq!(decide("42 hello there.", "hello"), "hello");
        assert_eq!(decide("(hello there.)", "hello"), "hello");
        assert_eq!(decide("Use google here.", "google"), "google");
        assert_eq!(decide("Visit Dr. smith.", "smith"), "smith");
        assert_eq!(
            decide(
                "The value is 2.5 and i agree.",
                "i agree".split_whitespace().next().unwrap()
            ),
            "I"
        );
        assert_eq!(decide("ß", "ß"), "SS");
    }

    #[test]
    fn invalid_dictionary_and_long_content() {
        let closed = AtomicBool::new(false);
        assert!(
            Model::load(
                Cursor::new(b"word invalid\n"),
                Cursor::new(b"word word 1\n"),
                &closed
            )
            .is_err()
        );
        assert!(
            Model::load(
                Cursor::new(b"word\n"),
                Cursor::new(b"word word 1\n"),
                &closed
            )
            .is_err()
        );
        let target = "z".repeat(100_000);
        let context = format!("Keep {target} unchanged.");
        assert_eq!(decide(&context, &target), target);
    }
}

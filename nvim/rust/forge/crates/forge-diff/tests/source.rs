use forge_diff::source::{
    MAX_SOURCE_BYTES, NewlineMetadata, Representation, SourceError, SourcePair, SourceVersion,
    validate_source_pair,
};

#[test]
fn exact_bytes_and_newline_metadata_survive_source_construction() {
    for (bytes, line_count, newline_count, crlf_count, has_final_newline) in [
        (&b""[..], 0, 0, 0, false),
        (&b"\n"[..], 1, 1, 0, true),
        (&b"a\r\nb\nlast\r"[..], 3, 2, 1, false),
        (&b"\r\n\r\n"[..], 2, 2, 2, true),
        ("é🙂\n終".as_bytes(), 2, 1, 0, false),
    ] {
        let source = SourceVersion::new(bytes.to_vec(), Representation::Raw).unwrap();
        assert_eq!(source.bytes(), bytes);
        assert_eq!(source.text().as_bytes(), bytes);
        assert_eq!(
            source.newline(),
            NewlineMetadata {
                line_count,
                newline_count,
                crlf_count,
                has_final_newline,
            }
        );
        let clone = source.clone();
        assert_eq!(source.bytes().as_ptr(), clone.bytes().as_ptr());
    }
}

#[test]
fn identical_bytes_in_distinct_representations_have_distinct_analysis_identities() {
    let raw = SourceVersion::new(b"same\n".to_vec(), Representation::Raw).unwrap();
    for representation in [Representation::GitCanonical, Representation::DisplayOnly] {
        let converted = SourceVersion::new(b"same\n".to_vec(), representation).unwrap();
        assert_eq!(
            raw.identity().content_hash,
            converted.identity().content_hash
        );
        assert_ne!(raw.identity(), converted.identity());
        validate_source_pair(&SourcePair {
            old: raw.clone(),
            new: converted,
        })
        .unwrap();
    }
}

#[test]
fn invalid_encoding_binary_and_size_are_distinct_admission_results() {
    assert_eq!(
        SourceVersion::new(vec![0xff], Representation::Raw).unwrap_err(),
        SourceError::UnsupportedEncoding
    );
    assert_eq!(
        SourceVersion::new(b"text\0data".to_vec(), Representation::Raw).unwrap_err(),
        SourceError::Binary
    );
    assert_eq!(
        SourceVersion::new(vec![0; MAX_SOURCE_BYTES + 1], Representation::Raw).unwrap_err(),
        SourceError::TooLarge {
            bytes: MAX_SOURCE_BYTES + 1,
            limit: MAX_SOURCE_BYTES,
        }
    );
    let accepted = SourceVersion::new(vec![b'x'; MAX_SOURCE_BYTES], Representation::Raw).unwrap();
    assert_eq!(accepted.bytes().len(), MAX_SOURCE_BYTES);
    assert_eq!(accepted.newline().line_count, 1);
}

#[test]
fn acquisition_claims_must_match_exact_content_and_newline_state() {
    let source = SourceVersion::new(b"a\r\n".to_vec(), Representation::GitCanonical).unwrap();
    SourceVersion::from_declared(source.bytes().to_vec(), source.identity(), source.newline())
        .unwrap();
    let mut wrong_identity = source.identity();
    wrong_identity.content_hash[0] ^= 1;
    assert_eq!(
        SourceVersion::from_declared(source.bytes().to_vec(), wrong_identity, source.newline())
            .unwrap_err(),
        SourceError::InconsistentIdentity
    );
    let mut wrong_newline = source.newline();
    wrong_newline.has_final_newline = false;
    assert_eq!(
        SourceVersion::from_declared(source.bytes().to_vec(), source.identity(), wrong_newline)
            .unwrap_err(),
        SourceError::InconsistentMetadata
    );
}

#[test]
fn spare_capacity_cannot_bypass_source_admission() {
    let mut content = Vec::with_capacity(MAX_SOURCE_BYTES + 1);
    content.push(b'x');
    let retained = content.capacity();
    assert_eq!(
        SourceVersion::new(content, Representation::Raw).unwrap_err(),
        SourceError::TooLarge {
            bytes: retained,
            limit: MAX_SOURCE_BYTES
        }
    );
}

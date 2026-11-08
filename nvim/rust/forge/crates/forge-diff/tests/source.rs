use forge_diff::source::{Representation, SourceError, SourcePair, SourceVersion};

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
        let comparison = forge_diff::raw::compute_hunks(SourcePair {
            old: raw.clone(),
            new: converted,
        });
        assert!(comparison.hunks().is_empty());
    }
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

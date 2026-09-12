use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

use anyhow::{Context, Result};
use serde::de::{IgnoredAny, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};

use crate::model::SnapshotState;
use crate::publication::MAX_CACHE_JSON_BYTES;

#[derive(Deserialize)]
struct SnapshotIdentity {
    repo: String,
    state: String,
    revision: Option<u64>,
    issue_count: usize,
    issues: IssueCount,
}

struct IssueCount(usize);

pub(crate) fn snapshot_matches(
    output: &Path,
    repo: &str,
    filter: SnapshotState,
    revision: u64,
) -> Result<bool> {
    let file = match File::open(output) {
        Ok(file) => file,
        Err(failure) if failure.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(failure) => return Err(failure).context("open completion snapshot for reconciliation"),
    };
    let mut reader =
        BufReader::with_capacity(64 * 1024, file.take((MAX_CACHE_JSON_BYTES + 1) as u64));
    let identity: SnapshotIdentity = match serde_json::from_reader(&mut reader) {
        Ok(identity) => identity,
        Err(failure) if failure.is_io() => {
            return Err(failure).context("read completion snapshot for reconciliation");
        }
        Err(_) => return Ok(false),
    };
    Ok(reader.get_ref().limit() > 0
        && identity.repo == repo
        && identity.revision == Some(revision)
        && identity.issue_count == identity.issues.0
        && identity.state
            == match filter {
                SnapshotState::Open => "open",
                SnapshotState::All => "all",
            })
}

impl<'input> Deserialize<'input> for IssueCount {
    fn deserialize<Decoder: Deserializer<'input>>(
        decoder: Decoder,
    ) -> Result<Self, Decoder::Error> {
        struct IssueCountVisitor;
        impl<'input> Visitor<'input> for IssueCountVisitor {
            type Value = IssueCount;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("an issue record array")
            }

            fn visit_seq<Sequence: SeqAccess<'input>>(
                self,
                mut sequence: Sequence,
            ) -> Result<Self::Value, Sequence::Error> {
                let mut count = 0;
                while sequence.next_element::<IgnoredAny>()?.is_some() {
                    count += 1;
                }
                Ok(IssueCount(count))
            }
        }
        decoder.deserialize_seq(IssueCountVisitor)
    }
}

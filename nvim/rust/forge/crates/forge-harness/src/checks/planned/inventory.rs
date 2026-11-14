//! Preserve one check's authoritative inventory and optional source-only supplementation.

use crate::checks::adapter::{DiscoverMode, Discovery, TestItemId};
use crate::checks::owner::CheckOwnerFailure;
use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

/// Retain a discovery result without assigning execution evidence to an unavailable inventory.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(super) enum InventoryResult {
    Available(Discovery),
    Failed { reason: String },
}

/// Distinguish a skipped supplementary request from an attempted request's diagnostic.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
enum SupplementaryResult {
    NotRequested,
    Available(Discovery),
    Failed { reason: String },
}

/// Keep source provenance distinct from the exact inventory that proves a case exists.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(super) enum SourceLocation {
    Exact { path: String, line: Option<u32> },
    Static { path: String, line: Option<u32> },
    Unverified,
}

impl SourceLocation {
    /// Get the normalized source path without discarding its provenance.
    pub(super) fn path(&self) -> Option<&str> {
        match self {
            Self::Exact { path, .. } | Self::Static { path, .. } => Some(path),
            Self::Unverified => None,
        }
    }
}

/// Bind both inventories to one configured check; only the primary inventory defines existence.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "RawInventory")]
pub(in crate::checks) struct PlannedInventory {
    check: String,
    discovery: InventoryResult,
    supplementary: SupplementaryResult,
}

impl PlannedInventory {
    /// Create one primary inventory from an owner-managed discovery operation.
    pub(in crate::checks) fn new(
        check: String,
        result: Result<Discovery, CheckOwnerFailure>,
    ) -> Result<Self> {
        RawInventory {
            check,
            discovery: match result {
                Ok(discovery) => InventoryResult::Available(discovery),
                Err(error) => InventoryResult::Failed {
                    reason: format!("{error:#}"),
                },
            },
            supplementary: SupplementaryResult::NotRequested,
        }
        .try_into()
    }

    /// Get the configured check qualifying every item in both inventories.
    pub(super) fn check(&self) -> &str {
        &self.check
    }

    /// Get the primary inventory without permitting replacement outside its constructor.
    pub(super) fn discovery(&self) -> &InventoryResult {
        &self.discovery
    }

    /// Get whether any authoritative case is missing a source path.
    pub(in crate::checks) fn needs_locations(&self) -> bool {
        match &self.discovery {
            InventoryResult::Available(discovery) => discovery
                .items()
                .iter()
                .any(|item| item.case && item.path.is_none()),
            InventoryResult::Failed { .. } => false,
        }
    }

    /// Retain supplementary locations or their diagnostic without replacing the exact inventory.
    pub(in crate::checks) fn supplement(&mut self, result: Result<Discovery, CheckOwnerFailure>) {
        self.supplementary = match result {
            Ok(discovery) => SupplementaryResult::Available(discovery),
            Err(error) => SupplementaryResult::Failed {
                reason: format!("{error:#}"),
            },
        };
    }

    /// Index locations by authoritative item identity without adding static-only cases.
    pub(super) fn locations(&self) -> std::collections::BTreeMap<TestItemId, SourceLocation> {
        let InventoryResult::Available(primary) = &self.discovery else {
            return Default::default();
        };
        let mut locations = primary
            .items()
            .iter()
            .map(|item| {
                (
                    item.id.clone(),
                    match &item.path {
                        Some(path) => match primary.mode() {
                            DiscoverMode::Exact => SourceLocation::Exact {
                                path: path.clone(),
                                line: item.line,
                            },
                            DiscoverMode::Static => SourceLocation::Static {
                                path: path.clone(),
                                line: item.line,
                            },
                        },
                        None => SourceLocation::Unverified,
                    },
                )
            })
            .collect::<std::collections::BTreeMap<_, _>>();
        if let SupplementaryResult::Available(discovery) = &self.supplementary {
            let cases = primary
                .items()
                .iter()
                .filter(|item| item.case)
                .map(|item| &item.id)
                .collect::<std::collections::BTreeSet<_>>();
            for source in discovery
                .items()
                .iter()
                .filter(|item| item.case && cases.contains(&item.id))
            {
                if let Some(path) = &source.path
                    && let Some(location @ SourceLocation::Unverified) =
                        locations.get_mut(&source.id)
                {
                    *location = SourceLocation::Static {
                        path: path.clone(),
                        line: source.line,
                    };
                }
            }
        }
        locations
    }

    /// Get a supplementary diagnostic without interpreting it as a resolution failure.
    pub(super) fn diagnostic(&self) -> Option<String> {
        match &self.supplementary {
            SupplementaryResult::Failed { reason } => Some(format!(
                "check {} supplementary static discovery: {reason}",
                self.check
            )),
            SupplementaryResult::NotRequested | SupplementaryResult::Available(_) => None,
        }
    }
}

/// Require every retained inventory to identify its check and preserve nonempty failure causes.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawInventory {
    check: String,
    discovery: InventoryResult,
    supplementary: SupplementaryResult,
}

impl TryFrom<RawInventory> for PlannedInventory {
    type Error = anyhow::Error;

    /// Reject missing check identities and unusable diagnostics before resolving restored entries.
    fn try_from(raw: RawInventory) -> Result<Self> {
        let RawInventory {
            check,
            discovery,
            supplementary,
        } = raw;
        ensure!(!check.trim().is_empty(), "planned inventory check is empty");
        if let InventoryResult::Failed { reason } = &discovery {
            ensure!(
                !reason.trim().is_empty(),
                "planned inventory failure has no cause"
            );
        }
        if let SupplementaryResult::Failed { reason } = &supplementary {
            ensure!(
                !reason.trim().is_empty(),
                "supplementary discovery failure has no cause"
            );
        }
        Ok(Self {
            check,
            discovery,
            supplementary,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checks::adapter::{DiscoverMode, TestItem};

    /// Construct a protocol item with independent identity, name, and source metadata.
    fn item(id: &str, name: &str, path: Option<&str>, line: Option<u32>) -> TestItem {
        TestItem {
            id: id.to_owned().try_into().unwrap(),
            parent: None,
            label: name.into(),
            case: true,
            name: Some(name.into()),
            path: path.map(str::to_owned),
            line,
        }
    }

    /// Join only equal case IDs within one check and preserve every exact location as returned.
    #[test]
    fn supplemental_locations_do_not_widen_or_replace_exact_inventory() {
        let primary = Discovery::new(
            vec![
                item("located", "works", Some("exact.rs"), None),
                item("missing", "works", None, None),
                item("same-name", "name_only", None, None),
                item("group-in-static", "case", None, None),
            ],
            DiscoverMode::Exact,
        )
        .unwrap();
        let mut inventory = PlannedInventory::new("first".into(), Ok(primary)).unwrap();
        assert!(inventory.needs_locations());
        let mut group = item("group-in-static", "group", Some("wrong.rs"), Some(1));
        group.case = false;
        let extra = Discovery::new(
            vec![
                item("located", "works", Some("wrong.rs"), Some(9)),
                item(
                    "missing",
                    "different_display_name",
                    Some("static.rs"),
                    Some(4),
                ),
                item("different-id", "name_only", Some("wrong.rs"), Some(1)),
                item("static-only", "extra", Some("extra.rs"), Some(1)),
                group,
            ],
            DiscoverMode::Static,
        )
        .unwrap();
        inventory.supplement(Ok(extra));
        let locations = inventory.locations();
        let id = |id: &str| TestItemId::try_from(id.to_owned()).unwrap();
        assert_eq!(locations.len(), 4);
        assert_eq!(
            locations[&id("located")],
            SourceLocation::Exact {
                path: "exact.rs".into(),
                line: None
            }
        );
        assert_eq!(
            locations[&id("missing")],
            SourceLocation::Static {
                path: "static.rs".into(),
                line: Some(4)
            }
        );
        assert_eq!(locations[&id("same-name")], SourceLocation::Unverified);
        assert_eq!(
            locations[&id("group-in-static")],
            SourceLocation::Unverified
        );
        let peer = PlannedInventory::new(
            "second".into(),
            Ok(Discovery::new(
                vec![item("missing", "works", None, None)],
                DiscoverMode::Exact,
            )
            .unwrap()),
        )
        .unwrap();
        assert_eq!(peer.locations()[&id("missing")], SourceLocation::Unverified);
        let restored: PlannedInventory =
            serde_json::from_slice(&serde_json::to_vec(&inventory).unwrap()).unwrap();
        assert_eq!(restored, inventory);
    }

    /// Failed supplementation stays diagnostic while exact discovery remains available.
    #[test]
    fn failed_supplementation_preserves_primary_cases_and_actual_cause() {
        let primary = Discovery::new(
            vec![item("missing", "works", None, None)],
            DiscoverMode::Exact,
        )
        .unwrap();
        let mut inventory = PlannedInventory::new("tests".into(), Ok(primary)).unwrap();
        inventory.supplement(Err(CheckOwnerFailure::Worker(
            anyhow::anyhow!("native static list unavailable").context("read source index"),
        )));
        let diagnostic = inventory.diagnostic().unwrap();
        assert!(diagnostic.contains("read source index"));
        assert!(diagnostic.contains("native static list unavailable"));
        assert_eq!(inventory.locations().len(), 1);
        let mut encoded = serde_json::to_value(&inventory).unwrap();
        encoded.as_object_mut().unwrap().remove("supplementary");
        assert!(serde_json::from_value::<PlannedInventory>(encoded).is_err());
        let mut encoded = serde_json::to_value(&inventory).unwrap();
        encoded["check"] = serde_json::json!("");
        assert!(serde_json::from_value::<PlannedInventory>(encoded).is_err());
        let mut encoded = serde_json::to_value(&inventory).unwrap();
        encoded["supplementary"]["Failed"]["reason"] = serde_json::json!("");
        assert!(serde_json::from_value::<PlannedInventory>(encoded).is_err());
    }
}

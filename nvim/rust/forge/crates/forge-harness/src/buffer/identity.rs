use std::collections::HashMap;
use std::fmt;

use crate::turn::ToolAddress;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
/// Identifies one rendered occurrence within its owning presentation lifetime.
pub struct DisplayId(usize);

impl serde::Serialize for DisplayId {
    fn serialize<Serializer: serde::Serializer>(
        &self,
        serializer: Serializer,
    ) -> Result<Serializer::Ok, Serializer::Error> {
        serializer.collect_str(self)
    }
}

impl DisplayId {
    pub(super) fn from_node(node: &str) -> Option<Self> {
        node.strip_prefix("display:")?
            .strip_suffix(":tool")?
            .parse()
            .ok()
            .map(Self)
    }

    #[cfg(test)]
    pub(crate) fn test() -> Self {
        Self(0)
    }
}

impl fmt::Display for DisplayId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "display:{}", self.0)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct DisplayKey {
    owner: String,
    source: DisplaySource,
    scope: Option<String>,
    occurrence: usize,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum DisplaySource {
    Tool(ToolAddress),
    Check { id: String, result: Option<usize> },
}

#[derive(Clone, Default)]
/// Retains numeric assignments until the presentation closes, including across reflow.
pub struct DisplayRegistry {
    id: HashMap<DisplayKey, DisplayId>,
}

pub(super) struct DisplayProjection<'registry> {
    registry: &'registry mut DisplayRegistry,
    owner: String,
    occurrence: HashMap<(ToolAddress, Option<String>), usize>,
}

impl DisplayRegistry {
    #[cfg(test)]
    pub(super) fn tool_occurrences<'registry>(
        &'registry self,
        source: &'registry str,
    ) -> impl Iterator<Item = DisplayId> + 'registry {
        self.id.iter().filter_map(move |(key, id)| {
            matches!(&key.source, DisplaySource::Tool(address) if address.legacy_key() == source)
                .then_some(*id)
        })
    }

    #[cfg(test)]
    pub(super) fn check_id(&self, command: &str, result: Option<usize>) -> Option<DisplayId> {
        self.id.iter().find_map(|(key, id)| {
            (key.source
                == DisplaySource::Check {
                    id: command.to_owned(),
                    result,
                })
            .then_some(*id)
        })
    }

    pub(super) fn project(&mut self, owner: String) -> DisplayProjection<'_> {
        DisplayProjection {
            registry: self,
            owner,
            occurrence: HashMap::new(),
        }
    }

    pub(super) fn tool(&self, owner: &str, source: &ToolAddress) -> Option<DisplayId> {
        self.id
            .get(&DisplayKey {
                owner: owner.to_owned(),
                source: DisplaySource::Tool(source.clone()),
                scope: None,
                occurrence: 0,
            })
            .copied()
    }

    pub(super) fn legacy_tool(&self, identity: &str) -> Option<DisplayId> {
        let mut found = self.id.iter().filter_map(|(key, id)| {
            matches!(&key.source, DisplaySource::Tool(source) if source.legacy_key() == identity)
                .then_some((key, *id))
        }).filter(|(key, _)| key.scope.is_none() && key.occurrence == 0);
        let (_, id) = found.next()?;
        found.all(|(_, other)| other == id).then_some(id)
    }
}

impl DisplayProjection<'_> {
    #[cfg(test)]
    pub(super) fn snapshot(&self) -> DisplayRegistry {
        self.registry.clone()
    }

    pub(super) fn tool(&mut self, source: ToolAddress, scope: Option<&str>) -> DisplayId {
        let scope = scope.map(str::to_owned);
        let occurrence = self
            .occurrence
            .entry((source.clone(), scope.clone()))
            .or_default();
        let key = DisplayKey {
            owner: self.owner.clone(),
            source: DisplaySource::Tool(source),
            scope,
            occurrence: *occurrence,
        };
        *occurrence += 1;
        let next = DisplayId(self.registry.id.len());
        *self.registry.id.entry(key).or_insert(next)
    }

    pub(super) fn check(&mut self, id: &str, result: Option<usize>) -> DisplayId {
        let key = DisplayKey {
            owner: self.owner.clone(),
            source: DisplaySource::Check {
                id: id.to_owned(),
                result,
            },
            scope: None,
            occurrence: 0,
        };
        let next = DisplayId(self.registry.id.len());
        *self.registry.id.entry(key).or_insert(next)
    }

    pub(super) fn legacy_tool(&self, identity: &str) -> Option<DisplayId> {
        self.registry.legacy_tool(identity)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identities_survive_reordering_and_distinguish_sources_scopes_and_occurrences() {
        let first = ToolAddress::new("a:b", "c");
        let second = ToolAddress::new("a", "b:c");
        let mut registry = DisplayRegistry::default();
        let mut projection = registry.project("entry".into());
        let root = projection.tool(first.clone(), None);
        let other = projection.tool(second.clone(), None);
        let scoped = projection.tool(first.clone(), Some("summary"));
        let repeated = projection.tool(first.clone(), None);
        assert_eq!(
            std::collections::HashSet::from([root, other, scoped, repeated]).len(),
            4
        );
        let mut projection = registry.project("entry".into());
        assert_eq!(projection.tool(second, None), other);
        assert_eq!(projection.tool(first.clone(), None), root);
        assert_eq!(projection.tool(first.clone(), Some("summary")), scoped);
        assert_eq!(projection.tool(first.clone(), None), repeated);
        assert_ne!(
            registry.project("other entry".into()).tool(first, None),
            root
        );
    }
}

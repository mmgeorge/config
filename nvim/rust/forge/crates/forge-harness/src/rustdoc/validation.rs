use super::{RustdocError, RustdocResolver};
use crate::plan::{ChangeAction, EntityReference, PlanDocument, PlanFlowRelation, PlanViolation};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RustApiValidationReport {
    pub warning: Vec<PlanViolation>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RustApiValidationError {
    pub violation: Vec<PlanViolation>,
}

impl std::fmt::Display for RustApiValidationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            formatter,
            "plan Rust API validation found {} violation(s)",
            self.violation.len()
        )?;
        for violation in &self.violation {
            writeln!(formatter, "- {}: {}", violation.path, violation.message)?;
        }
        Ok(())
    }
}

impl std::error::Error for RustApiValidationError {}

pub async fn validate_plan_rust_api(
    resolver: &RustdocResolver,
    document: &mut PlanDocument,
) -> Result<RustApiValidationReport, RustApiValidationError> {
    let mut violation = Vec::new();
    let mut warning = Vec::new();
    let mut package_version = HashMap::<String, String>::new();
    let mut unavailable_package = HashSet::<String>::new();

    for (dependency_index, dependency) in document.dependencies.iter_mut().enumerate() {
        if !is_cargo_manifest(&dependency.manifest) || dependency.action == ChangeAction::Remove {
            continue;
        }
        let path = format!("dependencies.{dependency_index}.version");
        let resolved = match dependency.resolved_version.as_deref() {
            Some(version) if requirement_matches(&dependency.version, version) => {
                Ok(version.to_owned())
            }
            _ => {
                resolver
                    .resolve_version(&dependency.name, &dependency.version)
                    .await
            }
        };
        match resolved {
            Ok(version) => {
                dependency.resolved_version = Some(version.clone());
                package_version.insert(dependency.name.clone(), version);
            }
            Err(RustdocError::Unavailable(message)) => {
                warning.push(PlanViolation {
                    path,
                    message: partial_validation_warning(
                        &message,
                        &format!("dependency `{}`", dependency.name),
                    ),
                });
                unavailable_package.insert(dependency.name.clone());
            }
            Err(error) => violation.push(PlanViolation {
                path,
                message: error.to_string(),
            }),
        }
    }

    let package_list = package_version
        .iter()
        .map(|(package, version)| (package.clone(), version.clone()))
        .collect::<Vec<_>>();
    let mut edge_list = Vec::new();
    for (flow_index, flow) in document.flows.iter().enumerate() {
        collect_flow_edge(
            &flow.edges,
            &format!("flows.{flow_index}.edges"),
            &mut edge_list,
        );
    }
    for (edge, path) in edge_list {
        let callable = match edge.relation {
            PlanFlowRelation::Call | PlanFlowRelation::Read | PlanFlowRelation::Write => {
                edge.callable.as_ref().expect("validated callable")
            }
            _ => continue,
        };
        let EntityReference::ExternalEntity {
            name: receiver,
            dependency: Some(dependency_name),
            ..
        } = &edge.target
        else {
            continue;
        };
        let dependency = document
            .dependencies
            .iter()
            .find(|dependency| dependency.name == *dependency_name);
        let Some(dependency) = dependency else {
            violation.push(PlanViolation {
                path,
                message: format!(
                    "receiver dependency `{dependency_name}` is not declared by this plan"
                ),
            });
            continue;
        };
        if dependency.action == ChangeAction::Remove {
            violation.push(PlanViolation {
                path,
                message: format!(
                    "receiver dependency `{dependency_name}` is scheduled for removal"
                ),
            });
            continue;
        }
        if unavailable_package.contains(dependency_name) {
            warning.push(PlanViolation {
                path,
                message: format!(
                    "could not validate `{}::{}` because `{dependency_name}` was unavailable; Rust API validation was partially skipped for this callable",
                    receiver, callable.name
                ),
            });
            continue;
        }
        let Some(version) = package_version.get(dependency_name) else {
            continue;
        };
        match resolver
            .type_hover(dependency_name, version, receiver)
            .await
        {
            Ok(_) => {}
            Err(RustdocError::Unavailable(message)) => {
                warning.push(PlanViolation {
                    path: path.clone(),
                    message: partial_validation_warning(&message, &format!("type `{receiver}`")),
                });
                continue;
            }
            Err(error) => {
                violation.push(PlanViolation {
                    path: path.clone(),
                    message: error.to_string(),
                });
                continue;
            }
        }
        match resolver
            .callable_hover(
                &package_list,
                dependency_name,
                version,
                receiver,
                &callable.name,
                callable.kind,
            )
            .await
        {
            Ok(_) => {}
            Err(RustdocError::Unavailable(message)) => warning.push(PlanViolation {
                path,
                message: partial_validation_warning(
                    &message,
                    &format!("callable `{}::{}`", receiver, callable.name),
                ),
            }),
            Err(error) => violation.push(PlanViolation {
                path,
                message: error.to_string(),
            }),
        }
    }

    if violation.is_empty() {
        Ok(RustApiValidationReport { warning })
    } else {
        Err(RustApiValidationError { violation })
    }
}

fn collect_flow_edge<'a>(
    edge_slice: &'a [crate::plan::PlanFlowEdge],
    parent_path: &str,
    edge_list: &mut Vec<(&'a crate::plan::PlanFlowEdge, String)>,
) {
    for (edge_index, edge) in edge_slice.iter().enumerate() {
        let edge_path = format!("{parent_path}.{edge_index}");
        edge_list.push((edge, format!("{edge_path}.callable")));
        collect_flow_edge(
            &edge.expansion,
            &format!("{edge_path}.expansion"),
            edge_list,
        );
        for (branch_index, branch) in edge.branches.iter().enumerate() {
            collect_flow_edge(
                &branch.edges,
                &format!("{edge_path}.branches.{branch_index}.edges"),
                edge_list,
            );
        }
    }
}

fn partial_validation_warning(network_error: &str, skipped_scope: &str) -> String {
    format!("{network_error}; Rust API validation was partially skipped for {skipped_scope}")
}

fn is_cargo_manifest(path: &str) -> bool {
    std::path::Path::new(path)
        .file_name()
        .is_some_and(|name| name == "Cargo.toml")
}

fn requirement_matches(requirement: &str, version: &str) -> bool {
    semver::VersionReq::parse(requirement)
        .ok()
        .zip(semver::Version::parse(version).ok())
        .is_some_and(|(requirement, version)| requirement.matches(&version))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::plan::{PlanFlowBranch, PlanFlowEdge, ReferencedEntityKind};

    fn endpoint(name: &str) -> EntityReference {
        EntityReference::ExternalEntity {
            entity_kind: ReferencedEntityKind::Endpoint,
            name: name.into(),
            dependency: None,
        }
    }

    fn emitting_edge() -> PlanFlowEdge {
        PlanFlowEdge {
            relation: PlanFlowRelation::Emit,
            target: endpoint("terminal"),
            callable: None,
            payload_type: Some("Output".into()),
            return_type: None,
            expansion: Vec::new(),
            branches: Vec::new(),
        }
    }

    #[test]
    fn collects_rust_api_edges_from_expansions_and_branches() {
        let edge_list = vec![PlanFlowEdge {
            relation: PlanFlowRelation::Emit,
            target: endpoint("terminal"),
            callable: None,
            payload_type: Some("Output".into()),
            return_type: None,
            expansion: vec![emitting_edge()],
            branches: vec![PlanFlowBranch {
                condition: "failure".into(),
                edges: vec![emitting_edge()],
            }],
        }];
        let mut collected_edge_list = Vec::new();

        collect_flow_edge(&edge_list, "flows.0.edges", &mut collected_edge_list);

        assert_eq!(
            collected_edge_list
                .into_iter()
                .map(|(_, path)| path)
                .collect::<Vec<_>>(),
            vec![
                "flows.0.edges.0.callable",
                "flows.0.edges.0.expansion.0.callable",
                "flows.0.edges.0.branches.0.edges.0.callable",
            ]
        );
    }
}

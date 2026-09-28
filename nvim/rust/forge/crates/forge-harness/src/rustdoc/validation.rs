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
            .declaration(
                dependency_name,
                version,
                receiver,
                Some((&callable.name, callable.kind)),
            )
            .await
        {
            Ok(declaration) => check_signature(
                edge,
                receiver,
                &declaration,
                &path,
                &mut violation,
                &mut warning,
            ),
            Err(RustdocError::Unavailable(message) | RustdocError::Ambiguous(message)) => warning
                .push(PlanViolation {
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

fn check_signature(
    edge: &crate::plan::PlanFlowEdge,
    receiver: &str,
    declaration: &super::index::SourceItem,
    path: &str,
    violation: &mut Vec<PlanViolation>,
    warning: &mut Vec<PlanViolation>,
) {
    let mut comparison = Vec::new();
    if let (Some(payload), Some(input)) = (&edge.payload_type, &declaration.input) {
        let actual = match input.as_slice() {
            [input] => input.clone(),
            input => format!("({})", input.join(", ")),
        };
        comparison.push(("payload_type", payload.clone(), actual));
    }
    if let (Some(output), Some(actual)) = (&edge.return_type, &declaration.output) {
        let expected = match &output.error_type {
            Some(error) => format!("Result<{}, {error}>", output.value_type),
            None => output.value_type.clone(),
        };
        comparison.push(("return_type", expected, actual.clone()));
    }
    for (field, expected, actual) in comparison {
        let field_path = format!(
            "{}.{}",
            path.strip_suffix(".callable").unwrap_or(path),
            field
        );
        match compare_type(&expected, &actual, receiver, &declaration.generic) {
            Ok(true) => {}
            Ok(false) => violation.push(PlanViolation {
                path: field_path,
                message: format!(
                    "declared `{expected}` but source signature `{}` declares `{actual}`",
                    declaration.signature
                ),
            }),
            Err(message) => warning.push(PlanViolation {
                path: field_path,
                message: format!("signature check skipped: {message}; source declares `{actual}`"),
            }),
        }
    }
}

fn compare_type(
    expected: &str,
    actual: &str,
    receiver: &str,
    generic: &[String],
) -> Result<bool, String> {
    use quote::ToTokens;
    use syn::visit_mut::VisitMut;
    struct Normalize<'a> {
        receiver: &'a syn::Type,
        generic: &'a [String],
        uncertain: bool,
    }
    impl VisitMut for Normalize<'_> {
        fn visit_type_mut(&mut self, value: &mut syn::Type) {
            if matches!(
                value,
                syn::Type::ImplTrait(_) | syn::Type::Infer(_) | syn::Type::Macro(_)
            ) {
                self.uncertain = true;
            }
            if let syn::Type::Path(path) = value {
                if path.path.is_ident("Self") {
                    *value = self.receiver.clone();
                } else if path
                    .path
                    .segments
                    .iter()
                    .any(|segment| self.generic.iter().any(|name| segment.ident == name))
                {
                    self.uncertain = true;
                }
            }
            syn::visit_mut::visit_type_mut(self, value);
        }
    }
    let receiver = syn::parse_str::<syn::Type>(receiver).map_err(|error| error.to_string())?;
    let mut expected = syn::parse_str::<syn::Type>(expected)
        .map_err(|error| format!("invalid stated type: {error}"))?;
    let mut actual = syn::parse_str::<syn::Type>(actual)
        .map_err(|error| format!("unsupported source type: {error}"))?;
    let mut normalizer = Normalize {
        receiver: &receiver,
        generic,
        uncertain: false,
    };
    normalizer.visit_type_mut(&mut expected);
    normalizer.visit_type_mut(&mut actual);
    let equal = expected.to_token_stream().to_string() == actual.to_token_stream().to_string();
    if !equal && normalizer.uncertain {
        return Err("generic substitution requires inference beyond declaration checking".into());
    }
    Ok(equal)
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

#[cfg(test)]
mod signature_test {
    use super::*;

    #[test]
    fn checks_concrete_inputs_outputs_and_preserves_declared_generics() {
        assert_eq!(compare_type("f32", "f32", "Clock", &[]), Ok(true));
        assert_eq!(compare_type("f64", "f32", "Clock", &[]), Ok(false));
        assert_eq!(
            compare_type("(Color, Vec2)", "(Color,Vec2)", "Sprite", &[]),
            Ok(true)
        );
        assert_eq!(compare_type("&mut App", "&mut Self", "App", &[]), Ok(true));
        assert_eq!(
            compare_type("Result<(), Error>", "Result<(), Error>", "App", &[]),
            Ok(true)
        );
        assert_eq!(compare_type("T", "T", "Clock", &["T".into()]), Ok(true));
        assert!(compare_type("f32", "T", "Clock", &["T".into()]).is_err());
    }

    #[test]
    fn reports_type_mismatches_on_the_plan_fields() {
        let (_directory, mut index) = super::super::index::test::fixture();
        let declaration = index
            .callable(
                "facade",
                "1.0.0",
                "Clock",
                "reset",
                crate::plan::PlanCallableKind::Method,
            )
            .unwrap();
        let edge: crate::plan::PlanFlowEdge = serde_json::from_value(serde_json::json!({
            "relation": "call", "target": {"kind": "external_entity", "entity_kind": "type", "name": "Clock", "dependency": "facade"},
            "callable": {"kind": "method", "name": "reset"}, "payload_type": "f64",
            "return_type": {"value_type": "bool", "error_type": "std::io::Error"}, "expansion": [], "branches": []
        })).unwrap();
        let mut violation = Vec::new();
        let mut warning = Vec::new();
        check_signature(
            &edge,
            "Clock",
            &declaration,
            "flows.0.edges.0.callable",
            &mut violation,
            &mut warning,
        );
        assert_eq!(violation.len(), 2);
        assert_eq!(violation[0].path, "flows.0.edges.0.payload_type");
        assert_eq!(violation[1].path, "flows.0.edges.0.return_type");
        assert!(warning.is_empty());
    }
}

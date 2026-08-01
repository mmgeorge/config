use std::collections::{HashMap, HashSet};

use super::{
    ChangeAction, EntityKind, EnumVariantChange, EnumVariantFieldChange, PlanDependencyChange,
    PlanDocument, ProgramEntityChange, ProgramEntityMemberChange,
};

/// Represents the entity, member, variant field, or dependency behind one semantic name.
#[derive(Clone, Copy, Debug)]
pub enum ResolvedPlanEntity<'a> {
    Entity(&'a ProgramEntityChange),
    Member(&'a ProgramEntityChange, &'a ProgramEntityMemberChange),
    Variant(&'a ProgramEntityChange, &'a EnumVariantChange),
    VariantField(
        &'a ProgramEntityChange,
        &'a EnumVariantChange,
        &'a EnumVariantFieldChange,
    ),
    Dependency(&'a PlanDependencyChange),
}

/// Locates one entity within the derived reviewer presentation forest.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PresentationPosition {
    path: Vec<usize>,
    preorder_rank: usize,
}

impl PresentationPosition {
    /// Return the one-based hierarchy path, such as `1.2.1`.
    pub fn path(&self) -> &[usize] {
        &self.path
    }

    /// Return the entity depth derived from its hierarchy path.
    pub fn depth(&self) -> usize {
        self.path.len().saturating_sub(1)
    }

    /// Return the zero-based parent-first traversal rank.
    pub fn preorder_rank(&self) -> usize {
        self.preorder_rank
    }
}

/// Indexes semantic plan entities for validation, rendering, review, and execution.
pub struct PlanGraph<'a> {
    entity_by_name: HashMap<&'a str, &'a ProgramEntityChange>,
    resolved_by_name: HashMap<String, ResolvedPlanEntity<'a>>,
    presentation_parent_by_entity_name: HashMap<&'a str, &'a str>,
    presentation_child_index_by_entity_name: HashMap<&'a str, Vec<usize>>,
    presentation_position_by_entity_name: HashMap<&'a str, PresentationPosition>,
}

impl<'a> PlanGraph<'a> {
    /// Build one semantic index from the canonical document.
    pub fn new(document: &'a PlanDocument) -> Self {
        let mut resolved_by_name = HashMap::new();
        for entity in &document.entity_changes {
            resolved_by_name.insert(entity.name.clone(), ResolvedPlanEntity::Entity(entity));
            for member in &entity.members {
                resolved_by_name.insert(
                    format!("{}::{}", entity.name, member.name),
                    ResolvedPlanEntity::Member(entity, member),
                );
            }
            for variant in &entity.variants {
                resolved_by_name.insert(
                    format!("{}::{}", entity.name, variant.name),
                    ResolvedPlanEntity::Variant(entity, variant),
                );
                for field in &variant.fields {
                    resolved_by_name.insert(
                        format!("{}::{}::{}", entity.name, variant.name, field.name),
                        ResolvedPlanEntity::VariantField(entity, variant, field),
                    );
                }
            }
        }
        for dependency in &document.dependencies {
            resolved_by_name.insert(
                dependency.name.clone(),
                ResolvedPlanEntity::Dependency(dependency),
            );
        }
        let entity_by_name = document
            .entity_changes
            .iter()
            .map(|entity| (entity.name.as_str(), entity))
            .collect();
        let presentation_hierarchy = build_presentation_hierarchy(document);
        Self {
            entity_by_name,
            resolved_by_name,
            presentation_parent_by_entity_name: presentation_hierarchy.parent_by_entity_name,
            presentation_child_index_by_entity_name: presentation_hierarchy
                .child_index_by_entity_name,
            presentation_position_by_entity_name: presentation_hierarchy.position_by_entity_name,
        }
    }

    /// Resolve one program entity from its unique semantic name.
    pub fn entity(&self, entity_name: &str) -> Option<&'a ProgramEntityChange> {
        self.entity_by_name.get(entity_name).copied()
    }

    /// Resolve one exact semantic entity from its qualified semantic name.
    pub fn resolve_entity(&self, entity_name: &str) -> Option<ResolvedPlanEntity<'a>> {
        self.resolved_by_name.get(entity_name).copied()
    }

    /// Resolve the repository path owned by one semantic entity.
    pub fn entity_path(&self, entity_name: &str) -> Option<&'a str> {
        match self.resolve_entity(entity_name)? {
            ResolvedPlanEntity::Entity(entity)
            | ResolvedPlanEntity::Member(entity, _)
            | ResolvedPlanEntity::Variant(entity, _)
            | ResolvedPlanEntity::VariantField(entity, _, _) => Some(&entity.path),
            ResolvedPlanEntity::Dependency(dependency) => Some(&dependency.manifest),
        }
    }

    /// Render one concise reviewer label for a semantic entity.
    pub fn entity_label(&self, entity_name: &str) -> Option<String> {
        match self.resolve_entity(entity_name)? {
            ResolvedPlanEntity::Entity(entity) => Some(entity.name.clone()),
            ResolvedPlanEntity::Member(entity, member) => {
                Some(format!("{}::{}", entity.name, member.name))
            }
            ResolvedPlanEntity::Variant(entity, variant) => {
                Some(format!("{}::{}", entity.name, variant.name))
            }
            ResolvedPlanEntity::VariantField(entity, variant, field) => {
                Some(format!("{}::{}::{}", entity.name, variant.name, field.name))
            }
            ResolvedPlanEntity::Dependency(dependency) => Some(dependency.name.clone()),
        }
    }

    /// Resolve the lifecycle action attached to one semantic entity.
    pub fn entity_action(&self, entity_name: &str) -> Option<ChangeAction> {
        match self.resolve_entity(entity_name)? {
            ResolvedPlanEntity::Entity(entity) => Some(entity.action.base_action()),
            ResolvedPlanEntity::Member(_, member) => Some(member.action),
            ResolvedPlanEntity::Variant(_, variant) => Some(variant.action),
            ResolvedPlanEntity::VariantField(_, _, field) => Some(field.action),
            ResolvedPlanEntity::Dependency(dependency) => Some(dependency.action),
        }
    }

    /// Resolve the sole concrete user that visually owns one concrete entity.
    pub fn presentation_parent(&self, entity_name: &str) -> Option<&'a str> {
        self.presentation_parent_by_entity_name
            .get(entity_name)
            .copied()
    }

    /// Iterate concrete entities visually nested beneath one concrete user.
    pub fn presentation_child_indices(
        &self,
        entity_name: &str,
    ) -> impl Iterator<Item = usize> + '_ {
        self.presentation_child_index_by_entity_name
            .get(entity_name)
            .into_iter()
            .flatten()
            .copied()
    }

    /// Resolve one entity's shared UML and task-list presentation position.
    pub fn presentation_position(&self, entity_name: &str) -> Option<&PresentationPosition> {
        self.entity(entity_name)?;
        self.presentation_position_by_entity_name.get(entity_name)
    }

    /// Resolve known entity IDs and order them by the shared presentation traversal.
    pub fn entities_in_presentation_order(
        &self,
        entity_name_list: &[String],
    ) -> Vec<&'a ProgramEntityChange> {
        let mut entity_list = entity_name_list
            .iter()
            .enumerate()
            .filter_map(|(original_index, entity_name)| {
                self.entity(entity_name)
                    .map(|entity| (original_index, entity))
            })
            .collect::<Vec<_>>();
        entity_list.sort_by_key(|(original_index, entity)| {
            (
                self.presentation_position(&entity.name)
                    .map(PresentationPosition::preorder_rank)
                    .unwrap_or(usize::MAX),
                *original_index,
            )
        });
        entity_list.into_iter().map(|(_, entity)| entity).collect()
    }
}

struct PresentationHierarchy<'a> {
    parent_by_entity_name: HashMap<&'a str, &'a str>,
    child_index_by_entity_name: HashMap<&'a str, Vec<usize>>,
    position_by_entity_name: HashMap<&'a str, PresentationPosition>,
}

fn build_presentation_hierarchy<'a>(document: &'a PlanDocument) -> PresentationHierarchy<'a> {
    let entity_by_name = document
        .entity_changes
        .iter()
        .map(|entity| (entity.name.as_str(), entity))
        .collect::<HashMap<_, _>>();
    let mut user_name_set_by_entity_name = HashMap::<&str, HashSet<&str>>::new();
    for user in document
        .entity_changes
        .iter()
        .filter(|entity| !is_contract_kind(entity.kind))
    {
        for type_expression in entity_type_expression_list(user) {
            for identifier in type_identifiers(type_expression) {
                let Some(target) = entity_by_name.get(identifier) else {
                    continue;
                };
                if target.name == user.name || is_contract_kind(target.kind) {
                    continue;
                }
                user_name_set_by_entity_name
                    .entry(target.name.as_str())
                    .or_default()
                    .insert(user.name.as_str());
            }
        }
    }

    let mut parent_by_entity_name = user_name_set_by_entity_name
        .into_iter()
        .filter_map(|(entity_name, user_name_set)| {
            (user_name_set.len() == 1).then(|| (entity_name, *user_name_set.iter().next().unwrap()))
        })
        .collect::<HashMap<_, _>>();
    let cyclic_entity_name_set = cyclic_entity_names(&parent_by_entity_name);
    parent_by_entity_name.retain(|entity_name, _| !cyclic_entity_name_set.contains(entity_name));

    let mut child_index_by_entity_name = HashMap::<&str, Vec<usize>>::new();
    for (entity_index, entity) in document.entity_changes.iter().enumerate() {
        if let Some(parent_name) = parent_by_entity_name.get(entity.name.as_str()) {
            child_index_by_entity_name
                .entry(*parent_name)
                .or_default()
                .push(entity_index);
        }
    }
    let position_by_entity_name = build_presentation_position_index(
        document,
        &parent_by_entity_name,
        &child_index_by_entity_name,
    );
    PresentationHierarchy {
        parent_by_entity_name,
        child_index_by_entity_name,
        position_by_entity_name,
    }
}

fn build_presentation_position_index<'a>(
    document: &'a PlanDocument,
    parent_by_entity_name: &HashMap<&'a str, &'a str>,
    child_index_by_entity_name: &HashMap<&'a str, Vec<usize>>,
) -> HashMap<&'a str, PresentationPosition> {
    let mut position_by_entity_name = HashMap::new();
    let mut preorder_rank = 0;
    let mut root_ordinal = 0;
    for (entity_index, entity) in document.entity_changes.iter().enumerate() {
        if parent_by_entity_name.contains_key(entity.name.as_str()) {
            continue;
        }
        root_ordinal += 1;
        index_presentation_subtree(
            document,
            child_index_by_entity_name,
            entity_index,
            vec![root_ordinal],
            &mut preorder_rank,
            &mut position_by_entity_name,
        );
    }
    position_by_entity_name
}

fn index_presentation_subtree<'a>(
    document: &'a PlanDocument,
    child_index_by_entity_name: &HashMap<&'a str, Vec<usize>>,
    entity_index: usize,
    path: Vec<usize>,
    preorder_rank: &mut usize,
    position_by_entity_name: &mut HashMap<&'a str, PresentationPosition>,
) {
    let entity = &document.entity_changes[entity_index];
    if position_by_entity_name.contains_key(entity.name.as_str()) {
        return;
    }
    position_by_entity_name.insert(
        entity.name.as_str(),
        PresentationPosition {
            path: path.clone(),
            preorder_rank: *preorder_rank,
        },
    );
    *preorder_rank += 1;
    for (child_offset, child_index) in child_index_by_entity_name
        .get(entity.name.as_str())
        .into_iter()
        .flatten()
        .enumerate()
    {
        let mut child_path = path.clone();
        child_path.push(child_offset + 1);
        index_presentation_subtree(
            document,
            child_index_by_entity_name,
            *child_index,
            child_path,
            preorder_rank,
            position_by_entity_name,
        );
    }
}

fn entity_type_expression_list(entity: &ProgramEntityChange) -> Vec<&str> {
    let mut type_expression_list = Vec::new();
    for member in &entity.members {
        type_expression_list.extend(member.type_name.as_deref());
        type_expression_list.extend(
            member
                .parameters
                .iter()
                .map(|parameter| parameter.type_name.as_str()),
        );
        type_expression_list.extend(member.return_type.as_deref());
    }
    type_expression_list.extend(
        entity
            .variants
            .iter()
            .flat_map(|variant| variant.fields.iter())
            .map(|field| field.type_name.as_str()),
    );
    type_expression_list
}

fn type_identifiers(type_expression: &str) -> impl Iterator<Item = &str> {
    type_expression
        .split(|character: char| !(character.is_alphanumeric() || character == '_'))
        .filter(|identifier| !identifier.is_empty())
}

fn cyclic_entity_names<'a>(parent_by_entity_name: &HashMap<&'a str, &'a str>) -> HashSet<&'a str> {
    let mut cyclic_entity_name_set = HashSet::new();
    for start_name in parent_by_entity_name.keys().copied() {
        let mut position_by_entity_name = HashMap::new();
        let mut ordered_entity_name_list = Vec::new();
        let mut current_name = start_name;
        while let Some(parent_name) = parent_by_entity_name.get(current_name).copied() {
            if let Some(cycle_start) = position_by_entity_name.get(current_name).copied() {
                cyclic_entity_name_set
                    .extend(ordered_entity_name_list.iter().skip(cycle_start).copied());
                break;
            }
            position_by_entity_name.insert(current_name, ordered_entity_name_list.len());
            ordered_entity_name_list.push(current_name);
            current_name = parent_name;
        }
    }
    cyclic_entity_name_set
}

fn is_contract_kind(kind: EntityKind) -> bool {
    matches!(
        kind,
        EntityKind::Trait | EntityKind::Interface | EntityKind::AbstractClass
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::plan::{
        ChangeAction, EntityChangeAction, EnumVariantChange, EnumVariantFieldChange,
        FunctionParameter, MemberKind, Visibility, document::test_fixture,
    };

    fn entity(path_stem: &str, name: &str, kind: EntityKind) -> ProgramEntityChange {
        ProgramEntityChange {
            action: EntityChangeAction::Add,
            kind,
            renamed_from: None,
            name: name.into(),
            description: format!("Defines {name}."),
            path: format!("src/{path_stem}.rs"),
            members: Vec::new(),
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
        }
    }

    fn member(
        member_name: &str,
        type_name: Option<&str>,
        parameter_type_list: &[&str],
        return_type: Option<&str>,
    ) -> ProgramEntityMemberChange {
        ProgramEntityMemberChange {
            action: ChangeAction::Add,
            renamed_from: None,
            kind: MemberKind::Method,
            name: member_name.into(),
            description: Some(format!("Defines {member_name}.")),
            visibility: Some(Visibility::Public),
            type_name: type_name.map(str::to_owned),
            parameters: parameter_type_list
                .iter()
                .enumerate()
                .map(|(parameter_index, type_name)| FunctionParameter {
                    name: format!("parameter_{parameter_index}"),
                    type_name: (*type_name).into(),
                })
                .collect(),
            return_type: return_type.map(str::to_owned),
        }
    }

    #[test]
    fn derives_concrete_children_from_structured_type_expressions() {
        let mut document = test_fixture("plan", "Overview");
        let report = entity("inspection_report", "InspectionReport", EntityKind::Struct);
        let error = entity("inspection_error", "InspectionError", EntityKind::Enum);
        let config = entity("inspection_config", "InspectionConfig", EntityKind::Config);
        let state = entity("inspection_state", "InspectionState", EntityKind::Struct);
        let mut inspector = entity(
            "geo_parquet_inspector",
            "GeoParquetInspector",
            EntityKind::Struct,
        );
        inspector.members = vec![
            member("state", Some("Option<InspectionState>"), &[], None),
            member(
                "inspect",
                None,
                &["&InspectionConfig", "&Path"],
                Some("Result<Vec<InspectionReport>, inspection::InspectionError>"),
            ),
            member("new", None, &[], Some("GeoParquetInspector")),
        ];
        let payload = entity(
            "inspection_payload",
            "InspectionPayload",
            EntityKind::Struct,
        );
        let mut envelope = entity(
            "inspection_envelope",
            "InspectionEnvelope",
            EntityKind::Enum,
        );
        envelope.variants = vec![EnumVariantChange {
            action: ChangeAction::Add,
            renamed_from: None,
            name: "Ready".into(),
            description: Some("Carries one inspection payload.".into()),
            fields: vec![EnumVariantFieldChange {
                action: ChangeAction::Add,
                renamed_from: None,
                name: "payload".into(),
                type_name: "InspectionPayload[]".into(),
                kind: None,
                description: None,
                visibility: None,
            }],
        }];
        document.entity_changes = vec![report, error, config, state, inspector, payload, envelope];

        let graph = PlanGraph::new(&document);

        for child_name in [
            "InspectionReport",
            "InspectionError",
            "InspectionConfig",
            "InspectionState",
        ] {
            assert_eq!(
                graph.presentation_parent(child_name),
                Some("GeoParquetInspector")
            );
        }
        assert_eq!(
            graph.presentation_parent("InspectionPayload"),
            Some("InspectionEnvelope")
        );
        assert_eq!(graph.presentation_parent("GeoParquetInspector"), None);
        assert_eq!(
            graph
                .presentation_position("GeoParquetInspector")
                .unwrap()
                .path(),
            &[1]
        );
        assert_eq!(
            graph
                .presentation_position("InspectionReport")
                .unwrap()
                .path(),
            &[1, 1]
        );
        assert_eq!(
            graph
                .presentation_position("InspectionError")
                .unwrap()
                .path(),
            &[1, 2]
        );
        assert_eq!(
            graph
                .presentation_position("InspectionError")
                .unwrap()
                .depth(),
            1
        );
        assert_eq!(
            graph
                .presentation_position("InspectionEnvelope")
                .unwrap()
                .path(),
            &[2]
        );
        assert_eq!(
            graph
                .presentation_position("InspectionPayload")
                .unwrap()
                .path(),
            &[2, 1]
        );
    }

    #[test]
    fn keeps_contract_targets_and_shared_concrete_entities_at_the_root() {
        let mut document = test_fixture("plan", "Overview");
        let mut backend = entity("backend", "Backend", EntityKind::Trait);
        backend.members = vec![member("load", None, &[], Some("ContractOutput"))];
        let contract_output = entity("contract_output", "ContractOutput", EntityKind::Struct);
        let shared_cache = entity("shared_cache", "SharedCache", EntityKind::Cache);
        let mut first_user = entity("first_user", "FirstUser", EntityKind::Struct);
        first_user.members = vec![member("cache", Some("@SharedCache"), &[], None)];
        let mut second_user = entity("second_user", "SecondUser", EntityKind::Struct);
        second_user.members = vec![member("cache", Some("&SharedCache"), &[], None)];
        document.entity_changes = vec![
            backend,
            contract_output,
            shared_cache,
            first_user,
            second_user,
        ];

        let graph = PlanGraph::new(&document);

        assert_eq!(graph.presentation_parent("Backend"), None);
        assert_eq!(graph.presentation_parent("ContractOutput"), None);
        assert_eq!(graph.presentation_parent("SharedCache"), None);
    }

    #[test]
    fn removes_cycles_and_preserves_acyclic_dependency_chains() {
        let mut document = test_fixture("plan", "Overview");
        let mut cycle_left = entity("cycle_left", "CycleLeft", EntityKind::Struct);
        cycle_left.members = vec![member("right", Some("CycleRight"), &[], None)];
        let mut cycle_right = entity("cycle_right", "CycleRight", EntityKind::Struct);
        cycle_right.members = vec![member("left", Some("CycleLeft"), &[], None)];
        let mut root = entity("root", "Root", EntityKind::Struct);
        root.members = vec![member("child", Some("Child"), &[], None)];
        let mut child = entity("child", "Child", EntityKind::Struct);
        child.members = vec![member("leaf", Some("Leaf"), &[], None)];
        let leaf = entity("leaf", "Leaf", EntityKind::Struct);
        document.entity_changes = vec![cycle_left, cycle_right, leaf, child, root];

        let graph = PlanGraph::new(&document);

        assert_eq!(graph.presentation_parent("CycleLeft"), None);
        assert_eq!(graph.presentation_parent("CycleRight"), None);
        assert_eq!(graph.presentation_parent("Child"), Some("Root"));
        assert_eq!(graph.presentation_parent("Leaf"), Some("Child"));
        assert_eq!(
            graph.presentation_child_indices("Root").collect::<Vec<_>>(),
            vec![3]
        );
        assert_eq!(
            graph.presentation_position("CycleLeft").unwrap().path(),
            &[1]
        );
        assert_eq!(
            graph.presentation_position("CycleRight").unwrap().path(),
            &[2]
        );
        assert_eq!(graph.presentation_position("Root").unwrap().path(), &[3]);
        assert_eq!(
            graph.presentation_position("Child").unwrap().path(),
            &[3, 1]
        );
        assert_eq!(
            graph.presentation_position("Leaf").unwrap().path(),
            &[3, 1, 1]
        );
    }

    #[test]
    fn orders_filtered_entity_sets_without_requiring_their_ancestors() {
        let mut document = test_fixture("plan", "Overview");
        let report = entity("inspection_report", "InspectionReport", EntityKind::Struct);
        let error = entity("inspection_error", "InspectionError", EntityKind::Enum);
        let mut inspector = entity(
            "geo_parquet_inspector",
            "GeoParquetInspector",
            EntityKind::Struct,
        );
        inspector.members = vec![member(
            "inspect",
            None,
            &[],
            Some("Result<InspectionReport, InspectionError>"),
        )];
        let main = entity("main", "main", EntityKind::Function);
        document.entity_changes = vec![report, error, inspector, main];

        let graph = PlanGraph::new(&document);

        let ordered_name_list = graph
            .entities_in_presentation_order(&[
                "InspectionError".into(),
                "main".into(),
                "InspectionReport".into(),
                "GeoParquetInspector".into(),
            ])
            .into_iter()
            .map(|entity| entity.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(
            ordered_name_list,
            vec![
                "GeoParquetInspector",
                "InspectionReport",
                "InspectionError",
                "main"
            ]
        );

        let missing_parent_name_list = graph
            .entities_in_presentation_order(&[
                "InspectionError".into(),
                "main".into(),
                "InspectionReport".into(),
            ])
            .into_iter()
            .map(|entity| entity.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(
            missing_parent_name_list,
            vec!["InspectionReport", "InspectionError", "main"]
        );
    }
}

use std::collections::{HashMap, HashSet};

use super::{
    ChangeAction, EntityKind, EnumVariantChange, EnumVariantFieldChange, PlanDependencyChange,
    PlanDocument, ProgramEntityChange, ProgramEntityMemberChange,
};

/// Represents the entity, member, variant field, or dependency behind one semantic identifier.
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
    entity_by_id: HashMap<&'a str, &'a ProgramEntityChange>,
    resolved_by_id: HashMap<&'a str, ResolvedPlanEntity<'a>>,
    presentation_parent_by_entity_id: HashMap<&'a str, &'a str>,
    presentation_child_index_by_entity_id: HashMap<&'a str, Vec<usize>>,
    presentation_position_by_entity_id: HashMap<&'a str, PresentationPosition>,
}

impl<'a> PlanGraph<'a> {
    /// Build one semantic index from the canonical document.
    pub fn new(document: &'a PlanDocument) -> Self {
        let mut resolved_by_id = HashMap::new();
        for entity in &document.entity_changes {
            resolved_by_id.insert(
                entity.entity_id.as_str(),
                ResolvedPlanEntity::Entity(entity),
            );
            resolved_by_id.insert(entity.name.as_str(), ResolvedPlanEntity::Entity(entity));
            for member in &entity.members {
                resolved_by_id.insert(
                    member.member_id.as_str(),
                    ResolvedPlanEntity::Member(entity, member),
                );
            }
            for variant in &entity.variants {
                resolved_by_id.insert(
                    variant.variant_id.as_str(),
                    ResolvedPlanEntity::Variant(entity, variant),
                );
                for field in &variant.fields {
                    resolved_by_id.insert(
                        field.field_id.as_str(),
                        ResolvedPlanEntity::VariantField(entity, variant, field),
                    );
                }
            }
        }
        for dependency in &document.dependencies {
            resolved_by_id.insert(
                dependency.dependency_id.as_str(),
                ResolvedPlanEntity::Dependency(dependency),
            );
        }
        let entity_by_id = document
            .entity_changes
            .iter()
            .flat_map(|entity| {
                [
                    (entity.entity_id.as_str(), entity),
                    (entity.name.as_str(), entity),
                ]
            })
            .collect();
        let presentation_hierarchy = build_presentation_hierarchy(document);
        Self {
            entity_by_id,
            resolved_by_id,
            presentation_parent_by_entity_id: presentation_hierarchy.parent_by_entity_id,
            presentation_child_index_by_entity_id: presentation_hierarchy.child_index_by_entity_id,
            presentation_position_by_entity_id: presentation_hierarchy.position_by_entity_id,
        }
    }

    /// Resolve one program entity from its stable identifier.
    pub fn entity(&self, entity_id: &str) -> Option<&'a ProgramEntityChange> {
        self.entity_by_id.get(entity_id).copied()
    }

    /// Resolve one exact semantic entity from the canonical object graph.
    pub fn resolve_entity(&self, entity_id: &str) -> Option<ResolvedPlanEntity<'a>> {
        self.resolved_by_id.get(entity_id).copied()
    }

    /// Resolve the repository path owned by one semantic entity.
    pub fn entity_path(&self, entity_id: &str) -> Option<&'a str> {
        match self.resolve_entity(entity_id)? {
            ResolvedPlanEntity::Entity(entity)
            | ResolvedPlanEntity::Member(entity, _)
            | ResolvedPlanEntity::Variant(entity, _)
            | ResolvedPlanEntity::VariantField(entity, _, _) => Some(&entity.path),
            ResolvedPlanEntity::Dependency(dependency) => Some(&dependency.manifest),
        }
    }

    /// Render one concise reviewer label for a semantic entity.
    pub fn entity_label(&self, entity_id: &str) -> Option<String> {
        match self.resolve_entity(entity_id)? {
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
    pub fn entity_action(&self, entity_id: &str) -> Option<ChangeAction> {
        match self.resolve_entity(entity_id)? {
            ResolvedPlanEntity::Entity(entity) => Some(entity.action.base_action()),
            ResolvedPlanEntity::Member(_, member) => Some(member.action),
            ResolvedPlanEntity::Variant(_, variant) => Some(variant.action),
            ResolvedPlanEntity::VariantField(_, _, field) => Some(field.action),
            ResolvedPlanEntity::Dependency(dependency) => Some(dependency.action),
        }
    }

    /// Resolve the sole concrete user that visually owns one concrete entity.
    pub fn presentation_parent(&self, entity_id: &str) -> Option<&'a str> {
        self.presentation_parent_by_entity_id
            .get(entity_id)
            .copied()
    }

    /// Iterate concrete entities visually nested beneath one concrete user.
    pub fn presentation_child_indices(&self, entity_id: &str) -> impl Iterator<Item = usize> + '_ {
        self.presentation_child_index_by_entity_id
            .get(entity_id)
            .into_iter()
            .flatten()
            .copied()
    }

    /// Resolve one entity's shared UML and task-list presentation position.
    pub fn presentation_position(&self, entity_id: &str) -> Option<&PresentationPosition> {
        let entity = self.entity(entity_id)?;
        self.presentation_position_by_entity_id
            .get(entity.entity_id.as_str())
    }

    /// Resolve known entity IDs and order them by the shared presentation traversal.
    pub fn entities_in_presentation_order(
        &self,
        entity_id_list: &[String],
    ) -> Vec<&'a ProgramEntityChange> {
        let mut entity_list = entity_id_list
            .iter()
            .enumerate()
            .filter_map(|(original_index, entity_id)| {
                self.entity(entity_id)
                    .map(|entity| (original_index, entity))
            })
            .collect::<Vec<_>>();
        entity_list.sort_by_key(|(original_index, entity)| {
            (
                self.presentation_position(&entity.entity_id)
                    .map(PresentationPosition::preorder_rank)
                    .unwrap_or(usize::MAX),
                *original_index,
            )
        });
        entity_list.into_iter().map(|(_, entity)| entity).collect()
    }
}

struct PresentationHierarchy<'a> {
    parent_by_entity_id: HashMap<&'a str, &'a str>,
    child_index_by_entity_id: HashMap<&'a str, Vec<usize>>,
    position_by_entity_id: HashMap<&'a str, PresentationPosition>,
}

fn build_presentation_hierarchy<'a>(document: &'a PlanDocument) -> PresentationHierarchy<'a> {
    let entity_by_name = document
        .entity_changes
        .iter()
        .map(|entity| (entity.name.as_str(), entity))
        .collect::<HashMap<_, _>>();
    let mut user_id_set_by_entity_id = HashMap::<&str, HashSet<&str>>::new();
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
                if target.entity_id == user.entity_id || is_contract_kind(target.kind) {
                    continue;
                }
                user_id_set_by_entity_id
                    .entry(target.entity_id.as_str())
                    .or_default()
                    .insert(user.entity_id.as_str());
            }
        }
    }

    let mut parent_by_entity_id = user_id_set_by_entity_id
        .into_iter()
        .filter_map(|(entity_id, user_id_set)| {
            (user_id_set.len() == 1).then(|| (entity_id, *user_id_set.iter().next().unwrap()))
        })
        .collect::<HashMap<_, _>>();
    let cyclic_entity_id_set = cyclic_entity_ids(&parent_by_entity_id);
    parent_by_entity_id.retain(|entity_id, _| !cyclic_entity_id_set.contains(entity_id));

    let mut child_index_by_entity_id = HashMap::<&str, Vec<usize>>::new();
    for (entity_index, entity) in document.entity_changes.iter().enumerate() {
        if let Some(parent_id) = parent_by_entity_id.get(entity.entity_id.as_str()) {
            child_index_by_entity_id
                .entry(*parent_id)
                .or_default()
                .push(entity_index);
        }
    }
    let position_by_entity_id = build_presentation_position_index(
        document,
        &parent_by_entity_id,
        &child_index_by_entity_id,
    );
    PresentationHierarchy {
        parent_by_entity_id,
        child_index_by_entity_id,
        position_by_entity_id,
    }
}

fn build_presentation_position_index<'a>(
    document: &'a PlanDocument,
    parent_by_entity_id: &HashMap<&'a str, &'a str>,
    child_index_by_entity_id: &HashMap<&'a str, Vec<usize>>,
) -> HashMap<&'a str, PresentationPosition> {
    let mut position_by_entity_id = HashMap::new();
    let mut preorder_rank = 0;
    let mut root_ordinal = 0;
    for (entity_index, entity) in document.entity_changes.iter().enumerate() {
        if parent_by_entity_id.contains_key(entity.entity_id.as_str()) {
            continue;
        }
        root_ordinal += 1;
        index_presentation_subtree(
            document,
            child_index_by_entity_id,
            entity_index,
            vec![root_ordinal],
            &mut preorder_rank,
            &mut position_by_entity_id,
        );
    }
    position_by_entity_id
}

fn index_presentation_subtree<'a>(
    document: &'a PlanDocument,
    child_index_by_entity_id: &HashMap<&'a str, Vec<usize>>,
    entity_index: usize,
    path: Vec<usize>,
    preorder_rank: &mut usize,
    position_by_entity_id: &mut HashMap<&'a str, PresentationPosition>,
) {
    let entity = &document.entity_changes[entity_index];
    if position_by_entity_id.contains_key(entity.entity_id.as_str()) {
        return;
    }
    position_by_entity_id.insert(
        entity.entity_id.as_str(),
        PresentationPosition {
            path: path.clone(),
            preorder_rank: *preorder_rank,
        },
    );
    *preorder_rank += 1;
    for (child_offset, child_index) in child_index_by_entity_id
        .get(entity.entity_id.as_str())
        .into_iter()
        .flatten()
        .enumerate()
    {
        let mut child_path = path.clone();
        child_path.push(child_offset + 1);
        index_presentation_subtree(
            document,
            child_index_by_entity_id,
            *child_index,
            child_path,
            preorder_rank,
            position_by_entity_id,
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

fn cyclic_entity_ids<'a>(parent_by_entity_id: &HashMap<&'a str, &'a str>) -> HashSet<&'a str> {
    let mut cyclic_entity_id_set = HashSet::new();
    for start_id in parent_by_entity_id.keys().copied() {
        let mut position_by_entity_id = HashMap::new();
        let mut ordered_entity_id_list = Vec::new();
        let mut current_id = start_id;
        while let Some(parent_id) = parent_by_entity_id.get(current_id).copied() {
            if let Some(cycle_start) = position_by_entity_id.get(current_id).copied() {
                cyclic_entity_id_set
                    .extend(ordered_entity_id_list.iter().skip(cycle_start).copied());
                break;
            }
            position_by_entity_id.insert(current_id, ordered_entity_id_list.len());
            ordered_entity_id_list.push(current_id);
            current_id = parent_id;
        }
    }
    cyclic_entity_id_set
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

    fn entity(entity_id: &str, name: &str, kind: EntityKind) -> ProgramEntityChange {
        ProgramEntityChange {
            entity_id: entity_id.into(),
            action: EntityChangeAction::Add,
            kind,
            renamed_from: None,
            name: name.into(),
            description: format!("Defines {name}."),
            path: format!("src/{entity_id}.rs"),
            members: Vec::new(),
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
        }
    }

    fn member(
        member_id: &str,
        type_name: Option<&str>,
        parameter_type_list: &[&str],
        return_type: Option<&str>,
    ) -> ProgramEntityMemberChange {
        ProgramEntityMemberChange {
            member_id: member_id.into(),
            action: ChangeAction::Add,
            kind: MemberKind::Method,
            name: member_id.into(),
            description: format!("Defines {member_id}."),
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
            variant_id: "inspection_ready".into(),
            action: ChangeAction::Add,
            name: "Ready".into(),
            description: "Carries one inspection payload.".into(),
            fields: vec![EnumVariantFieldChange {
                field_id: "inspection_ready_payload".into(),
                action: ChangeAction::Add,
                name: "payload".into(),
                type_name: "InspectionPayload[]".into(),
            }],
        }];
        document.entity_changes = vec![report, error, config, state, inspector, payload, envelope];

        let graph = PlanGraph::new(&document);

        for child_id in [
            "inspection_report",
            "inspection_error",
            "inspection_config",
            "inspection_state",
        ] {
            assert_eq!(
                graph.presentation_parent(child_id),
                Some("geo_parquet_inspector")
            );
        }
        assert_eq!(
            graph.presentation_parent("inspection_payload"),
            Some("inspection_envelope")
        );
        assert_eq!(graph.presentation_parent("geo_parquet_inspector"), None);
        assert_eq!(
            graph
                .presentation_position("geo_parquet_inspector")
                .unwrap()
                .path(),
            &[1]
        );
        assert_eq!(
            graph
                .presentation_position("inspection_report")
                .unwrap()
                .path(),
            &[1, 1]
        );
        assert_eq!(
            graph
                .presentation_position("inspection_error")
                .unwrap()
                .path(),
            &[1, 2]
        );
        assert_eq!(
            graph
                .presentation_position("inspection_error")
                .unwrap()
                .depth(),
            1
        );
        assert_eq!(
            graph
                .presentation_position("inspection_envelope")
                .unwrap()
                .path(),
            &[2]
        );
        assert_eq!(
            graph
                .presentation_position("inspection_payload")
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

        assert_eq!(graph.presentation_parent("backend"), None);
        assert_eq!(graph.presentation_parent("contract_output"), None);
        assert_eq!(graph.presentation_parent("shared_cache"), None);
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

        assert_eq!(graph.presentation_parent("cycle_left"), None);
        assert_eq!(graph.presentation_parent("cycle_right"), None);
        assert_eq!(graph.presentation_parent("child"), Some("root"));
        assert_eq!(graph.presentation_parent("leaf"), Some("child"));
        assert_eq!(
            graph.presentation_child_indices("root").collect::<Vec<_>>(),
            vec![3]
        );
        assert_eq!(
            graph.presentation_position("cycle_left").unwrap().path(),
            &[1]
        );
        assert_eq!(
            graph.presentation_position("cycle_right").unwrap().path(),
            &[2]
        );
        assert_eq!(graph.presentation_position("root").unwrap().path(), &[3]);
        assert_eq!(
            graph.presentation_position("child").unwrap().path(),
            &[3, 1]
        );
        assert_eq!(
            graph.presentation_position("leaf").unwrap().path(),
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
                "inspection_error".into(),
                "main".into(),
                "inspection_report".into(),
                "geo_parquet_inspector".into(),
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
                "inspection_error".into(),
                "main".into(),
                "inspection_report".into(),
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

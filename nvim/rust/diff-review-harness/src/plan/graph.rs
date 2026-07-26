use std::collections::HashMap;

use super::{
    ChangeAction, EnumVariantChange, EnumVariantFieldChange, PlanDependencyChange, PlanDocument,
    ProgramEntityChange, ProgramEntityMemberChange,
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

/// Indexes semantic plan entities for validation, rendering, review, and execution.
pub struct PlanGraph<'a> {
    entity_by_id: HashMap<&'a str, &'a ProgramEntityChange>,
    resolved_by_id: HashMap<&'a str, ResolvedPlanEntity<'a>>,
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
        Self {
            entity_by_id: document
                .entity_changes
                .iter()
                .flat_map(|entity| {
                    [
                        (entity.entity_id.as_str(), entity),
                        (entity.name.as_str(), entity),
                    ]
                })
                .collect(),
            resolved_by_id,
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
            ResolvedPlanEntity::Entity(entity) => Some(entity.action),
            ResolvedPlanEntity::Member(_, member) => Some(member.action),
            ResolvedPlanEntity::Variant(_, variant) => Some(variant.action),
            ResolvedPlanEntity::VariantField(_, _, field) => Some(field.action),
            ResolvedPlanEntity::Dependency(dependency) => Some(dependency.action),
        }
    }
}

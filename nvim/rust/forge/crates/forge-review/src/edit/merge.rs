use super::*;

pub struct PreparedFieldMerge<'store> {
    store: &'store mut EditStore,
    field: BTreeMap<RegionId, EditableField>,
    outcome: BTreeMap<RegionId, MergeOutcome>,
}
impl PreparedFieldMerge<'_> {
    pub fn snapshot(&self, region: &RegionId) -> Result<FieldSnapshot<'_>, EditError> {
        let field = self.field.get(region).ok_or(EditError::UnknownRegion)?;
        Ok(FieldSnapshot {
            text: &field.current.value,
            baseline: &field.baseline.value,
            revision: field.revision,
            sequence: field.sequence,
            dirty: field.current.value != field.baseline.value,
            pending_saves: 0,
            uncertain: false,
            remote: field.remote.as_ref().map(|text| text.value.as_ref()),
        })
    }
    pub fn outcome(&self, region: &RegionId) -> Result<&MergeOutcome, EditError> {
        self.outcome.get(region).ok_or(EditError::UnknownRegion)
    }
    pub fn commit(self) {
        for (region, field) in self.field {
            *self
                .store
                .field
                .get_mut(&region)
                .expect("prepared merge region") = field;
        }
    }
}
impl EditStore {
    pub fn prepare_merge(
        &mut self,
        observed: Vec<(RegionId, String)>,
    ) -> Result<PreparedFieldMerge<'_>, EditError> {
        if observed.is_empty() || observed.len() > self.limits.active_fields {
            return Err(EditError::Capacity("merge field admission is full"));
        }
        let mut candidate = BTreeMap::new();
        let mut outcome = BTreeMap::new();
        for (region, observed) in observed {
            self.validate_text(&observed)?;
            if candidate.contains_key(&region) {
                return Err(EditError::Invalid("duplicate merge region"));
            }
            let current = self.field.get(&region).ok_or(EditError::UnknownRegion)?;
            if self
                .pending
                .values()
                .any(|pending| pending.record.region == region)
            {
                return Err(EditError::AlreadySaving);
            }
            let mut field = current.clone();
            let result = if current.baseline.value.as_ref() == observed {
                field.remote = None;
                MergeOutcome::Unchanged
            } else if current.current.value.as_ref() == observed {
                field.baseline = Arc::clone(&field.current);
                field.remote = None;
                MergeOutcome::Converged
            } else {
                let clean =
                    current.current.value == current.baseline.value && current.remote.is_none();
                let text = self.retain_text(current, observed)?;
                if clean {
                    field.revision = current.revision.next()?;
                    field.current = Arc::clone(&text);
                    field.baseline = text;
                    MergeOutcome::Updated {
                        revision: field.revision,
                    }
                } else {
                    field.remote = Some(text);
                    MergeOutcome::Conflict
                }
            };
            candidate.insert(region.clone(), field);
            outcome.insert(region, result);
        }
        Ok(PreparedFieldMerge {
            store: self,
            field: candidate,
            outcome,
        })
    }
}

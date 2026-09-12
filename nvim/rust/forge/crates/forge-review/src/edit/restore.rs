use super::*;

#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FieldRestoration {
    pub revision: RegionRevision,
    pub sequence: EditSequence,
    pub text: String,
    pub baseline: String,
    pub remote: Option<String>,
    pub pending: Option<SaveRestoration>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SaveRestoration {
    pub revision: RegionRevision,
    pub sequence: EditSequence,
    pub text: String,
    pub uncertain: bool,
}

impl EditStore {
    pub fn restore_field(
        &mut self,
        region: RegionId,
        restored: FieldRestoration,
    ) -> Result<Option<SaveSubmission>, EditError> {
        region.validate()?;
        restored.revision.validate()?;
        restored.sequence.validate()?;
        self.validate_text(&restored.text)?;
        self.validate_text(&restored.baseline)?;
        if let Some(remote) = &restored.remote {
            self.validate_text(remote)?;
        }
        if let Some(field) = self.field.get(&region) {
            if field.revision != RegionRevision(0)
                || field.sequence != EditSequence(0)
                || field.current.value != field.baseline.value
                || field.remote.is_some()
                || field.settled_save != 0
            {
                return Err(EditError::Invalid("restoration requires a pristine region"));
            }
        } else {
            if self.used_region.contains(&region) {
                return Err(EditError::Invalid(
                    "review region identity was already used",
                ));
            }
            if self.field.len() >= self.limits.active_fields
                || self.used_region.len() >= self.limits.region_lifetimes
            {
                return Err(EditError::Capacity("review region admission is full"));
            }
        }
        if self
            .pending
            .values()
            .any(|pending| pending.record.region == region)
        {
            return Err(EditError::AlreadySaving);
        }
        let serial = if let Some(pending) = &restored.pending {
            pending.revision.validate()?;
            pending.sequence.validate()?;
            self.validate_text(&pending.text)?;
            if pending.revision > restored.revision
                || pending.sequence > restored.sequence
                || (pending.sequence == restored.sequence && pending.text != restored.text)
            {
                return Err(EditError::Invalid(
                    "restored capture does not precede current field state",
                ));
            }
            if self.pending.len() >= self.limits.pending_saves {
                return Err(EditError::Capacity("review save admission is full"));
            }
            Some(
                self.next_save
                    .checked_add(1)
                    .filter(|serial| *serial <= MAX_COUNTER)
                    .ok_or(EditError::Capacity("review save identity exhausted"))?,
            )
        } else {
            None
        };
        let current = self.budget.retain(restored.text)?;
        let baseline = if current.value.as_ref() == restored.baseline {
            Arc::clone(&current)
        } else {
            self.budget.retain(restored.baseline)?
        };
        let mut field = EditableField {
            current,
            baseline,
            revision: restored.revision,
            sequence: restored.sequence,
            settled_save: 0,
            remote: None,
        };
        if let Some(remote) = restored.remote {
            field.remote = Some(self.retain_text(&field, remote)?);
        }
        let pending = restored
            .pending
            .map(|pending| {
                Ok::<_, EditError>(PendingSave {
                    record: Arc::new(SaveRecord {
                        owner: Arc::clone(&self.owner),
                        serial: serial.expect("validated restore serial"),
                        region: region.clone(),
                        revision: pending.revision,
                        sequence: pending.sequence,
                        text: self.retain_text(&field, pending.text)?,
                    }),
                    uncertain: pending.uncertain,
                })
            })
            .transpose()?;
        self.used_region.insert(region.clone());
        self.field.insert(region, field);
        Ok(pending.map(|pending| {
            self.next_save = pending.record.serial;
            let submission = SaveSubmission {
                record: Arc::clone(&pending.record),
            };
            self.pending.insert(pending.record.serial, pending);
            submission
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn restoration() -> FieldRestoration {
        FieldRestoration {
            revision: RegionRevision(9),
            sequence: EditSequence(100),
            text: "newer".into(),
            baseline: "baseline".into(),
            remote: None,
            pending: Some(SaveRestoration {
                revision: RegionRevision(8),
                sequence: EditSequence(90),
                text: "submitted".into(),
                uncertain: true,
            }),
        }
    }

    #[test]
    fn restored_capture_keeps_exact_counters_and_new_owner_settlement() {
        let budget = EditBudget::new(1024).unwrap();
        let identity = DocumentId("restored".into());
        let region = RegionId("body".into());
        let mut store = EditStore::new(identity.clone(), EditLimits::default(), budget).unwrap();
        let capture = store
            .restore_field(region.clone(), restoration())
            .unwrap()
            .unwrap();
        assert_eq!(
            (capture.revision(), capture.sequence(), capture.text()),
            (RegionRevision(8), EditSequence(90), "submitted")
        );
        let field = store.snapshot(&region).unwrap();
        assert_eq!(
            (field.revision, field.sequence),
            (RegionRevision(9), EditSequence(100))
        );
        assert!(field.uncertain && field.dirty);
        let mut foreign = EditStore::new(
            DocumentId("foreign".into()),
            EditLimits::default(),
            EditBudget::new(1024).unwrap(),
        )
        .unwrap();
        assert_eq!(
            foreign
                .reconcile_save(&capture, "submitted".into())
                .unwrap_err(),
            EditError::WrongOwner
        );
        store
            .accept(RegionEdit {
                document: identity,
                region: region.clone(),
                base: RegionRevision(9),
                sequence: EditSequence(101),
                text: "latest".into(),
            })
            .unwrap();
        store.reconcile_save(&capture, "submitted".into()).unwrap();
        let field = store.snapshot(&region).unwrap();
        assert_eq!(
            (field.text, field.baseline, field.sequence),
            ("latest", "submitted", EditSequence(101))
        );
        assert!(field.dirty && !field.uncertain);
    }

    #[test]
    fn failed_restoration_preserves_allocation_and_region_admission() {
        let budget = EditBudget::new(10).unwrap();
        let mut store = EditStore::new(
            DocumentId("restored".into()),
            EditLimits::default(),
            budget.clone(),
        )
        .unwrap();
        let region = RegionId("body".into());
        assert!(store.restore_field(region.clone(), restoration()).is_err());
        assert_eq!(budget.retained_bytes(), 0);
        assert_eq!(store.next_save, 0);
        assert!(store.snapshot(&region).is_err());
        store
            .insert(region.clone(), RegionRevision(0), "same".into())
            .unwrap();
        let mut restored = restoration();
        restored.pending.as_mut().unwrap().sequence = EditSequence(101);
        assert!(store.restore_field(region.clone(), restored).is_err());
        assert_eq!(store.snapshot(&region).unwrap().text, "same");
        assert_eq!(budget.retained_bytes(), 4);
    }

    #[test]
    fn restoration_deduplicates_text_and_rejects_accepted_regions() {
        let budget = EditBudget::new(4).unwrap();
        let mut store = EditStore::new(
            DocumentId("restored".into()),
            EditLimits::default(),
            budget.clone(),
        )
        .unwrap();
        let region = RegionId("body".into());
        store
            .restore_field(
                region.clone(),
                FieldRestoration {
                    revision: RegionRevision(5),
                    sequence: EditSequence(100),
                    text: "same".into(),
                    baseline: "same".into(),
                    remote: Some("same".into()),
                    pending: Some(SaveRestoration {
                        revision: RegionRevision(5),
                        sequence: EditSequence(100),
                        text: "same".into(),
                        uncertain: true,
                    }),
                },
            )
            .unwrap();
        assert_eq!(budget.retained_bytes(), 4);
        assert!(store.restore_field(region.clone(), restoration()).is_err());
        assert_eq!(store.snapshot(&region).unwrap().sequence, EditSequence(100));
    }
}

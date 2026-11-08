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

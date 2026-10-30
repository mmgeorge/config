//! Document-owned diff controls with atomic publication of prepared presentations.

/// Declaration selection, independent of body presentation.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DiffVisibility {
    #[default]
    All,
    Public,
}

/// Body presentation. Full retains the consumer's existing diff context policy.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DiffPresentation {
    #[default]
    Full,
    Semantic,
}

/// Presentations admitted by a document throughout its lifetime.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DiffCapabilities {
    Source,
    SemanticOnly,
}

/// Independent controls published with the corresponding buffer contents.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct DiffViewOptions {
    pub visibility: DiffVisibility,
    pub presentation: DiffPresentation,
}

/// Shared Normal-mode actions, mapped by the editor to ov and ox.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiffViewAction {
    ToggleVisibility,
    TogglePresentation,
}

/// Immutable preparation token scoped to its owning document and source comparison.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DiffViewRequest {
    pub generation: u64,
    pub options: DiffViewOptions,
}

/// Control failures leave both requested and committed state unchanged.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiffViewError {
    UnsupportedPresentation,
    GenerationExhausted,
}

/// Retains the usable settings while a successor prepares outside the document lock.
///
/// The document owner serializes access and checks source identity before publication.
/// Requests never perform I/O, syntax analysis, or buffer mutation.
pub struct DiffViewState {
    capabilities: DiffCapabilities,
    committed: DiffViewOptions,
    requested: DiffViewRequest,
}

impl DiffViewState {
    /// Initialize source documents in Full and declaration-only documents in Semantic.
    pub fn new(capabilities: DiffCapabilities, visibility: DiffVisibility) -> Self {
        let options = DiffViewOptions {
            visibility,
            presentation: match capabilities {
                DiffCapabilities::Source => DiffPresentation::Full,
                DiffCapabilities::SemanticOnly => DiffPresentation::Semantic,
            },
        };
        Self {
            capabilities,
            committed: options,
            requested: DiffViewRequest {
                generation: 0,
                options,
            },
        }
    }

    /// Return controls for the last successfully published buffer.
    pub fn committed(&self) -> DiffViewOptions {
        self.committed
    }

    /// Return the latest desired settings, including changes still preparing.
    pub fn requested(&self) -> DiffViewRequest {
        self.requested
    }

    /// Accept an action against the latest desired state, coalescing rapid toggles.
    ///
    /// Semantic-only documents reject presentation actions. Counter exhaustion also
    /// returns an error without changing either state.
    pub fn request(&mut self, action: DiffViewAction) -> Result<DiffViewRequest, DiffViewError> {
        let mut options = self.requested.options;
        match action {
            DiffViewAction::ToggleVisibility => {
                options.visibility = match options.visibility {
                    DiffVisibility::All => DiffVisibility::Public,
                    DiffVisibility::Public => DiffVisibility::All,
                };
            }
            DiffViewAction::TogglePresentation => {
                options.presentation = match options.presentation {
                    DiffPresentation::Full => DiffPresentation::Semantic,
                    DiffPresentation::Semantic => DiffPresentation::Full,
                };
            }
        }
        self.request_options(options)
    }

    /// Prepare explicit settings for source views sharing one atomic document update.
    /// Unsupported presentations and counter exhaustion leave the desired settings unchanged.
    pub fn request_options(
        &mut self,
        options: DiffViewOptions,
    ) -> Result<DiffViewRequest, DiffViewError> {
        if self.capabilities == DiffCapabilities::SemanticOnly
            && options.presentation != DiffPresentation::Semantic
        {
            return Err(DiffViewError::UnsupportedPresentation);
        }
        let generation = self
            .requested
            .generation
            .checked_add(1)
            .ok_or(DiffViewError::GenerationExhausted)?;
        self.requested = DiffViewRequest {
            generation,
            options,
        };
        Ok(self.requested)
    }

    /// Publish settings only for the latest successfully prepared request.
    ///
    /// The owner must validate the document and source comparison and apply the buffer
    /// update in the same non-yielding commit. Stale requests return false.
    pub fn commit(&mut self, request: DiffViewRequest) -> bool {
        if request != self.requested {
            return false;
        }
        self.committed = request.options;
        true
    }

    /// Roll a failed current preparation back to the last usable settings.
    /// Superseded failures leave newer requests untouched.
    pub fn fail(&mut self, request: DiffViewRequest) -> bool {
        if request != self.requested {
            return false;
        }
        self.requested.options = self.committed;
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rapid_toggles_preserve_committed_controls_and_reject_stale_completion() {
        let mut state = DiffViewState::new(DiffCapabilities::Source, DiffVisibility::All);
        let semantic = state.request(DiffViewAction::TogglePresentation).unwrap();
        let public = state.request(DiffViewAction::ToggleVisibility).unwrap();
        assert_eq!(state.committed(), DiffViewOptions::default());
        assert!(!state.commit(semantic));
        assert!(!state.fail(semantic));
        assert!(state.commit(public));
        assert_eq!(
            state.committed(),
            DiffViewOptions {
                visibility: DiffVisibility::Public,
                presentation: DiffPresentation::Semantic,
            }
        );
        let full = state.request(DiffViewAction::TogglePresentation).unwrap();
        assert!(state.fail(full));
        assert_eq!(state.requested().options, state.committed());
        assert!(!state.commit(full));
    }

    #[test]
    fn semantic_only_preserves_visibility_and_rejects_full_without_mutation() {
        let mut state = DiffViewState::new(DiffCapabilities::SemanticOnly, DiffVisibility::Public);
        let initial = state.requested();
        assert_eq!(
            state.request(DiffViewAction::TogglePresentation),
            Err(DiffViewError::UnsupportedPresentation)
        );
        assert_eq!(state.requested(), initial);
        let all = state.request(DiffViewAction::ToggleVisibility).unwrap();
        assert!(state.commit(all));
        assert_eq!(state.committed().visibility, DiffVisibility::All);
        assert_eq!(state.committed().presentation, DiffPresentation::Semantic);
    }
}

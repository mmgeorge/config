use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::EditError;

/// Shared text allocation bound that includes submissions retained after their store closes.
#[derive(Clone, Debug)]
pub struct EditBudget {
    state: Arc<BudgetState>,
}

#[derive(Debug)]
struct BudgetState {
    limit: usize,
    retained: AtomicUsize,
}

#[derive(Debug)]
pub(super) struct StoredText {
    pub(super) value: Box<str>,
    budget: EditBudget,
}

impl EditBudget {
    /// Requires a positive byte limit. Clones share the same accounting lifetime.
    pub fn new(limit: usize) -> Result<Self, EditError> {
        if limit == 0 {
            return Err(EditError::Invalid("edit byte budget must be positive"));
        }
        Ok(Self {
            state: Arc::new(BudgetState {
                limit,
                retained: AtomicUsize::new(0),
            }),
        })
    }

    /// Returns charged UTF-8 text bytes, excluding field and allocator metadata.
    pub fn retained_bytes(&self) -> usize {
        self.state.retained.load(Ordering::Acquire)
    }

    pub(super) fn retain(&self, text: String) -> Result<Arc<StoredText>, EditError> {
        let bytes = text.len();
        self.state
            .retained
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |retained| {
                retained
                    .checked_add(bytes)
                    .filter(|next| *next <= self.state.limit)
            })
            .map_err(|_| EditError::Capacity("edit text budget is full"))?;
        Ok(Arc::new(StoredText {
            value: text.into_boxed_str(),
            budget: self.clone(),
        }))
    }
}

impl Drop for StoredText {
    fn drop(&mut self) {
        self.budget
            .state
            .retained
            .fetch_sub(self.value.len(), Ordering::AcqRel);
    }
}

use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::EditError;

/// Tracks text retained by stores and submissions across their shared lifetime.
#[derive(Clone, Debug, Default)]
pub struct EditBudget {
    state: Arc<BudgetState>,
}

#[derive(Debug, Default)]
struct BudgetState {
    retained: AtomicUsize,
}

#[derive(Debug)]
pub(super) struct StoredText {
    pub(super) value: Box<str>,
    budget: EditBudget,
}

impl EditBudget {
    /// Returns charged UTF-8 text bytes, excluding field and allocator metadata.
    pub fn retained_bytes(&self) -> usize {
        self.state.retained.load(Ordering::Acquire)
    }

    pub(super) fn retain(&self, text: String) -> Result<Arc<StoredText>, EditError> {
        let bytes = text.len();
        self.state
            .retained
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |retained| {
                retained.checked_add(bytes)
            })
            .map_err(|_| EditError::Capacity("edit text accounting overflow"))?;
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

use std::collections::HashMap;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};

use crate::{ContractError, identity::DocumentId};

/// Retains at most eight document identities until their admission owners drop.
#[derive(Default)]
pub struct DocumentAdmissionStore {
    document: Mutex<HashMap<DocumentId, Arc<AtomicBool>>>,
    released: tokio::sync::Notify,
}

/// Keeps an opening or retained document admitted through cancellation and collection.
pub struct DocumentAdmission {
    store: Arc<DocumentAdmissionStore>,
    id: DocumentId,
    cancelled: Arc<AtomicBool>,
}

impl DocumentAdmissionStore {
    /// Rejects duplicate identities and a ninth admission without replacing a live owner.
    pub fn admit(self: &Arc<Self>, id: DocumentId) -> Result<DocumentAdmission, ContractError> {
        let mut document = self.document.lock().expect("document admission lock");
        if document.contains_key(&id) {
            return Err(ContractError("document identity is already admitted"));
        }
        if document.len() >= 8 {
            return Err(ContractError("document admission is full"));
        }
        let cancelled = Arc::new(AtomicBool::new(false));
        document.insert(id.clone(), Arc::clone(&cancelled));
        Ok(DocumentAdmission {
            store: Arc::clone(self),
            id,
            cancelled,
        })
    }

    /// Revokes adoption while retaining capacity until the admission owner drops.
    pub fn cancel(&self, id: &DocumentId) -> bool {
        let document = self.document.lock().expect("document admission lock");
        let Some(cancelled) = document.get(id) else {
            return false;
        };
        cancelled.store(true, Ordering::Release);
        true
    }

    /// Revokes every currently admitted document without releasing its ownership.
    pub fn cancel_all(&self) {
        for cancelled in self
            .document
            .lock()
            .expect("document admission lock")
            .values()
        {
            cancelled.store(true, Ordering::Release);
        }
    }

    /// Waits for owner release, including when cancellation has already been requested.
    pub async fn wait_closed(&self, id: &DocumentId) {
        loop {
            let released = self.released.notified();
            tokio::pin!(released);
            released.as_mut().enable();
            if !self
                .document
                .lock()
                .expect("document admission lock")
                .contains_key(id)
            {
                return;
            }
            released.await;
        }
    }
}

impl DocumentAdmission {
    /// Rejects adoption after cancellation without releasing the retained owner.
    pub fn check(&self) -> Result<(), ContractError> {
        if self.cancelled.load(Ordering::Acquire) {
            return Err(ContractError("document closed before adoption"));
        }
        Ok(())
    }
}

impl Drop for DocumentAdmission {
    fn drop(&mut self) {
        self.store
            .document
            .lock()
            .expect("document admission lock")
            .remove(&self.id);
        self.store.released.notify_waiters();
    }
}

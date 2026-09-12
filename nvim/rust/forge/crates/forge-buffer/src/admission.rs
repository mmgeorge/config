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

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn cancellation_retains_capacity_until_collected_and_wakes_every_waiter() {
        let store = Arc::new(DocumentAdmissionStore::default());
        let mut owner = Vec::new();
        for index in 0..8 {
            owner.push(
                store
                    .admit(DocumentId(format!("document-{index}")))
                    .unwrap(),
            );
        }
        let first = DocumentId("document-0".into());
        assert!(store.cancel(&first));
        assert!(owner[0].check().is_err());
        assert!(store.admit(first.clone()).is_err());
        assert!(store.admit(DocumentId("overflow".into())).is_err());
        assert!(
            tokio::time::timeout(Duration::from_millis(1), store.wait_closed(&first))
                .await
                .is_err()
        );
        let first_waiter = tokio::spawn({
            let store = Arc::clone(&store);
            let first = first.clone();
            async move { store.wait_closed(&first).await }
        });
        let second_waiter = tokio::spawn({
            let store = Arc::clone(&store);
            let first = first.clone();
            async move { store.wait_closed(&first).await }
        });
        tokio::task::yield_now().await;
        drop(owner.remove(0));
        tokio::time::timeout(Duration::from_secs(1), first_waiter)
            .await
            .unwrap()
            .unwrap();
        tokio::time::timeout(Duration::from_secs(1), second_waiter)
            .await
            .unwrap()
            .unwrap();
        owner.push(store.admit(first).unwrap());
        store.cancel_all();
        assert!(owner.iter().all(|owner| owner.check().is_err()));
    }
}

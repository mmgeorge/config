use crate::{
    TextError,
    contract::{CorrectRequest, CorrectionDecision, InitializeRequest, ProfileId, ProfileReady},
    model::Model,
};
use sha2::{Digest, Sha256};
use std::{
    collections::{HashMap, HashSet},
    io::BufReader,
    sync::{
        Arc, Mutex, OnceLock,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
    time::Duration,
};
use tokio::sync::{Notify, Semaphore};

/// Own bounded local workers and immutable terminology profiles independently of providers.
#[derive(Clone)]
pub struct TextCorrectionService {
    inner: Arc<ServiceState>,
}
struct ServiceState {
    base: OnceLock<Arc<Model>>,
    profile: Mutex<ProfileStore>,
    worker: Arc<Semaphore>,
    closed: AtomicBool,
    active: AtomicUsize,
    idle: Notify,
}
struct ProfileStore {
    published: HashMap<ProfileId, Arc<Profile>>,
    requested: HashMap<ProfileId, usize>,
    sequence: usize,
}
struct Profile {
    model: Arc<Model>,
    dictionary: symspell_rs::SymSpell,
    terminology: HashSet<String>,
    revision: String,
}
struct WorkerGuard {
    state: Arc<ServiceState>,
}
impl Drop for WorkerGuard {
    fn drop(&mut self) {
        self.state.active.fetch_sub(1, Ordering::AcqRel);
        self.state.idle.notify_waiters();
    }
}
impl Default for TextCorrectionService {
    fn default() -> Self {
        Self::new()
    }
}
impl TextCorrectionService {
    /// Create an idle owner without loading dictionaries or starting a provider.
    pub fn new() -> Self {
        Self {
            inner: Arc::new(ServiceState {
                base: OnceLock::new(),
                profile: Mutex::new(ProfileStore {
                    published: HashMap::new(),
                    requested: HashMap::new(),
                    sequence: 0,
                }),
                worker: Arc::new(Semaphore::new(2)),
                closed: AtomicBool::new(false),
                active: AtomicUsize::new(0),
                idle: Notify::new(),
            }),
        }
    }

    /// Validate a complete replacement and publish it without altering other profiles.
    pub async fn initialize(&self, request: InitializeRequest) -> Result<ProfileReady, TextError> {
        let initialization = {
            let mut profiles = self
                .inner
                .profile
                .lock()
                .map_err(|_| TextError::Worker("profile lock poisoned".into()))?;
            profiles.sequence += 1;
            let sequence = profiles.sequence;
            profiles.requested.insert(request.profile.clone(), sequence);
            sequence
        };
        let owner = self.inner.clone();
        self.execute(move || {
            let model = if let Some(paths) = request.assets {
                Arc::new(Model::load(
                    BufReader::new(std::fs::File::open(paths.unigram)?),
                    BufReader::new(std::fs::File::open(paths.bigram)?),
                    &owner.closed,
                )?)
            } else if let Some(model) = owner.base.get() {
                model.clone()
            } else {
                let model = Arc::new(Model::bundled(&owner.closed)?);
                owner.base.get_or_init(|| model).clone()
            };
            let mut terminology = HashSet::new();
            let mut dictionary = symspell_rs::SymSpell::new(2, None, 7, 1);
            for term in request.terminology {
                let words = crate::token::words(&term);
                if words.len() != 1 || words[0].text != term {
                    return Err(TextError::InvalidInput(
                        "terminology must contain complete individual words".into(),
                    ));
                }
                dictionary.create_dictionary_entry(term.to_lowercase(), 1);
                terminology.insert(term.to_lowercase());
            }
            let mut ordered: Vec<_> = terminology.iter().collect();
            ordered.sort();
            let mut digest = Sha256::new();
            digest.update(crate::model::MODEL_REVISION);
            digest.update(model.revision());
            for term in ordered {
                digest.update(term.as_bytes());
                digest.update([0]);
            }
            let revision = format!("{:x}", digest.finalize());
            let ready = ProfileReady {
                profile: request.profile.clone(),
                profile_revision: revision.clone(),
                model_revision: crate::model::MODEL_REVISION.into(),
                dictionary_revision: model.revision().into(),
            };
            if owner.closed.load(Ordering::Acquire) {
                return Err(TextError::Closed);
            }
            let mut profiles = owner
                .profile
                .lock()
                .map_err(|_| TextError::Worker("profile lock poisoned".into()))?;
            if profiles.requested.get(&request.profile) != Some(&initialization) {
                return Err(TextError::Profile("initialization superseded".into()));
            }
            profiles.published.insert(
                request.profile,
                Arc::new(Profile {
                    model,
                    dictionary,
                    terminology,
                    revision,
                }),
            );
            Ok(ready)
        })
        .await
    }

    /// Evaluate one occurrence against exactly the initialized profile revision.
    pub async fn correct(&self, request: CorrectRequest) -> Result<CorrectionDecision, TextError> {
        let profile = self
            .inner
            .profile
            .lock()
            .map_err(|_| TextError::Worker("profile lock poisoned".into()))?
            .published
            .get(&request.profile)
            .cloned()
            .ok_or_else(|| TextError::Profile(request.profile.as_str().into()))?;
        if request.profile_revision != profile.revision {
            return Err(TextError::Profile("profile revision changed".into()));
        }
        let owner = self.inner.clone();
        self.execute(move || {
            profile.model.correct(
                request,
                &profile.terminology,
                Some(&profile.dictionary),
                &owner.closed,
            )
        })
        .await
    }

    /// Stop admission and invalidate work without detaching its owning worker.
    pub fn close(&self) {
        self.inner.closed.store(true, Ordering::Release);
        self.inner.worker.close();
    }

    /// Wait for owned worker completion after stopping new work.
    pub async fn shutdown(&self, duration: Duration) -> Result<(), TextError> {
        self.close();
        tokio::time::timeout(duration, async {
            loop {
                let changed = self.inner.idle.notified();
                tokio::pin!(changed);
                changed.as_mut().enable();
                if self.inner.active.load(Ordering::Acquire) == 0 {
                    return;
                }
                changed.await;
            }
        })
        .await
        .map_err(|_| TextError::Worker("local text workers did not stop".into()))
    }

    /// Run CPU and filesystem work off the host input loop while retaining its permit until completion.
    async fn execute<Value: Send + 'static>(
        &self,
        work: impl FnOnce() -> Result<Value, TextError> + Send + 'static,
    ) -> Result<Value, TextError> {
        let permit = self
            .inner
            .worker
            .clone()
            .acquire_owned()
            .await
            .map_err(|_| TextError::Closed)?;
        if self.inner.closed.load(Ordering::Acquire) {
            return Err(TextError::Closed);
        }
        self.inner.active.fetch_add(1, Ordering::AcqRel);
        let guard = WorkerGuard {
            state: self.inner.clone(),
        };
        tokio::task::spawn_blocking(move || {
            let _permit = permit;
            let _guard = guard;
            work()
        })
        .await
        .map_err(|error| TextError::Worker(error.to_string()))?
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::contract::{ByteRange, OccurrenceId};

    #[tokio::test]
    async fn shutdown_owns_abandoned_workers() {
        let service = TextCorrectionService::new();
        let entered = Arc::new(Notify::new());
        let worker_entered = entered.clone();
        let owner = service.inner.clone();
        let worker_service = service.clone();
        let request = tokio::spawn(async move {
            worker_service
                .execute(move || {
                    worker_entered.notify_one();
                    while !owner.closed.load(Ordering::Acquire) {
                        std::thread::yield_now();
                    }
                    Ok(())
                })
                .await
        });
        entered.notified().await;
        request.abort();
        service.shutdown(Duration::from_secs(2)).await.unwrap();
        assert_eq!(service.inner.active.load(Ordering::Acquire), 0);
    }

    /// Build one lexical query with the caller's published identity.
    fn request(profile: &ProfileReady, context: &str, target: &str) -> CorrectRequest {
        let start = context.find(target).unwrap();
        CorrectRequest {
            profile: profile.profile.clone(),
            profile_revision: profile.profile_revision.clone(),
            occurrence: OccurrenceId::try_from("test".to_owned()).unwrap(),
            context: context.into(),
            target: ByteRange {
                start,
                end: start + target.len(),
            },
            protected: Vec::new(),
        }
    }

    #[tokio::test]
    async fn profile_reload_isolation_and_shutdown() {
        let service = TextCorrectionService::new();
        let profile = ProfileId::try_from("project-a".to_owned()).unwrap();
        let ready = service
            .initialize(InitializeRequest {
                profile: profile.clone(),
                terminology: vec!["tokio".into(), "teh".into(), "repo".into()],
                assets: None,
            })
            .await
            .unwrap();
        assert!(matches!(
            service
                .correct(request(&ready, "Use tokio here.", "tokio"))
                .await
                .unwrap(),
            CorrectionDecision::Unchanged { .. }
        ));
        for (context, target) in [("Use teh word here.", "teh"), ("Use repo here.", "repo")] {
            assert!(matches!(
                service
                    .correct(request(&ready, context, target))
                    .await
                    .unwrap(),
                CorrectionDecision::Unchanged { .. }
            ));
        }
        assert!(
            service
                .initialize(InitializeRequest {
                    profile: profile.clone(),
                    terminology: vec!["not a word".into()],
                    assets: None
                })
                .await
                .is_err()
        );
        assert!(matches!(
            service
                .correct(request(&ready, "Use tokio here.", "tokio"))
                .await
                .unwrap(),
            CorrectionDecision::Unchanged { .. }
        ));
        let other = service
            .initialize(InitializeRequest {
                profile: ProfileId::try_from("project-b".to_owned()).unwrap(),
                terminology: Vec::new(),
                assets: None,
            })
            .await
            .unwrap();
        assert_ne!(ready.profile_revision, other.profile_revision);
        let mut invalid = request(&ready, "é recieve", "recieve");
        invalid.target.start = 1;
        assert!(service.correct(invalid).await.is_err());
        let mut protected = request(&ready, "Please recieve", "recieve");
        protected.protected.push(protected.target.clone());
        assert!(service.correct(protected).await.is_err());
        service.shutdown(Duration::from_secs(2)).await.unwrap();
        assert!(matches!(
            service
                .correct(request(&ready, "Please recieve", "recieve"))
                .await,
            Err(TextError::Closed)
        ));
    }
}

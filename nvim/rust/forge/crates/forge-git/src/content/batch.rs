use super::*;

/// Shares conversion preparation across bounded reads. Call `verify` before publishing results.
pub(crate) struct ContentBatch {
    local: gix::Repository,
    repository: RepositoryIdentity,
    session: Option<conversion::ConversionSession>,
    pub(crate) preparation_us: u128,
    pub(crate) source_reads: usize,
}

impl ContentBatch {
    pub(crate) fn new(local: &gix::Repository, repository: &RepositoryIdentity) -> Self {
        Self {
            local: local.clone(),
            repository: repository.clone(),
            session: None,
            preparation_us: 0,
            source_reads: 0,
        }
    }

    pub(crate) fn acquire(
        &mut self,
        source: ContentSource,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<ContentResult> {
        self.source_reads += 1;
        match source {
            ContentSource::Object(object) => read_object(
                &self.local,
                object,
                ContentLimits::default(),
                None,
                ContentOrigin::Object(object),
                None,
                check,
            ),
            ContentSource::Worktree { path, conversion } => {
                if conversion == WorktreeConversion::GitCanonical && self.session.is_none() {
                    let started = Instant::now();
                    self.session = Some(conversion::ConversionSession::new(
                        &mut self.local,
                        &self.repository,
                        check,
                    )?);
                    self.preparation_us += started.elapsed().as_micros();
                }
                let result = read_worktree(
                    &mut self.local,
                    &self.repository,
                    WorktreeRead {
                        path: path.clone(),
                        conversion,
                        limits: ContentLimits::default(),
                        expected: None,
                        session: self.session.as_mut(),
                    },
                    None,
                    check,
                )?;
                if matches!(result, ContentResult::Missing) {
                    return Ok(ContentResult::Ready(FileContent {
                        source: SourceVersion::new(Vec::new(), Representation::GitCanonical)?,
                        origin: ContentOrigin::Worktree {
                            worktree: self
                                .repository
                                .worktree
                                .clone()
                                .context("statistics require a worktree")?,
                            path,
                            stamp: WorktreeStamp::Missing,
                            conversion: None,
                        },
                    }));
                }
                Ok(result)
            }
            _ => anyhow::bail!("statistics require an immutable object or worktree source"),
        }
    }

    pub(crate) fn verify(&mut self, check: &mut dyn FnMut() -> Result<()>) -> Result<()> {
        if let Some(session) = &self.session {
            session.verify(&mut self.local, &self.repository, check)?;
        }
        check()
    }
}

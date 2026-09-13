use std::path::PathBuf;

use super::{GhClient, classify_failure, diagnostic, remote_result};
use crate::model::GithubRepositoryId;
use crate::remote::{RemoteActor, RemoteFailure, RemoteFailureKind};

impl GhClient {
    pub(super) async fn read_actor_at(
        &self,
        directory: PathBuf,
        repository: GithubRepositoryId,
    ) -> Result<RemoteActor, RemoteFailure> {
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            repository.hostname().into(),
            "user".into(),
        ];
        remote_result(
            self.run_native(directory, argument, None, 1024, |output| {
                if !output.status.success() {
                    return Err(classify_failure(diagnostic(&String::from_utf8_lossy(
                        if output.stderr.is_empty() {
                            &output.stdout
                        } else {
                            &output.stderr
                        },
                    ))));
                }
                let actor: RemoteActor =
                    serde_json::from_slice(&output.stdout).map_err(|error| RemoteFailure {
                        kind: RemoteFailureKind::InvalidResponse,
                        message: error.to_string(),
                    })?;
                if actor.login.is_empty()
                    || actor.login.len() > 256
                    || actor.node_id.is_empty()
                    || actor.node_id.len() > 256
                {
                    return Err(RemoteFailure {
                        kind: RemoteFailureKind::InvalidResponse,
                        message: "authenticated actor identity is invalid".into(),
                    });
                }
                Ok(actor)
            })
            .await,
        )
    }
}

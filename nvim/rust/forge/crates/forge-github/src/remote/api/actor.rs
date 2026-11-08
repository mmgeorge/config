use super::{ApiRequest, GithubClient, remote_result};
use crate::model::GithubRepositoryId;
use crate::remote::{RemoteActor, RemoteFailure, RemoteFailureKind};

impl GithubClient {
    pub(super) async fn read_actor_at(
        &self,
        repository: GithubRepositoryId,
    ) -> Result<RemoteActor, RemoteFailure> {
        remote_result(
            self.send(ApiRequest::get(repository.hostname(), "user"), |response| {
                let actor: RemoteActor = response.into_json()?;
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

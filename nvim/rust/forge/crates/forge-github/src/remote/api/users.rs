use std::collections::BTreeMap;

use serde::Deserialize;

use super::{ApiRequest, ApiResponse, GithubClient, append_diagnostic, diagnostic, remote_result};
use crate::metadata::{RepositoryUser, RepositoryUsers, validate_user};
use crate::model::GithubRepositoryId;
use crate::remote::{RemoteFailure, RemoteFailureKind};

#[derive(Deserialize)]
struct UserResponse {
    login: String,
    name: Option<String>,
}

impl GithubClient {
    pub(super) async fn read_users(
        &self,
        repository: GithubRepositoryId,
    ) -> Result<RepositoryUsers, RemoteFailure> {
        let mut user_map: BTreeMap<String, RepositoryUser> = BTreeMap::new();
        let mut failure = Vec::new();
        let mut successful = 0;
        for source in ["contributors", "collaborators"] {
            let request = ApiRequest::get(
                repository.hostname(),
                format!(
                    "repos/{}/{source}?per_page=100",
                    repository.repository_name()
                ),
            )
            .all_pages();
            match remote_result(self.send(request, decode_users).await) {
                Ok(users) => {
                    successful += 1;
                    for user in users {
                        let key = user.login.to_lowercase();
                        if let Some(existing) = user_map.get_mut(&key) {
                            if existing.name.is_none() {
                                existing.name = user.name;
                            }
                        } else {
                            user_map.insert(key, user);
                        }
                    }
                }
                Err(mut error) => {
                    error.message = diagnostic(&format!("{source}: {}", error.message));
                    failure.push(error);
                }
            }
        }
        if successful == 0 {
            let mut combined = failure.remove(0);
            for error in failure {
                append_diagnostic(&mut combined.message, &error.to_string());
            }
            return Err(combined);
        }
        Ok(RepositoryUsers {
            contributors: user_map.into_values().collect(),
            failure,
        })
    }
}

fn decode_users(response: ApiResponse) -> Result<Vec<RepositoryUser>, RemoteFailure> {
    let pages: Vec<Vec<UserResponse>> =
        serde_json::from_slice(&response.into_success()?).map_err(|failure| {
            invalid(&format!(
                "GitHub returned invalid repository user JSON: {failure}"
            ))
        })?;
    let mut users = Vec::new();
    for page in pages {
        for user in page {
            let user = RepositoryUser {
                login: user.login,
                name: user.name,
            };
            validate_user(&user).map_err(|failure| invalid(&failure.to_string()))?;
            users.push(user);
        }
    }
    Ok(users)
}

fn invalid(message: &str) -> RemoteFailure {
    RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: diagnostic(message),
    }
}

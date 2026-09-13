use std::collections::BTreeMap;
use std::path::PathBuf;
use std::process::Output;

use serde::Deserialize;

use super::{BoundedVec, GhClient, append_diagnostic, classify_failure, diagnostic, remote_result};
use crate::metadata::{MAX_USERS, RepositoryUser, RepositoryUsers, validate_user};
use crate::model::GithubRepositoryId;
use crate::remote::{RemoteFailure, RemoteFailureKind};

#[derive(Deserialize)]
struct UserResponse {
    login: String,
    name: Option<String>,
}

impl GhClient {
    pub(super) async fn read_users(
        &self,
        directory: PathBuf,
        repository: GithubRepositoryId,
    ) -> Result<RepositoryUsers, RemoteFailure> {
        let mut user_map: BTreeMap<String, RepositoryUser> = BTreeMap::new();
        let mut failure = Vec::new();
        let mut successful = 0;
        for source in ["contributors", "collaborators"] {
            let argument = vec![
                "api".into(),
                "--hostname".into(),
                repository.hostname().into(),
                format!(
                    "/repos/{}/{source}?per_page=100",
                    repository.repository_name()
                )
                .into(),
                "--paginate".into(),
                "--slurp".into(),
            ];
            match remote_result(
                self.run_native(directory.clone(), argument, None, 0, decode_users)
                    .await,
            ) {
                Ok(users) => {
                    successful += 1;
                    for user in users {
                        let key = user.login.to_lowercase();
                        if let Some(existing) = user_map.get_mut(&key) {
                            if existing.name.is_none() {
                                existing.name = user.name;
                            }
                        } else {
                            if user_map.len() == MAX_USERS {
                                return Err(invalid("repository users exceed their record limit"));
                            }
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

fn decode_users(output: Output) -> Result<Vec<RepositoryUser>, RemoteFailure> {
    if !output.status.success() {
        let mut message = diagnostic(&format!(
            "gh exited {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
        append_diagnostic(&mut message, &String::from_utf8_lossy(&output.stdout));
        return Err(classify_failure(message));
    }
    let pages: BoundedVec<BoundedVec<UserResponse, 100>, 1000> =
        serde_json::from_slice(&output.stdout).map_err(|failure| {
            invalid(&format!(
                "gh returned invalid repository user JSON: {failure}"
            ))
        })?;
    let mut users = Vec::new();
    for page in pages.0 {
        for user in page.0 {
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

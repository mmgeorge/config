use std::{future::Future, path::PathBuf, pin::Pin};

use super::{GhClient, GhDirectory, classify_failure, diagnostic, remote_result};
use crate::notification::{
    GithubNotificationRemote, NotificationDetail, NotificationDetailRequest, NotificationPage,
    NotificationReadRequest, decode_detail, decode_page,
};
use crate::remote::{RemoteFailure, RemoteFailureKind};

impl GithubNotificationRemote for GhDirectory {
    fn notification_detail(
        &self,
        request: NotificationDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<NotificationDetail, RemoteFailure>> + Send + '_>> {
        Box::pin(
            self.client
                .notification_detail_at(self.directory.clone(), request),
        )
    }

    /// Reads one host-wide participating page through the shared native process admission.
    fn notification_page(
        &self,
        request: NotificationReadRequest,
    ) -> Pin<Box<dyn Future<Output = Result<NotificationPage, RemoteFailure>> + Send + '_>> {
        Box::pin(
            self.client
                .notification_page_at(self.directory.clone(), request),
        )
    }
}

impl GhClient {
    async fn notification_detail_at(
        &self,
        directory: PathBuf,
        request: NotificationDetailRequest,
    ) -> Result<NotificationDetail, RemoteFailure> {
        let endpoint = request.endpoint().map_err(|failure| RemoteFailure {
            kind: RemoteFailureKind::InvalidResponse,
            message: failure.to_string(),
        })?;
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            request.record.repository.hostname().into(),
            endpoint.into(),
        ];
        remote_result(
            self.run_native(directory, argument, None, 4096, move |output| {
                if !output.status.success() {
                    return Err(classify_failure(diagnostic(&String::from_utf8_lossy(
                        if output.stderr.is_empty() {
                            &output.stdout
                        } else {
                            &output.stderr
                        },
                    ))));
                }
                decode_detail(&output.stdout, &request).map_err(|failure| RemoteFailure {
                    kind: RemoteFailureKind::InvalidResponse,
                    message: failure.to_string(),
                })
            })
            .await,
        )
    }

    async fn notification_page_at(
        &self,
        directory: PathBuf,
        mut request: NotificationReadRequest,
    ) -> Result<NotificationPage, RemoteFailure> {
        request.validate().map_err(|failure| RemoteFailure {
            kind: RemoteFailureKind::InvalidResponse,
            message: failure.to_string(),
        })?;
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            request.hostname.clone().into(),
            request.endpoint().into(),
        ];
        remote_result(
            self.run_native(directory, argument, None, 4096, move |output| {
                if !output.status.success() {
                    return Err(classify_failure(diagnostic(&String::from_utf8_lossy(
                        if output.stderr.is_empty() {
                            &output.stdout
                        } else {
                            &output.stderr
                        },
                    ))));
                }
                decode_page(&output.stdout, &request).map_err(|failure| RemoteFailure {
                    kind: RemoteFailureKind::InvalidResponse,
                    message: failure.to_string(),
                })
            })
            .await,
        )
    }
}

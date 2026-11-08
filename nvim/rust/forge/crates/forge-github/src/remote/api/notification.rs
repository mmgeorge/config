use std::{future::Future, pin::Pin};

use super::{ApiRequest, GithubClient, remote_result};
use crate::notification::{
    GithubNotificationRemote, NotificationDetail, NotificationDetailRequest, NotificationPage,
    NotificationReadRequest, decode_detail, decode_page,
};
use crate::remote::{RemoteFailure, RemoteFailureKind};

impl GithubNotificationRemote for GithubClient {
    fn notification_detail(
        &self,
        request: NotificationDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<NotificationDetail, RemoteFailure>> + Send + '_>> {
        Box::pin(self.notification_detail_at(request))
    }

    /// Reads one host-wide participating page through the shared request admission.
    fn notification_page(
        &self,
        request: NotificationReadRequest,
    ) -> Pin<Box<dyn Future<Output = Result<NotificationPage, RemoteFailure>> + Send + '_>> {
        Box::pin(self.notification_page_at(request))
    }
}

impl GithubClient {
    async fn notification_detail_at(
        &self,
        request: NotificationDetailRequest,
    ) -> Result<NotificationDetail, RemoteFailure> {
        let endpoint = request.endpoint().map_err(|failure| RemoteFailure {
            kind: RemoteFailureKind::InvalidResponse,
            message: failure.to_string(),
        })?;
        let api_request = ApiRequest::get(request.record.repository.hostname(), endpoint);
        remote_result(
            self.send(api_request, move |response| {
                decode_detail(&response.into_success()?, &request).map_err(|failure| {
                    RemoteFailure {
                        kind: RemoteFailureKind::InvalidResponse,
                        message: failure.to_string(),
                    }
                })
            })
            .await,
        )
    }

    async fn notification_page_at(
        &self,
        mut request: NotificationReadRequest,
    ) -> Result<NotificationPage, RemoteFailure> {
        request.validate().map_err(|failure| RemoteFailure {
            kind: RemoteFailureKind::InvalidResponse,
            message: failure.to_string(),
        })?;
        let api_request = ApiRequest::get(&request.hostname, request.endpoint());
        remote_result(
            self.send(api_request, move |response| {
                decode_page(&response.into_success()?, &request).map_err(|failure| RemoteFailure {
                    kind: RemoteFailureKind::InvalidResponse,
                    message: failure.to_string(),
                })
            })
            .await,
        )
    }
}

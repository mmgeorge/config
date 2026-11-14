use std::{
	future::Future,
	io,
	pin::Pin,
	process::ExitStatus,
	sync::Arc,
	task::{Context, Poll},
};

use tokio::task::JoinHandle;

/// Retain descendant collection independently of any one caller's wait future.
#[derive(Debug)]
pub(super) enum Collection {
	Waiting(JoinHandle<io::Result<ExitStatus>>),
	Finished(ExitStatus),
	Failed(Arc<io::Error>),
}

impl Collection {
	/// Start one collector whose ownership survives cancellation of a waiter.
	pub(super) fn start(task: JoinHandle<io::Result<ExitStatus>>) -> Self {
		Self::Waiting(task)
	}

	/// Observe the retained collector without treating root exit as tree completion.
	pub(super) fn try_wait(&mut self) -> io::Result<Option<ExitStatus>> {
		match self.poll(&mut Context::from_waker(futures::task::noop_waker_ref())) {
			Poll::Ready(result) => result.map(Some),
			Poll::Pending => Ok(None),
		}
	}

	/// Register a waiter on the same collector after any earlier waiter was dropped.
	pub(super) fn poll(&mut self, context: &mut Context<'_>) -> Poll<io::Result<ExitStatus>> {
		let result = match self {
			Self::Waiting(task) => match Pin::new(task).poll(context) {
				Poll::Pending => return Poll::Pending,
				Poll::Ready(result) => result.map_err(io::Error::other).and_then(|result| result),
			},
			Self::Finished(status) => return Poll::Ready(Ok(*status)),
			Self::Failed(error) => {
				return Poll::Ready(Err(io::Error::new(error.kind(), error.clone())));
			}
		};
		*self = match result {
			Ok(status) => Self::Finished(status),
			Err(error) => Self::Failed(Arc::new(error)),
		};
		self.poll(context)
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use std::future::poll_fn;
	use tokio::sync::oneshot;

	/// Cancelling one waiter must neither complete nor duplicate descendant collection.
	#[tokio::test]
	async fn cancelled_wait_retains_collection_until_descendants_finish() {
		let (finish, finished) = oneshot::channel();
		let task = tokio::spawn(async {
			finished.await.unwrap();
			Ok(success())
		});
		let mut collection = Collection::start(task);
		{
			let wait = poll_fn(|context| collection.poll(context));
			tokio::pin!(wait);
			assert!(futures::poll!(&mut wait).is_pending());
		}
		assert!(collection.try_wait().unwrap().is_none());
		finish.send(()).unwrap();
		assert!(
			poll_fn(|context| collection.poll(context))
				.await
				.unwrap()
				.success()
		);
		assert!(collection.try_wait().unwrap().unwrap().success());
	}

	/// A failed collector remains failed on both polling and a later waiter.
	#[tokio::test]
	async fn collection_failure_preserves_its_cause_on_retry() {
		let mut collection = Collection::start(tokio::spawn(async {
			Err(io::Error::new(
				io::ErrorKind::PermissionDenied,
				"collection denied",
			))
		}));
		let error = poll_fn(|context| collection.poll(context))
			.await
			.unwrap_err();
		assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);
		assert_eq!(error.to_string(), "collection denied");
		let error = collection.try_wait().unwrap_err();
		assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);
		assert_eq!(error.to_string(), "collection denied");
	}

	/// Construct a successful platform status without starting a process.
	fn success() -> ExitStatus {
		#[cfg(unix)]
		use std::os::unix::process::ExitStatusExt;
		#[cfg(windows)]
		use std::os::windows::process::ExitStatusExt;
		ExitStatus::from_raw(0)
	}
}

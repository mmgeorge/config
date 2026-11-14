# Forge process collection patch

Source: `process-wrap` 10.0.1 from crates.io, dual licensed under the included
Apache-2.0 and MIT licenses. `Cargo.toml.orig` retains the upstream manifest.

Tokio job/process-group waits previously stored the root process's exit status
before descendant collection completed. Dropping that wait future could leave
its blocking collector active; a second wait returned the cached root status
without observing collection. Forge uses cancellation and deadlines around these
waits, so that behavior could release repository access while collection remained
unfinished.

The Tokio wrappers now retain one collection task and its terminal result across
wait cancellation. Both `wait` and `try_wait` observe that same task. A cached
root exit cannot bypass the collector. Collection failures remain failures on retry.
On Windows the collector queries the job's active-process count instead of
accepting an arbitrary completion-port notification. It owns a duplicated job
handle until collection ends; dropping the child wrapper stops that collector so
the duplicate cannot indefinitely delay the job's kill-on-close behavior.
The [Windows job documentation](https://learn.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-jobobject_associate_completion_port)
states that job notifications are not guaranteed; polling the job's accounting
avoids both unrelated notifications and a missed notification.

The vendored manifest omits upstream subprocess integration targets and their
`remoteprocess` dependency. Forge exercises real process trees through its
Nushell-driven Harness integration suite; the new collection-state tests do not
launch processes. This patched dependency is a workspace member so those tests
run against the same dependency lockfile as Forge. This patch does not supply
crash-safe launch registration,
escaped-descendant containment, or recovery after host death.
The Unix collector still has upstream `waitpid` scope; it does not prove that
reparented or escaped descendants have exited.

Forge's Windows launchers use the added `JobObject::spawn` entry point. It creates
the kill-on-close job before spawning and supplies `PROC_THREAD_ATTRIBUTE_JOB_LIST`
through Rust's process attributes, eliminating the separate suspended-spawn and
job-assignment calls. The job handle is non-inheritable. The compatibility wrapper
still uses the upstream assignment sequence; Forge's shell and Git launchers no
longer use that wrapper on Windows.

This entry point requires the workspace's pinned nightly Rust and Windows
`--cfg tokio_unstable` flag for Tokio's `Command::spawn_with` hook. The
[Windows attribute reference](https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-updateprocthreadattribute)
defines the job-list attribute and its handle-array lifetime. Creation-time
assignment does not supply durable registration, reconciliation, or proof about
every interruption inside the native creation call. Those remain part of the
repository journal work.

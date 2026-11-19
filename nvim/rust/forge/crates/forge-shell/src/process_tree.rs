//! Identify and stop the processes a native shell command left behind its own wrapper.
//!
//! A provider that runs each command through a wrapper process, such as Copilot's per-command
//! `pwsh -Command`, can stop the wrapper while the command's own processes keep running with a dead
//! parent. Neither Windows nor macOS reparents them in a way that keeps the tree findable later, so
//! the caller captures the tree while the wrapper is alive and stops what survives afterwards.
//!
//! A process id alone is not an identity: the operating system reuses ids, so a captured id may
//! name an unrelated process by the time it is stopped. Every identity here pairs the id with the
//! process creation time, and a process is stopped only when both still match.

use std::collections::{HashMap, HashSet};
use std::io;

/// One process, distinguished from any later process that reuses its id.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ProcessIdentity {
    pub id: u32,
    /// Creation time in the platform's monotonic-enough clock units, comparable only within one
    /// machine: 100 ns FILETIME ticks on Windows, microseconds since the epoch on macOS.
    pub started: u64,
}

/// One row of a process table snapshot.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProcessRecord {
    pub identity: ProcessIdentity,
    pub parent_id: u32,
}

/// Select the live descendants of one command wrapper, deepest first.
///
/// The wrapper itself is excluded: the provider that launched it owns its termination and exit
/// status, and stopping it here would race that. The selection is empty unless the wrapper is a
/// grandchild of `owner_id` through a parent that `owner_id` launched, which is how a provider
/// runtime launched by this process runs its commands. That check rejects a wrapper id that an
/// unrelated process reused after the wrapper exited, and it rejects a provider that reports its
/// own runtime's id, whose descendants would include every other command.
///
/// A row counts as a child only when its creation time is not earlier than its parent's: a
/// process whose recorded parent id was reused by a younger process is not that process's child.
pub fn command_descendants(
    table: &[ProcessRecord],
    wrapper_id: u32,
    owner_id: u32,
) -> Vec<ProcessIdentity> {
    let by_id = table
        .iter()
        .map(|record| (record.identity.id, record))
        .collect::<HashMap<_, _>>();
    let Some(wrapper) = by_id.get(&wrapper_id) else {
        return Vec::new();
    };
    let launcher_owned = by_id.get(&wrapper.parent_id).is_some_and(|launcher| {
        launcher.parent_id == owner_id
            && launcher.identity.id != owner_id
            && wrapper.identity.started >= launcher.identity.started
    });
    if !launcher_owned {
        return Vec::new();
    }
    let mut children = HashMap::<u32, Vec<&ProcessRecord>>::new();
    for record in table {
        children.entry(record.parent_id).or_default().push(record);
    }
    let mut selected = Vec::new();
    let mut visited = HashSet::from([wrapper.identity.id]);
    let mut frontier = vec![(wrapper.identity, 0_usize)];
    while let Some((parent, depth)) = frontier.pop() {
        for child in children.get(&parent.id).into_iter().flatten() {
            if child.identity.started < parent.started || !visited.insert(child.identity.id) {
                continue;
            }
            selected.push((child.identity, depth + 1));
            frontier.push((child.identity, depth + 1));
        }
    }
    selected.sort_by_key(|(_, depth)| std::cmp::Reverse(*depth));
    selected.into_iter().map(|(identity, _)| identity).collect()
}

/// Read the current process table, omitting processes this user cannot inspect.
pub fn snapshot() -> io::Result<Vec<ProcessRecord>> {
    platform::snapshot()
}

/// Stop one process only if its id still names the same process.
///
/// Returns whether this call stopped it; a process that already exited, or whose id now names a
/// different process, is left alone and reported as not stopped.
pub fn terminate(identity: ProcessIdentity) -> io::Result<bool> {
    platform::terminate(identity)
}

#[cfg(windows)]
mod platform {
    use super::{ProcessIdentity, ProcessRecord};
    use std::io;
    use windows::Win32::Foundation::{CloseHandle, FILETIME, HANDLE};
    use windows::Win32::System::Diagnostics::ToolHelp::{
        CreateToolhelp32Snapshot, PROCESSENTRY32W, Process32FirstW, Process32NextW,
        TH32CS_SNAPPROCESS,
    };
    use windows::Win32::System::Threading::{
        GetProcessTimes, OpenProcess, PROCESS_ACCESS_RIGHTS, PROCESS_QUERY_LIMITED_INFORMATION,
        PROCESS_TERMINATE, TerminateProcess,
    };

    /// Close one kernel handle when its owner leaves scope.
    struct OwnedHandle(HANDLE);

    impl Drop for OwnedHandle {
        fn drop(&mut self) {
            // SAFETY: the handle came from a successful open and is closed exactly once.
            let _ = unsafe { CloseHandle(self.0) };
        }
    }

    /// Open a process and read its creation time, or report that it cannot be inspected.
    fn open(id: u32, access: PROCESS_ACCESS_RIGHTS) -> Option<(OwnedHandle, u64)> {
        // SAFETY: OpenProcess has no memory preconditions; failure returns an error.
        let handle = OwnedHandle(
            unsafe { OpenProcess(access | PROCESS_QUERY_LIMITED_INFORMATION, false, id) }.ok()?,
        );
        let mut created = FILETIME::default();
        let mut exited = FILETIME::default();
        let mut kernel = FILETIME::default();
        let mut user = FILETIME::default();
        // SAFETY: every output pointer refers to a live local FILETIME.
        unsafe { GetProcessTimes(handle.0, &mut created, &mut exited, &mut kernel, &mut user) }
            .ok()?;
        let started = (u64::from(created.dwHighDateTime) << 32) | u64::from(created.dwLowDateTime);
        Some((handle, started))
    }

    pub(super) fn snapshot() -> io::Result<Vec<ProcessRecord>> {
        // SAFETY: a process snapshot has no memory preconditions; failure returns an error.
        let table = OwnedHandle(
            unsafe { CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0) }.map_err(io::Error::other)?,
        );
        let mut entry = PROCESSENTRY32W {
            dwSize: size_of::<PROCESSENTRY32W>() as u32,
            ..Default::default()
        };
        let mut records = Vec::new();
        // SAFETY: `entry` is a live PROCESSENTRY32W whose dwSize is initialized.
        let mut more = unsafe { Process32FirstW(table.0, &mut entry) }.is_ok();
        while more {
            if let Some((_, started)) = open(entry.th32ProcessID, PROCESS_ACCESS_RIGHTS(0)) {
                records.push(ProcessRecord {
                    identity: ProcessIdentity {
                        id: entry.th32ProcessID,
                        started,
                    },
                    parent_id: entry.th32ParentProcessID,
                });
            }
            // SAFETY: as above; the snapshot handle stays open for the whole walk.
            more = unsafe { Process32NextW(table.0, &mut entry) }.is_ok();
        }
        Ok(records)
    }

    pub(super) fn terminate(identity: ProcessIdentity) -> io::Result<bool> {
        let Some((handle, started)) = open(identity.id, PROCESS_TERMINATE) else {
            return Ok(false);
        };
        if started != identity.started {
            return Ok(false);
        }
        // SAFETY: the handle holds PROCESS_TERMINATE for the process whose identity matched, and
        // an open handle keeps that id from being reused until it closes.
        unsafe { TerminateProcess(handle.0, 1) }.map_err(io::Error::other)?;
        Ok(true)
    }
}

#[cfg(target_os = "macos")]
mod platform {
    use super::{ProcessIdentity, ProcessRecord};
    use std::io;

    /// Read one process's parent and start time, or report that it cannot be inspected.
    fn inspect(id: libc::pid_t) -> Option<ProcessRecord> {
        let mut info = std::mem::MaybeUninit::<libc::proc_bsdinfo>::zeroed();
        let size = size_of::<libc::proc_bsdinfo>() as libc::c_int;
        // SAFETY: the buffer is a live proc_bsdinfo of exactly `size` bytes.
        let written = unsafe {
            libc::proc_pidinfo(
                id,
                libc::PROC_PIDTBSDINFO,
                0,
                info.as_mut_ptr().cast(),
                size,
            )
        };
        if written != size {
            return None;
        }
        // SAFETY: proc_pidinfo filled the whole structure.
        let info = unsafe { info.assume_init() };
        Some(ProcessRecord {
            identity: ProcessIdentity {
                id: u32::try_from(id).ok()?,
                started: info
                    .pbi_start_tvsec
                    .saturating_mul(1_000_000)
                    .saturating_add(info.pbi_start_tvusec),
            },
            parent_id: info.pbi_ppid,
        })
    }

    pub(super) fn snapshot() -> io::Result<Vec<ProcessRecord>> {
        // SAFETY: a null buffer asks only for the current process count.
        let count = unsafe { libc::proc_listallpids(std::ptr::null_mut(), 0) };
        if count < 0 {
            return Err(io::Error::last_os_error());
        }
        // Processes may start between the two calls, so the buffer leaves room for them.
        let mut ids = vec![0 as libc::pid_t; count as usize * 2 + 64];
        let bytes = libc::c_int::try_from(ids.len() * size_of::<libc::pid_t>())
            .map_err(io::Error::other)?;
        // SAFETY: the buffer is live and `bytes` long.
        let filled = unsafe { libc::proc_listallpids(ids.as_mut_ptr().cast(), bytes) };
        if filled < 0 {
            return Err(io::Error::last_os_error());
        }
        ids.truncate(filled as usize);
        Ok(ids
            .into_iter()
            .filter(|id| *id > 0)
            .filter_map(inspect)
            .collect())
    }

    pub(super) fn terminate(identity: ProcessIdentity) -> io::Result<bool> {
        let Ok(id) = libc::pid_t::try_from(identity.id) else {
            return Ok(false);
        };
        if inspect(id).is_none_or(|record| record.identity != identity) {
            return Ok(false);
        }
        // SAFETY: kill has no memory preconditions. The identity matched immediately before, so
        // only a reuse inside that interval could redirect the signal.
        if unsafe { libc::kill(id, libc::SIGKILL) } == 0 {
            return Ok(true);
        }
        let error = io::Error::last_os_error();
        if error.raw_os_error() == Some(libc::ESRCH) {
            Ok(false)
        } else {
            Err(error)
        }
    }
}

#[cfg(not(any(windows, target_os = "macos")))]
mod platform {
    use super::{ProcessIdentity, ProcessRecord};
    use std::io;

    pub(super) fn snapshot() -> io::Result<Vec<ProcessRecord>> {
        Err(io::Error::from(io::ErrorKind::Unsupported))
    }

    pub(super) fn terminate(_identity: ProcessIdentity) -> io::Result<bool> {
        Err(io::Error::from(io::ErrorKind::Unsupported))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const OWNER: u32 = 10;
    const RUNTIME: u32 = 20;
    const WRAPPER: u32 = 30;

    fn row(id: u32, started: u64, parent_id: u32) -> ProcessRecord {
        ProcessRecord {
            identity: ProcessIdentity { id, started },
            parent_id,
        }
    }

    fn identity(id: u32, started: u64) -> ProcessIdentity {
        ProcessIdentity { id, started }
    }

    /// Owner, its provider runtime, and one command wrapper, as a provider launches a command.
    fn launched() -> Vec<ProcessRecord> {
        vec![
            row(OWNER, 1, 1),
            row(RUNTIME, 2, OWNER),
            row(WRAPPER, 5, RUNTIME),
        ]
    }

    /// The wrapper's whole subtree is selected, deepest first, and the wrapper itself is not.
    #[test]
    fn selects_wrapper_descendants_deepest_first() {
        let mut table = launched();
        table.extend([
            row(40, 6, WRAPPER),
            row(50, 7, 40),
            row(60, 8, 50),
            row(41, 6, WRAPPER),
            // A sibling command of the same runtime is not part of this command.
            row(31, 5, RUNTIME),
            row(70, 9, 31),
        ]);
        let selected = command_descendants(&table, WRAPPER, OWNER);
        assert_eq!(selected.len(), 4);
        assert_eq!(selected[0], identity(60, 8));
        assert_eq!(selected[1], identity(50, 7));
        assert!(selected[2..].contains(&identity(40, 6)));
        assert!(selected[2..].contains(&identity(41, 6)));
        assert!(!selected.iter().any(|selected| selected.id == WRAPPER));
    }

    /// A row older than its recorded parent belongs to an earlier holder of that id.
    #[test]
    fn excludes_rows_created_before_their_parent() {
        let mut table = launched();
        table.extend([row(40, 4, WRAPPER), row(50, 6, 40), row(41, 6, WRAPPER)]);
        assert_eq!(
            command_descendants(&table, WRAPPER, OWNER),
            vec![identity(41, 6)]
        );
    }

    /// Only a grandchild of the owner through a launcher it started can root a selection.
    #[test]
    fn rejects_wrappers_outside_the_owned_runtime() {
        let mut table = launched();
        table.extend([
            row(40, 6, WRAPPER),
            row(80, 3, 1),
            row(81, 4, 80),
            row(82, 5, 81),
        ]);
        // The runtime itself would select every command it runs.
        assert!(command_descendants(&table, RUNTIME, OWNER).is_empty());
        // An unrelated tree that reused the wrapper's id is not this command.
        assert!(command_descendants(&table, 81, OWNER).is_empty());
        // A wrapper that already exited selects nothing.
        assert!(command_descendants(&table, 99, OWNER).is_empty());
        assert_eq!(
            command_descendants(&table, WRAPPER, OWNER),
            vec![identity(40, 6)]
        );
    }
}

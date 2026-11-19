//! Recover process-tree ownership independently of a dead launcher's PID.

use std::io;

/// Retain an operation's named containment before any child may execute.
#[derive(Debug)]
pub struct ProcessContainment {
    #[cfg(windows)]
    handle: std::os::windows::io::OwnedHandle,
}

/// Distinguish proven collection from work that still prevents admission.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CollectionStatus {
    Collected,
    Pending,
    Unsupported,
}

impl ProcessContainment {
    /// Create an empty container whose identity the caller must persist before spawning.
    pub fn create(identity: &str) -> io::Result<Self> {
        #[cfg(windows)]
        {
            windows::create(identity)
        }
        #[cfg(not(windows))]
        {
            let _ = identity;
            Ok(Self {})
        }
    }

    /// Launch into registered containment before the child runs any instruction.
    pub fn spawn(
        &self,
        command: tokio::process::Command,
    ) -> io::Result<Box<dyn process_wrap::tokio::ChildWrapper>> {
        #[cfg(windows)]
        {
            use std::os::windows::io::AsHandle;
            crate::process::spawn_contained(command, Some(self.handle.as_handle()))
        }
        #[cfg(not(windows))]
        {
            crate::process::spawn_owned(command)
        }
    }

    /// Stop a dead owner's exact container and report collection without trusting its PID.
    pub fn recover(identity: &str) -> io::Result<CollectionStatus> {
        #[cfg(windows)]
        {
            windows::recover(identity)
        }
        #[cfg(not(windows))]
        {
            let _ = identity;
            Ok(CollectionStatus::Unsupported)
        }
    }

    /// Check the live container before releasing repository ownership.
    pub fn collected(&self) -> io::Result<bool> {
        #[cfg(windows)]
        {
            windows::collected(&self.handle)
        }
        #[cfg(not(windows))]
        {
            Ok(true)
        }
    }
}

#[cfg(windows)]
mod windows {
    use super::{CollectionStatus, ProcessContainment};
    use ::windows::{
        Win32::{
            Foundation::{ERROR_ALREADY_EXISTS, ERROR_FILE_NOT_FOUND, GetLastError, HANDLE},
            System::JobObjects::{
                CreateJobObjectW, JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
                JOBOBJECT_BASIC_ACCOUNTING_INFORMATION, JOBOBJECT_EXTENDED_LIMIT_INFORMATION,
                JobObjectBasicAccountingInformation, JobObjectExtendedLimitInformation,
                OpenJobObjectW, QueryInformationJobObject, SetInformationJobObject,
                TerminateJobObject,
            },
            System::SystemServices::{JOB_OBJECT_QUERY, JOB_OBJECT_TERMINATE},
        },
        core::PCWSTR,
    };
    use std::{
        io,
        os::windows::io::{AsRawHandle, FromRawHandle, OwnedHandle},
    };

    /// Derive a kernel name from a host-generated UUID, never a serialized arbitrary path.
    fn name(identity: &str) -> io::Result<Vec<u16>> {
        if identity.len() != 36
            || !identity
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() || byte == b'-')
        {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid process containment identity",
            ));
        }
        Ok(format!("Local\\ForgeRepository-{identity}\0")
            .encode_utf16()
            .collect())
    }

    /// Establish kill-on-close ownership before exposing an empty named job.
    pub(super) fn create(identity: &str) -> io::Result<ProcessContainment> {
        let name = name(identity)?;
        // SAFETY: name is terminated and remains valid for the call; security attributes are absent.
        let raw =
            unsafe { CreateJobObjectW(None, PCWSTR(name.as_ptr())) }.map_err(io::Error::other)?;
        // SAFETY: CreateJobObjectW returned a new owned handle.
        let handle = unsafe { OwnedHandle::from_raw_handle(raw.0) };
        // SAFETY: read the thread-local result immediately after CreateJobObjectW.
        if unsafe { GetLastError() } == ERROR_ALREADY_EXISTS {
            return Err(io::Error::new(
                io::ErrorKind::AlreadyExists,
                "process containment identity already exists",
            ));
        }
        let mut limits = JOBOBJECT_EXTENDED_LIMIT_INFORMATION::default();
        limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        // SAFETY: the job and correctly sized limits remain live during configuration.
        unsafe {
            SetInformationJobObject(
                raw,
                JobObjectExtendedLimitInformation,
                &limits as *const _ as _,
                std::mem::size_of_val(&limits)
                    .try_into()
                    .expect("job limits fit DWORD"),
            )
        }
        .map_err(io::Error::other)?;
        Ok(ProcessContainment { handle })
    }

    /// Collect the named job or prove that the kernel has already destroyed it.
    pub(super) fn recover(identity: &str) -> io::Result<CollectionStatus> {
        let name = name(identity)?;
        // SAFETY: name is terminated; the returned handle belongs exclusively to this recovery attempt.
        let raw = match unsafe {
            OpenJobObjectW(
                JOB_OBJECT_QUERY | JOB_OBJECT_TERMINATE,
                false,
                PCWSTR(name.as_ptr()),
            )
        } {
            Ok(handle) => handle,
            Err(error) if error.code() == ERROR_FILE_NOT_FOUND.to_hresult() => {
                return Ok(CollectionStatus::Collected);
            }
            Err(error) => return Err(io::Error::other(error)),
        };
        // SAFETY: OpenJobObjectW returned a new owned handle.
        let handle = unsafe { OwnedHandle::from_raw_handle(raw.0) };
        // SAFETY: the handle requests terminate rights and remains live through the query.
        unsafe { TerminateJobObject(raw, 1) }.map_err(io::Error::other)?;
        Ok(if collected(&handle)? {
            CollectionStatus::Collected
        } else {
            CollectionStatus::Pending
        })
    }

    /// Query the kernel's active count instead of treating root exit as tree exit.
    pub(super) fn collected(handle: &OwnedHandle) -> io::Result<bool> {
        let mut accounting = JOBOBJECT_BASIC_ACCOUNTING_INFORMATION::default();
        // SAFETY: the output buffer has the required layout and the query handle remains live.
        unsafe {
            QueryInformationJobObject(
                Some(HANDLE(handle.as_raw_handle())),
                JobObjectBasicAccountingInformation,
                &mut accounting as *mut _ as _,
                std::mem::size_of_val(&accounting)
                    .try_into()
                    .expect("job accounting fits DWORD"),
                None,
            )
        }
        .map_err(io::Error::other)?;
        Ok(accounting.ActiveProcesses == 0)
    }
}

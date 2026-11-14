use std::{
    env, fs,
    path::Path,
    process::{Command, Stdio},
    thread,
    time::Duration,
};

/// Keep an unpiped child alive after its parent exits for the Nushell integration fixture.
fn main() {
    let args: Vec<_> = env::args().collect();
    let ready = Path::new(&args[2]);
    let release = Path::new(&args[3]);
    if args[1] == "child" {
        fs::write(ready.with_extension("child"), "ready").unwrap();
        while !release.exists() {
            thread::sleep(Duration::from_millis(5));
        }
        return;
    }
    close_inherited_standard_handles();
    let child = Command::new(env::current_exe().unwrap())
        .args(["child", &args[2], &args[3]])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
    while !ready.with_extension("child").exists() {
        thread::sleep(Duration::from_millis(5));
    }
    fs::write(
        ready,
        format!(
            "{{\"parent_pid\":{},\"child_pid\":{}}}",
            std::process::id(),
            child.id()
        ),
    )
    .unwrap();
    // Drop without waiting: the child deliberately outlives both this root and its pipes.
}

/// Prevent an unrelated inherited pipe handle from disguising early tree completion.
fn close_inherited_standard_handles() {
    #[cfg(windows)]
    {
        #[link(name = "kernel32")]
        unsafe extern "system" {
            fn GetStdHandle(kind: u32) -> *mut std::ffi::c_void;
            fn SetHandleInformation(handle: *mut std::ffi::c_void, mask: u32, flags: u32) -> i32;
        }
        for kind in [u32::MAX - 9, u32::MAX - 10, u32::MAX - 11] {
            // SAFETY: inspect the current process's standard handles and clear only their
            // inheritance flags; their ownership and I/O use remain unchanged.
            unsafe {
                assert_ne!(SetHandleInformation(GetStdHandle(kind), 1, 0), 0);
            }
        }
    }
}

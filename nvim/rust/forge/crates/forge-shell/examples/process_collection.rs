use std::{path::PathBuf, process::Stdio, time::Duration};

/// Probe the public process wrapper; orchestration and assertions live in Nushell.
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args: Vec<_> = std::env::args_os().skip(1).collect();
    if args.first().is_some_and(|mode| mode == "--recover") {
        let identity = args
            .get(1)
            .ok_or("missing containment identity")?
            .to_str()
            .ok_or("invalid containment identity")?;
        println!(
            "{:?}",
            forge_shell::containment::ProcessContainment::recover(identity)?
        );
        return Ok(());
    }
    let containment = if args
        .first()
        .is_some_and(|mode| mode == "--contained" || mode == "--prepare")
    {
        let prepare = args[0] == "--prepare";
        let identity = args
            .get(1)
            .ok_or("missing containment identity")?
            .to_str()
            .ok_or("invalid containment identity")?;
        let containment = forge_shell::containment::ProcessContainment::create(identity)?;
        args.drain(..2);
        if prepare {
            std::fs::write(
                args.first().ok_or("missing owner marker")?,
                std::process::id().to_string(),
            )?;
            let release = args.get(1).ok_or("missing preparation release")?;
            while !std::path::Path::new(release).exists() {
                tokio::time::sleep(Duration::from_millis(5)).await;
            }
            return Ok(());
        }
        Some(containment)
    } else {
        None
    };
    if args.len() < 5 {
        return Err(
            "expected runner, readiness, release, cancellation, and collection paths".into(),
        );
    }
    let mut command = tokio::process::Command::new(&args[0]);
    command
        .arg("parent")
        .args(&args[1..3])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    let mut child = match &containment {
        Some(containment) => containment.spawn(command)?,
        None => forge_shell::process::spawn_owned(command)?,
    };
    if let Some(owner) = args.get(5) {
        std::fs::write(owner, std::process::id().to_string())?;
    }
    let cancel = PathBuf::from(&args[3]);
    let collected = PathBuf::from(&args[4]);
    loop {
        tokio::select! {
            result = child.wait() => {
                result?;
                break;
            }
            _ = tokio::time::sleep(Duration::from_millis(5)) => {
                if cancel.exists() {
                    child.start_kill()?;
                    child.wait().await?;
                    break;
                }
            }
        }
    }
    std::fs::write(collected, "collected")?;
    Ok(())
}

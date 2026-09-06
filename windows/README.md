# Windows machine setup

`setup.ps1` installs missing applications, configures Nushell when no configuration
already exists, and calls the Kanata installer to configure launch at login.
It runs in Windows PowerShell 5.1 or PowerShell 7 and requires an existing
PowerShell 7 installation (`pwsh`) and WinGet from Microsoft App Installer.
Keep this repository at its intended permanent location
because Nushell and the Kanata shortcut reference it.

From PowerShell at the repository root, check installed applications and current
configuration without installing applications or writing configuration:

```powershell
.\windows\setup.ps1 -Preview
```

Preview uses setup's detection checks and prints one status per item, such as
`Rust: installed (skip)` or `Zotero: missing (install)`. It compares PowerToys
settings against the snapshot and checks the Kanata startup shortcut. Checks
that require a missing Nushell or Kanata installation are reported as unchecked.
Detection errors stop preview instead of being reported as missing packages.

Run the installer after reviewing the list:

```powershell
.\windows\setup.ps1
```

Setup collects missing main applications and installs them in one WinGet call.
Shared CLI dependencies use a separate batch. Rust, PowerToys, and Kanata retain
their own setup steps. Installers run silently and accept package agreements. Windows can still request
administrator approval for packages that require it. Setup does not request an
automatic reboot. Kanata setup runs on every setup invocation and preserves an
already matching startup shortcut.

Application IDs live in `windows/packages.json`. Shared CLI dependencies live in
`nushell/dependencies.nu`, which both `env.nu` and setup use. Setup checks for
`pwsh` before installing any packages and does not install PowerShell itself.
Node.js uses the `OpenJS.NodeJS.LTS` package. Setup does not install fnm.
The main setup also calls `setup-rust.ps1` to ensure MSVC C++ tools and a Windows
SDK exist, installing missing prerequisites through WinGet's
`Microsoft.VisualStudio.2022.BuildTools` package. Existing Visual Studio C++
installations can satisfy this check. A partial installation may be updated to
add the missing components.

Rust uses `Rustlang.Rustup`. Setup selects stable MSVC on a new installation,
preserves an existing default toolchain, and checks `rustc` and Cargo. It does
not run `rustup update` or install Cargo applications. A Rust installation without
an available rustup is preserved and reported for manual reconciliation.

Reruns skip available CLI commands and applications registered with WinGet.
Detection failures stop setup instead of being treated as missing applications.
An existing working Node.js LTS installation is retained. A detected non-LTS or
broken Node installation stops setup before package installation so it can be
resolved explicitly.

Setup creates a directory junction from the current user's AppData Nushell
directory to this repository's `nushell` directory. An existing matching junction
is retained. Any other existing configuration is reported as preserved and
must be reconciled manually. The script does not change the default terminal or
configure WezTerm's shell. Start `nu` in a new terminal to use Nushell.

Kanata setup preserves an already matching login shortcut and verifies both
keyboard profiles. Validate Kanata separately without changing startup:

```powershell
nu --no-config-file kanata/setup-kanata.nu --check
```

If an installation fails, resolve the reported package error and rerun setup.
Already installed packages are skipped. If a newly installed command is not
available after PATH refresh, open a new terminal and rerun setup.

The setup preview was exercised on an existing Windows installation and with
simulated missing application packages. A full clean-machine installation has
not been tested.

## PowerToys

The main setup script calls `setup-powertoys.ps1`, which installs PowerToys only
when missing. Its default snapshot is `powertoys/backup`.
It validates the snapshot's JSON and checksums before installing or restoring.
Matching settings are skipped. Existing differing settings are preserved by
default, while an unconfigured installation receives the saved settings.

Preview PowerToys installation and the saved files without changing anything:

```powershell
pwsh -NoProfile -File .\windows\setup-powertoys.ps1 -Preview
```

To replace existing settings, exit PowerToys and Command Palette, then run:

```powershell
.\windows\setup.ps1 -ApplyPowerToysConfig
```

The standalone equivalent is `pwsh -NoProfile -File
.\windows\setup-powertoys.ps1 -ApplyConfig`. Changed destination files are backed
up beneath the current user's LocalAppData `Microsoft\PowerToys-SetupBackups`
directory before replacement. If an installer starts PowerToys automatically,
exit PowerToys and rerun with `-ApplyConfig` to apply the snapshot. Start PowerToys
after restoration to load the settings. The script requires PowerShell 7.

# Set up Kanata

`setup-kanata.nu` installs Kanata when missing, validates `layer.kbd` and
`default.kbd`, and registers automatic startup. Both platforms use the same
profiles. The custom layout loads first.

Run commands from the repository root with Nushell installed. Windows requires
WinGet. macOS requires Homebrew and an administrator account. Run setup as your
normal user, without putting `sudo` before `nu`.

## Windows

Create or update both startup and desktop shortcuts:

```nu
nu kanata/setup-kanata.nu --desktop
```

Omit `--desktop` to update only startup. Disable AHK separately before running
Kanata. Existing Kanata processes must be exited and relaunched to receive
changes to the shortcut's profile list.

## macOS

Install Kanata and configure its system services:

```nu
nu kanata/setup-kanata.nu
```

The script uses Homebrew for Kanata and installs the standalone
Karabiner-DriverKit-VirtualHIDDevice 6.2.0 package if its manager or daemon is
missing. It checks the downloaded package's SHA-256 before installation. This
driver version follows [Kanata's release instructions](https://github.com/jtroo/kanata/releases/tag/v1.12.0).
An existing driver installation is retained. Compatibility with other installed
driver versions requires verification on the Mac.

Approve the Karabiner system extension when macOS requests it. If setup reports
that activation is pending, approve it in System Settings, restart if requested,
and run setup again. The script does not register services before driver approval.

Grant Input Monitoring permission to the Kanata executable printed by setup under
System Settings → Privacy & Security → Input Monitoring. The executable normally
resides under `/opt/homebrew/opt/kanata/bin/` on Apple Silicon or
`/usr/local/opt/kanata/bin/` on Intel. macOS permissions cannot be granted by this
script.

Setup registers two root-owned LaunchDaemons:

- `local.dotfiles.kanata-driver` starts the virtual device daemon and restarts it
  if it exits.
- `local.dotfiles.kanata` starts Kanata at boot and attempts to start it during
  setup. It retries after an error with a 10-second throttle. A successful exit,
  including the emergency exit shortcut, does not trigger a restart.

If Kanata exits before permissions are granted, start it again after granting
them:

```nu
sudo launchctl kickstart system/local.dotfiles.kanata
```

The Mac setup does not install a menu bar application. To make the vanilla
profile active at startup, rerun setup with it first:

```nu
nu kanata/setup-kanata.nu --config kanata/default.kbd
```

Run setup without `--config` to restore the custom layout. The Windows-only
`--desktop` option is rejected on macOS.

## Verification and recovery

Validate discovery and profiles without installing software or changing startup:

```nu
nu kanata/setup-kanata.nu --check
```

On macOS, this check also requires the driver manager and daemon to exist. It
does not prove that Input Monitoring or keyboard interception works. Check the
registered service and its log on the Mac:

```nu
sudo launchctl print system/local.dotfiles.kanata
sudo tail -n 50 /var/log/local.dotfiles.kanata.log
```

Verify ordinary typing, Caps Lock tap/hold, and the Alt layer after permission
setup. If Kanata stopped after a successful exit, use the
`kickstart` command above. Driver output appears in
`/var/log/local.dotfiles.kanata-driver.log`.

Setup refuses to create overlapping services when it detects a Homebrew Kanata
service or a virtual device daemon owned by another service. Stop the existing
service before rerunning setup. For a Homebrew Kanata service, use
`sudo brew services stop kanata`. Do not run Karabiner-Elements remapping and
Kanata remapping simultaneously.

To disable Kanata on macOS without uninstalling either executable:

```nu
sudo launchctl disable system/local.dotfiles.kanata
sudo launchctl bootout system/local.dotfiles.kanata
```

Rerunning setup enables and registers the service again. Keep the repository at
its configured location, or rerun setup after moving it.

The macOS branch has syntax and service-file tests on Windows. Installation,
permission approval, reboot behavior, and actual keyboard input still require
verification on a Mac.

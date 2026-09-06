use kanata-macos.nu [macos-executable macos-startup]

def main [
    --executable: path
    --config: path
    --check
    --preview
    --desktop
] {
    const adapter = path self kanata-startup.ps1
    const default_config_path = path self layer.kbd
    const vanilla_config_path = path self default.kbd
    let config_path = ($config | default $default_config_path | path expand)
    let platform = (sys host | get name)
    if $preview and $platform != 'Windows' {
        error make {msg: '--preview is Windows-only. Use --check on macOS.'}
    }
    if $platform not-in ['Windows' 'Darwin'] {
        error make {msg: $'Unsupported operating system: ($platform)'}
    }
    if $platform == 'Darwin' and $desktop {
        error make {msg: '--desktop is Windows-only. On macOS, omit it to configure launchd startup.'}
    }
    let location = if $platform == 'Windows' {
        let location_result = (^pwsh -NoProfile -NonInteractive -File $adapter locations | complete)
        if $location_result.exit_code != 0 {
            error make {msg: $'Cannot resolve Windows folders: ($location_result.stderr)($location_result.stdout)'}
        }
        $location_result.stdout | from json
    } else { {} }
    let executable_path = if $executable != null {
        $executable | path expand
    } else if $platform == 'Darwin' {
        macos-executable $check
    } else {
        let package_pattern = ($location.local_app_data | path join 'Microsoft' 'WinGet' 'Packages' 'jtroo.kanata_gui_*' 'kanata_windows_gui_winIOv2_x64.exe')
        let discovery_pattern = ($package_pattern | str replace --all '\' '/')
        mut candidate = (glob $discovery_pattern)
        if ($candidate | is-empty) {
            if $preview {
                print 'Kanata: missing (install)'
                print 'Kanata startup: unchecked (requires Kanata installation)'
                return
            }
            if $check {
                error make {msg: 'Kanata GUI is not installed. Run this script without --check to install it and configure startup.'}
            }
            if (which winget.exe | is-empty) {
                error make {msg: 'WinGet is missing. Install Microsoft App Installer, then run this script again.'}
            }
            print 'Installing Kanata GUI with WinGet...'
            let installation = (^winget.exe install --id jtroo.kanata_gui --exact --source winget --scope user --silent --no-upgrade --accept-package-agreements --accept-source-agreements --disable-interactivity | complete)
            if $installation.exit_code != 0 {
                error make {msg: $'Kanata installation failed with exit code ($installation.exit_code): ($installation.stdout)($installation.stderr)'}
            }
            $candidate = (glob $discovery_pattern)
            if ($candidate | is-empty) {
                error make {msg: 'WinGet completed, but the Kanata GUI executable was not found in the user package directory. Pass --executable with its installed path.'}
            }
        }
        if ($candidate | length) != 1 {
            error make {msg: 'Found multiple Kanata GUI packages. Pass --executable with the GUI executable to use.'}
        }
        $candidate | first
    }
    if not ($executable_path | path exists) {
        error make {msg: $'Kanata executable does not exist: ($executable_path)'}
    }
    let validator_path = if $platform == 'Windows' {
        $executable_path | str replace 'kanata_windows_gui_' 'kanata_windows_tty_'
    } else { $executable_path }
    if $platform == 'Windows' and (($validator_path == $executable_path) or not ($validator_path | path exists)) {
        error make {msg: 'Select a kanata_windows_gui_* executable with its matching kanata_windows_tty_* validator installed beside it.'}
    }
    let config_path_list = [$config_path $vanilla_config_path]
    for profile_path in $config_path_list {
        if not ($profile_path | path exists) {
            error make {msg: $'Kanata configuration does not exist: ($profile_path)'}
        }
        let validation = (run-external $validator_path '--check' '--cfg' $profile_path '--no-wait' | complete)
        if $validation.exit_code != 0 {
            error make {msg: $'Kanata rejected ($profile_path): ($validation.stdout)($validation.stderr)'}
        }
    }
    if $platform == 'Darwin' {
        macos-startup $executable_path $config_path_list $check
        return
    }
    let shortcut_path = ($location.startup | path join 'Kanata.lnk')
    print 'Kanata: installed, profiles valid (skip)'
    if $check {
        print {executable: $executable_path, profiles: $config_path_list, shortcut: $shortcut_path, desktop: $desktop, status: 'Validated without changing startup'}
        return
    }
    let shortcut_folder_list = if $desktop { ['Startup' 'Desktop'] } else { ['Startup'] }
    for shortcut_folder in $shortcut_folder_list {
        let action = if $preview { 'preview' } else { 'create' }
        let creation = (^pwsh -NoProfile -NonInteractive -File $adapter $action $shortcut_folder $executable_path ...$config_path_list | complete)
        if $creation.exit_code != 0 {
            error make {msg: $'Cannot create ($shortcut_folder) shortcut: ($creation.stdout)($creation.stderr)'}
        }
        print ($creation.stdout | str trim)
    }
    if not $preview { print 'Kanata will launch at your next Windows sign-in. Disable AHK separately before then.' }
}

def checked-command [program: string, ...argument: string] {
    let result = (run-external $program ...$argument | complete)
    if $result.exit_code != 0 {
        error make {msg: $'($program) failed with exit code ($result.exit_code): ($result.stdout)($result.stderr)'}
    }
    $result.stdout | str trim
}

export def macos-executable [check: bool] {
    let installed = (which kanata)
    if not ($installed | is-empty) {
        return ($installed | first | get path)
    }
    let brew_candidate = ([(which brew | get path) ['/opt/homebrew/bin/brew' '/usr/local/bin/brew']] | flatten | flatten | where {|candidate| $candidate | path exists })
    if ($brew_candidate | is-empty) {
        error make {msg: 'Homebrew is missing. Install Homebrew from https://brew.sh, then rerun setup as your normal user.'}
    }
    let brew_path = ($brew_candidate | first)
    let prefix = (checked-command $brew_path '--prefix')
    let executable_path = ($prefix | path join 'opt' 'kanata' 'bin' 'kanata')
    if not ($executable_path | path exists) {
        if $check {
            error make {msg: 'Kanata is missing. Run without --check to install it through Homebrew.'}
        }
        print 'Installing Kanata through Homebrew...'
        checked-command $brew_path 'install' 'kanata' | print
    }
    $executable_path
}

export def macos-service [label: string, argument: list<string>, restart_policy: string] {
    let program_xml = ($argument | each {|value|
        let escaped = ($value | str replace --all '&' '&amp;' | str replace --all '<' '&lt;' | str replace --all '>' '&gt;')
        $'<string>($escaped)</string>'
    } | str join '')
    let keep_alive_xml = match $restart_policy {
        'always' => '<true/>'
        'failure' => '<dict><key>SuccessfulExit</key><false/></dict>'
        _ => { error make {msg: $'Unknown restart policy: ($restart_policy)'} }
    }
    $'<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>Label</key><string>($label)</string>
<key>ProgramArguments</key><array>($program_xml)</array>
<key>RunAtLoad</key><true/>
<key>KeepAlive</key>($keep_alive_xml)
<key>ThrottleInterval</key><integer>10</integer>
<key>ProcessType</key><string>Interactive</string>
<key>StandardOutPath</key><string>/var/log/($label).log</string>
<key>StandardErrorPath</key><string>/var/log/($label).log</string>
</dict></plist>'
}

export def macos-startup [executable_path: path, config_path_list: list<path>, check: bool] {
    let manager_path = '/Applications/.Karabiner-VirtualHIDDevice-Manager.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Manager'
    let daemon_path = '/Library/Application Support/org.pqrs/Karabiner-DriverKit-VirtualHIDDevice/Applications/Karabiner-VirtualHIDDevice-Daemon.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Daemon'
    let driver_installed = (($manager_path | path exists) and ($daemon_path | path exists))
    let service_path = '/Library/LaunchDaemons/local.dotfiles.kanata.plist'
    if $check {
        print {executable: $executable_path, profiles: $config_path_list, driver_installed: $driver_installed, service: $service_path, status: 'Profiles validated. Driver activation and Input Monitoring require verification on this Mac.'}
        if not $driver_installed {
            error make {msg: 'Karabiner virtual keyboard driver is missing. Run without --check to install it.'}
        }
        return
    }
    checked-command sudo '-v' | ignore
    if not $driver_installed {
        let package_path = (mktemp --tmpdir --suffix '.pkg')
        try {
            checked-command curl '--fail' '--location' '--max-time' '60' '--output' $package_path 'https://raw.githubusercontent.com/pqrs-org/Karabiner-DriverKit-VirtualHIDDevice/main/dist/Karabiner-DriverKit-VirtualHIDDevice-6.2.0.pkg' | ignore
            if (open --raw $package_path | hash sha256) != '9e8c46239f0748161241e42444857901224e5c82f5b58a1731df4c70bf0736a8' {
                error make {msg: 'Karabiner driver package checksum mismatch.'}
            }
            checked-command sudo '-n' '/usr/sbin/installer' '-pkg' $package_path '-target' '/' | print
        } catch {|failure|
            rm $package_path
            error make {msg: $failure.msg}
        }
        rm $package_path
    }
    if not ($manager_path | path exists) or not ($daemon_path | path exists) {
        error make {msg: 'Karabiner driver installation did not provide the expected manager and daemon.'}
    }
    checked-command $manager_path 'activate' | print
    let extension_result = (checked-command /usr/bin/systemextensionsctl 'list')
    if not ($extension_result | lines | any {|line| ($line | str contains 'org.pqrs.Karabiner-DriverKit-VirtualHIDDevice') and ($line | str contains '[activated enabled]') }) {
        error make {msg: 'Approve the Karabiner driver in System Settings, restart if macOS requests it, then rerun setup. Startup has not been changed.'}
    }
    let existing_brew_service = (^sudo -n launchctl print system/homebrew.mxcl.kanata | complete)
    if $existing_brew_service.exit_code == 0 or ('/Library/LaunchDaemons/homebrew.mxcl.kanata.plist' | path exists) {
        error make {msg: 'An existing Homebrew Kanata service would overlap. Run sudo brew services stop kanata, then rerun setup.'}
    }
    let daemon_running = (^pgrep -x Karabiner-VirtualHIDDevice-Daemon | complete)
    let own_daemon = (^sudo -n launchctl print system/local.dotfiles.kanata-driver | complete)
    if $daemon_running.exit_code == 0 and $own_daemon.exit_code != 0 {
        error make {msg: 'Another service already runs the Karabiner virtual device daemon. Stop that service before configuring this standalone Kanata setup.'}
    }
    let config_argument = ($config_path_list | each {|profile| ['--cfg' $profile] } | flatten)
    let service_list = [
        {label: 'local.dotfiles.kanata-driver', argument: [$daemon_path], restart_policy: 'always'}
        {label: 'local.dotfiles.kanata', argument: ([$executable_path '--no-wait'] | append $config_argument), restart_policy: 'failure'}
    ]
    for service in $service_list {
        let temporary_path = (mktemp --tmpdir --suffix '.plist')
        let destination = $'/Library/LaunchDaemons/($service.label).plist'
        try {
            macos-service $service.label $service.argument $service.restart_policy | save --force $temporary_path
            checked-command plutil '-lint' $temporary_path | ignore
            checked-command sudo '-n' install '-o' 'root' '-g' 'wheel' '-m' '644' $temporary_path $destination | ignore
            let loaded = (^sudo -n launchctl print $'system/($service.label)' | complete)
            if $loaded.exit_code == 0 {
                checked-command sudo '-n' launchctl 'bootout' $'system/($service.label)' | ignore
            }
            checked-command sudo '-n' launchctl 'enable' $'system/($service.label)' | ignore
            checked-command sudo '-n' launchctl 'bootstrap' 'system' $destination | ignore
            checked-command sudo '-n' launchctl 'print' $'system/($service.label)' | print
        } catch {|failure|
            rm $temporary_path
            error make {msg: $failure.msg}
        }
        rm $temporary_path
    }
    print $'Startup services registered. Grant Input Monitoring to ($executable_path) in System Settings. If Kanata exited before permission was granted, run: sudo launchctl kickstart system/local.dotfiles.kanata'
    print 'Kanata logs: /var/log/local.dotfiles.kanata.log. This setup does not install a macOS tray application.'
}

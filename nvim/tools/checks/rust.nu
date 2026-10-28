def main [action: string] {
  let request = $in | from json
  if $request.version != 1 { error make {msg: "Unsupported check request version"} }
  if $action not-in ["check" "test" "format" "format-fix" "clippy"] { error make {msg: "Unsupported Rust check action"} }
  let root = $request.workspace | path join "nvim" "rust" "forge"
  cd $root
  let inputs = $request.inputs
  let package = $inputs.package? | default ""
  let test = $inputs.test? | default ""
  let executable = $inputs.executable? | default "lib"
  let full = ($inputs.full? | default false) or $request.selection.kind == "all"
  let files = $inputs.files? | default ($request.changed_files? | default [])
  if $test != "" and $package == "" {
    error make {msg: "An exact Rust test requires an explicit package"}
  }
  if $test != "" and $action != "test" {
    error make {msg: "Test names apply only to the test action"}
  }
  if $full and ($test != "" or $package != "") {
    error make {msg: "Full workspace coverage cannot be combined with a package or test"}
  }
  let packages = if $full {
    []
  } else if $package != "" {
    [$package]
  } else {
    if ($files | is-empty) { error make {msg: "Specify a package, changed files, or full:true"} }
    let metadata = ^cargo metadata --format-version 1 --no-deps --offline --locked | complete
    if $metadata.exit_code != 0 {
      print --stderr $metadata.stderr
      exit $metadata.exit_code
    }
    let metadata = $metadata.stdout | from json
    let members = $metadata.packages | where {|member| $member.id in $metadata.workspace_members }
    let root_parts = $root | path expand | path split
    let rust_files = $files | where {|file|
      let parts = $request.workspace | path join $file | path expand | path split
      (($parts | length) >= ($root_parts | length)) and (($parts | take ($root_parts | length)) == $root_parts)
    }
    if ($rust_files | is-empty) { error make {msg: "No Rust workspace paths selected; specify a package or full:true"} }
    $rust_files | each {|file|
      let absolute = $request.workspace | path join $file | path expand
      let owners = $members | where {|member|
        let directory = $member.manifest_path | path dirname | path expand
        let candidate = $absolute | path split
        let owner = $directory | path split
        (($candidate | length) >= ($owner | length)) and (($candidate | take ($owner | length)) == $owner)
      } | sort-by {|member| $member.manifest_path | str length } | reverse
      if ($owners | is-empty) { error make {msg: $"No Rust package owns ($file)"} }
      $owners | first | get name
    } | uniq
  }
  let coverage = if $full { [""] } else { $packages }
  mut failed = false
  mut reports = []
  for selected in $coverage {
    let scope = if $full { ["--workspace"] } else { ["--package" $selected] }
    let target = if $executable == "all" {
      ["--all-targets"]
    } else if $full {
      ["--lib" "--bins"]
    } else if $selected == "forge" and $test == "" and $executable == "lib" {
      ["--bin" "forge"]
    } else if $executable == "lib" {
      ["--lib"]
    } else if ($executable | str starts-with "test:") {
      ["--test" ($executable | str substring 5..)]
    } else if ($executable | str starts-with "bin:") {
      ["--bin" ($executable | str substring 4..)]
    } else {
      error make {msg: "executable must be all, lib, test:<name>, or bin:<name>"}
    }
    let arguments = if $action in ["format" "format-fix"] {
      ["fmt"] | append (if $full { ["--all"] } else { $scope }) | append (if $action == "format" { ["--" "--check"] } else { [] })
    } else if $action == "check" {
      ["check" "--profile" "dev" "--locked"] | append $scope | append $target
    } else if $action == "clippy" {
      ["clippy" "--profile" "dev" "--locked"] | append $scope | append $target | append ["--" "-D" "warnings"]
    } else {
      ["test" "--profile" "dev" "--locked"] | append $scope | append $target | append (
        if $test == "" { [] } else { ["--" $test "--exact"] }
      )
    }
    let started = date now
    let result = (
      ^cargo ...$arguments
      | tee --stderr { lines | each {|line| print --stderr $line } | ignore }
      | tee { lines | each {|line| print $line } | ignore }
      | complete
    )
    let elapsed = ((date now) - $started) / 1ms | into int
    let ran = $test == "" or ($result.stdout | lines | any {|line|
      ($line | str trim) == $"test ($test) ... ok"
    })
    let passed = $result.exit_code == 0 and $ran
    if not $ran {
      print --stderr $"Requested test did not execute successfully: ($test)"
    }
    if not $passed { $failed = true }
    $reports = $reports | append {
      file: "nvim/rust/forge/Cargo.toml"
      state: (if $passed { "passed" } else { "failed" })
      duration_ms: $elapsed
      command: (["cargo"] | append $arguments | str join " ")
      output: ($result.stdout + $result.stderr)
    }
  }
  {version: 1, result: $reports} | to json --raw | save --force $request.report_path
  {version: 1, selection: $request.selection} | to json --raw | save --force $request.receipt_path
  if $failed { exit 1 }
}

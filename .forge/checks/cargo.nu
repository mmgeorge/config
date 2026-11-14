use adapter-common.nu [emit request source-position]

def main [] {
  error make {msg: 'Use cargo.nu adapter with a protocol request on stdin'}
}

def 'main adapter' [--bin: string, --release] {
  let input = $in | request
  if $input.op not-in ['typecheck' 'lint' 'lint_fix' 'build' 'format_check' 'format_apply'] {
    emit $input.events {event: 'unsupported'}
    return
  }
  let result = try {
    let directory = $input.workspace | path join $input.directory | path expand
    cd $directory
    let widening = if ($input.select.paths | is-empty) { null } else if ($input.op | str starts-with 'format_') {
      'cargo fmt owns target and module discovery; formatted the workspace'
    } else { 'cargo checks whole packages; ran the workspace' }
    emit $input.events {event: 'plan', widened: $widening}
    if ($input.op | str starts-with 'format_') {
      run-format $input $directory
    } else if $input.op == 'lint_fix' {
      let fixed = run-cargo $input $directory ['clippy' '--fix' '--allow-dirty' '--allow-staged' '--profile' 'dev' '--locked' '--workspace' '--all-targets' '--message-format' 'json'] --mutation
      let checked = run-cargo $input $directory ['clippy' '--profile' 'dev' '--locked' '--workspace' '--all-targets' '--message-format' 'json' '--' '-D' 'warnings']
      if $fixed != 0 { $fixed } else { $checked }
    } else {
      let args = match $input.op {
        'typecheck' => ['check' '--profile' 'dev' '--locked' '--workspace' '--all-targets' '--message-format' 'json']
        'lint' => ['clippy' '--profile' 'dev' '--locked' '--workspace' '--all-targets' '--message-format' 'json' '--' '-D' 'warnings']
        'build' => {
          let profile = if $release { 'release' } else { 'dev' }
          let target = if $bin == null { ['--workspace'] } else { ['--bin' $bin] }
          ['build' '--profile' $profile '--locked' '--message-format' 'json'] | append $target
        }
      }
      run-cargo $input $directory $args
    }
  } catch {|failure|
    emit $input.events {event: 'error', message: $failure.msg}
    1
  }
  emit $input.events {event: 'complete'}
  exit $result
}

# Stream stdout while Cargo runs; send stderr through Harness's bounded log sink.
def run-cargo [input: record, directory: path, args: list<string>, --mutation] {
  let stderr = $input.events + '.cargo-stderr'
  let result = try {
    ^cargo ...$args err> $stderr | lines | reduce --fold [] {|line, seen|
      if $mutation {
        emit $input.events {event: 'output', text: ($line + "\n")}
      } else {
        let record = $line | from json
        if $record.reason == 'compiler-message' {
          let diagnostic = translate-diagnostic $record $input.workspace $directory
          if $diagnostic != null { emit $input.events $diagnostic }
        } else if $record.reason == 'compiler-artifact' {
          # A package can compile multiple targets and test/non-test units.
          let identity = $record | select package_id target profile features | to json --raw | hash sha256
          if $identity not-in $seen {
            emit $input.events {event: 'target_finished', id: $"cargo:($identity)", status: 'passed'}
            if $input.op == 'build' and $record.executable? != null {
              let path = repository-path $record.executable $input.workspace $directory
              if $path != null {
                emit $input.events {event: 'artifact', path: $path, kind: 'executable'}
              } else { emit $input.events {event: 'output', text: ($line + "\n")} }
            }
            return ($seen | append $identity)
          }
        }
      }
      $seen
    } | ignore
    $env.LAST_EXIT_CODE | into int
  } catch {|failure|
    if $failure.msg == 'External command had a non-zero exit code' {
      $env.LAST_EXIT_CODE | into int
    } else {
      emit $input.events {event: 'error', message: $failure.msg}
      1
    }
  }
  for line in (open --raw $stderr | lines) { emit $input.events {event: 'output', text: ($line + "\n")} }
  rm $stderr
  if $result != 0 { emit $input.events {event: 'error', message: $"cargo ($args.0) exited with status ($result); see command output"} }
  $result
}

def run-format [input: record, directory: path] {
  let args = if $input.op == 'format_check' { ['fmt' '--all' '--' '--check' '-l'] } else { ['fmt' '--all' '--' '-l'] }
  let output = ^cargo ...$args | complete
  for line in ($output.stdout | lines | where {|line| $line != '' } | uniq) {
    let path = repository-path $line $input.workspace $directory
    if $path == null or not ($input.workspace | path join $path | path exists) {
      error make {msg: $"Unrecognized cargo fmt file output: ($line)"}
    }
    emit $input.events {event: 'file', path: $path, status: (if $input.op == 'format_check' { 'unformatted' } else { 'changed' })}
  }
  if $output.stderr != '' { emit $input.events {event: 'output', text: $output.stderr} }
  if $output.exit_code != 0 and ($output.stdout | str trim | is-empty) {
    emit $input.events {event: 'error', message: $"cargo fmt exited with status ($output.exit_code): ($output.stderr | str trim)"}
  }
  $output.exit_code
}

export def translate-diagnostic [record: record, workspace: path, directory: path] {
  let message = $record.message
  if ($message.message | str starts-with 'aborting due to ') and ($message.spans | is-empty) and $message.code? == null { return null }
  let code = $message.code?.code?
  let severity = match $message.level { 'error' => 'error', 'warning' => 'warning', _ => 'info' }
  mut result = {event: 'diagnostic', severity: $severity, message: $message.message, code: $code,
    source: (if ($code | default '' | str starts-with 'clippy::') { 'clippy' } else { 'rustc' }), rendered: $message.rendered?}
  let primary = $message.spans | where is_primary | first 1
  if not ($primary | is-empty) {
    let located = locate-span $primary.0 $workspace $directory
    if $located != null { $result = $result | merge $located }
  }
  let fixes = suggested-fixes $message $workspace $directory
  $result | insert fixes $fixes
}

def suggested-fixes [message: record, workspace: path, directory: path] {
  mut fixes = []
  let spans = $message.spans | where {|span| $span.suggested_replacement? != null }
  if not ($spans | is-empty) {
    let edits = $spans | each {|span|
      let location = locate-span $span $workspace $directory
      if $location != null and $location.range? != null {
        $location | insert replacement $span.suggested_replacement
      }
    } | compact
    # Multipart suggestions are useful only when every edit has a safe location.
    if ($edits | length) == ($spans | length) {
      $fixes = $fixes | append {message: $message.message, edits: $edits}
    }
  }
  for child in $message.children { $fixes = $fixes | append (suggested-fixes $child $workspace $directory) }
  $fixes
}

def locate-span [span: record, workspace: path, directory: path] {
  let path = repository-path $span.file_name $workspace $directory
  if $path == null { return null }
  let range = try {
    let source = open --raw ($workspace | path join $path | path expand) | into string
    {start: (source-position $source $span.byte_start), end: (source-position $source $span.byte_end)}
  } catch { null }
  if $range == null { {path: $path} } else { {path: $path, range: $range} }
}

export def repository-path [value: string, workspace: path, directory: path] {
  let absolute = $directory | path join $value | path expand
  let relative = try { $absolute | path relative-to ($workspace | path expand) } catch { return null }
  let normalized = $relative | str replace --all '\' '/'
  if ($normalized | split row '/' | any {|part| $part == '..' }) { return null }
  $normalized
}

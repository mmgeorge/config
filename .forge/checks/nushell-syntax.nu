use adapter-common.nu [emit request owns-path relative-path source-position]
const repository_root = path self | path dirname | path dirname | path dirname

# Preserve the argument-based command used by the current check configuration.
def main [...files: string] {
  let paths = $files | each {|file|
    if $file =~ '^(?:/|[A-Za-z]:)' { $file | path expand | path relative-to $repository_root } else { relative-path $file }
  }
  exit (check-files {workspace: $repository_root, directory: '.', select: {paths: $paths}})
}

def 'main adapter' [] {
  let input = $in | request
  if $input.op != 'typecheck' {
    emit $input.events {event: 'unsupported'}
    return
  }
  let code = try { check-files $input $input.events } catch {|failure|
    emit $input.events {event: 'error', message: $failure.msg}
    1
  }
  emit $input.events {event: 'complete'}
  exit $code
}

def publish [target: any, event: record] {
  if $target != null { emit $target $event } else if $event.event == 'diagnostic' {
    print --stderr $"($event.path): ($event.message)"
  } else if $event.event == 'target_finished' {
    print $"($event.status)  ($event.id)"
  }
}

def check-files [input: record, output?: path] {
  cd $input.workspace
  let listed = ^git ls-files --cached --others --exclude-standard -z -- '*.nu' | complete
  if $listed.exit_code != 0 { error make {msg: $listed.stderr} }
  let explicit = $input.select.paths | where {|file| ($file | str ends-with '.nu') and ($file | path type) == 'file' }
  let files = $listed.stdout | split row (char nul) | where {|file| $file != '' } | append $explicit
    | each {|file| relative-path $file } | uniq | sort
    | where {|file| (owns-path $file [$input.directory]) and (owns-path $file $input.select.paths) and ($file | path exists) }
  publish $output {event: 'plan', targets: $files}
  mut failed = false
  for file in $files {
    let absolute = $input.workspace | path join $file
    # The child preserves parser diagnostics that bypass Nushell's catch value.
    let parsed = ^$nu.current-exe --no-config-file --commands $"nu-check --debug ($absolute | to nuon)" | complete
    let passed = $parsed.exit_code == 0 and ($parsed.stdout | str trim) == 'true'
    if not $passed {
      $failed = true
      let inspected = ^$nu.current-exe --no-config-file --ide-check 100 $absolute | complete
      let diagnostics = $inspected.stdout | lines | each {|line| try { $line | from json } catch { null } }
        | where {|row| $row != null and $row.type? == 'diagnostic' and $row.severity? == 'Error' }
      let first = if ($diagnostics | is-empty) { null } else { $diagnostics | first }
      mut diagnostic = {event: 'diagnostic', severity: 'error', source: 'nushell', target: $file, path: $file,
        message: ($first.message? | default 'Nushell syntax check failed'), rendered: ($parsed.stderr + $parsed.stdout)}
      if $first.span? != null {
        let range = try {
          let source = open --raw ($absolute | path expand) | into string
          {start: (source-position $source $first.span.start), end: (source-position $source $first.span.end)}
        } catch { null }
        if $range != null { $diagnostic = $diagnostic | insert range $range }
      }
      publish $output $diagnostic
    }
    publish $output {event: 'target_finished', id: $file, status: (if $passed { 'passed' } else { 'failed' })}
  }
  if $failed { 1 } else { 0 }
}

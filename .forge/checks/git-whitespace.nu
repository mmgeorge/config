use adapter-common.nu [emit request relative-path owns-path]
const repository_root = path self | path dirname | path dirname | path dirname

def main [...paths: string] {
  let result = inspect {workspace: $repository_root, directory: '.', select: {paths: $paths}}
  print --no-newline $result.stdout
  print --stderr --no-newline $result.stderr
  exit $result.exit_code
}

def 'main adapter' [] {
  let input = $in | request
  if $input.op != 'lint' {
    emit $input.events {event: 'unsupported'}
    return
  }
  emit $input.events {event: 'plan'}
  let result = try { inspect $input } catch {|failure|
    {exit_code: 1, stdout: '', stderr: $failure.msg}
  }
  let diagnostics = translate $result.stdout
  for diagnostic in $diagnostics { emit $input.events $diagnostic }
  if $result.stderr != '' { emit $input.events {event: 'output', text: $result.stderr} }
  if $result.exit_code != 0 and ($diagnostics | is-empty) {
    emit $input.events {event: 'error', message: ($result.stderr + $result.stdout | str trim | default 'git diff --check failed')}
  }
  emit $input.events {event: 'complete'}
  exit $result.exit_code
}

def inspect [input: record] {
  cd $input.workspace
  let selected = if ($input.select.paths | is-empty) { [$input.directory] } else {
    $input.select.paths | each {|path|
      if (owns-path $path [$input.directory]) { $path } else if (owns-path $input.directory [$path]) { $input.directory } else { null }
    } | compact | uniq
  }
  if ($selected | is-empty) { return {exit_code: 0, stdout: '', stderr: ''} }
  ^git -c core.quotepath=false diff --no-ext-diff --no-color --check HEAD -- ...$selected | complete
}

export def translate [output: string] {
  $output | lines | each {|line|
    let parsed = $line | parse --regex '^(?<path>.+):(?<line>[0-9]+): (?<message>.+)$'
    if ($parsed | is-empty) { return null }
    let record = $parsed | first
    let path = if ($record.path | str starts-with '"') { $record.path | from json } else { $record.path }
    {event: 'diagnostic', severity: 'error', source: 'git', path: (relative-path $path),
      range: {start: {line: ($record.line | into int), column: 1}}, message: $record.message}
  } | compact
}

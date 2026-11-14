# Shared protocol boundaries for repository-owned script adapters.
export def emit [events: path, event: record] {
  (($event | to json --raw) + "\n") | save --append $events
}

export def relative-path [value: string] {
  let normalized = $value | str replace --all '\' '/'
  if ($normalized =~ '^(?:/|[A-Za-z]:)') or ($normalized | split row '/' | any {|part| $part == '..' }) or ($normalized | str contains (char nul)) {
    error make {msg: $"Expected a repository-relative path: ($value)"}
  }
  $normalized | str replace --regex '^\./' '' | str trim --right --char '/'
}

export def request [] {
  let value = $in | from json
  if $value.version != 1 { error make {msg: 'Unsupported adapter protocol version'} }
  if not ($value.workspace | path exists) { error make {msg: 'Adapter workspace does not exist'} }
  let directory = relative-path $value.directory
  let paths = $value.select.paths? | default [] | each {|path| relative-path $path }
  $value | upsert workspace ($value.workspace | path expand) | upsert directory $directory | upsert select {paths: $paths}
}

export def owns-path [file: string, selection: list<string>] {
  ($selection | is-empty) or ($selection | any {|path|
    $path in ['' '.'] or $file == $path or ($file | str starts-with ($path + '/'))
  })
}

export def source-position [source: string, offset: int] {
  let bytes = $source | encode utf-8
  if $offset < 0 or $offset > ($bytes | bytes length) { error make {msg: 'Source offset is outside the file'} }
  let prefix = if $offset == 0 { '' } else { $bytes | bytes at 0..<$offset | decode utf-8 }
  let rows = $prefix | split row "\n"
  {line: ($rows | length), column: (($rows | last | encode utf-8 | bytes length) + 1)}
}

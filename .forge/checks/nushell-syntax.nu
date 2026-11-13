const repository_root = path self | path dirname | path dirname | path dirname

# Check Nushell syntax with nu-check. With no files, check every `.nu` file Git tracks or would
# track; otherwise check the given files, skipping other file types and deleted paths.
def main [...files: string] {
  cd $repository_root
  let selected = if ($files | is-empty) {
    ^git ls-files --cached --others --exclude-standard -z -- '*.nu' | split row (char nul) | where {|file| $file != "" }
  } else {
    $files | where {|file| ($file | str ends-with ".nu") and ($file | path exists) }
  }
  mut failures = 0
  for file in $selected {
    let passed = try { nu-check --debug $file } catch {|failure| print --stderr $failure.msg; false }
    print $"(if $passed { 'PASS' } else { 'FAIL' })  ($file)"
    if not $passed { $failures += 1 }
  }
  if $failures > 0 { exit 1 }
}

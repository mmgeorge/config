const repository_root = path self | path dirname | path dirname | path dirname
const baseline_hash_threads = 8

def verify-asset [root: path, entry: record] {
  let component = $entry.path | path split
  if ($component | any {|part| $part == ".." or $part == "/" or ($part | str contains ":") }) {
    error make {msg: $"Asset path leaves its package: ($entry.path)"}
  }
  let path = $root | path join $entry.path
  if not ($path | path exists) or ((open --raw $path | hash sha256) != $entry.sha256) {
    error make {msg: $"Vendored asset digest differs: ($path)"}
  }
}

def verify-source [baseline_entry, completed_gates, entry] {
  let original = $baseline_entry | where source == $entry.source | first
  if $original.blob != $entry.baseline_blob {
    return {error: $"Baseline blob identity differs: ($entry.source)"}
  }

  let content = ^git cat-file blob $entry.baseline_blob | complete
  if $content.exit_code != 0 {
    return {error: $content.stderr}
  }
  if ($content.stdout | hash sha256) != $entry.baseline_sha256 {
    return {error: $"Baseline SHA-256 differs: ($entry.source)"}
  }

  let captured = match $entry.capture_provenance {
    baseline_bytes => ($content.stdout | hash sha256)
    baseline_reference => ($content.stdout | hash sha256)
    baseline_crlf => ($content.stdout | str replace --all "\r\n" "\n" | str replace --all "\n" "\r\n" | hash sha256)
    dirty_overlay_unavailable => null
    _ => { return {error: $"Unknown capture provenance: ($entry.source)"} }
  }
  if $captured != null and $captured != $entry.sha256 {
    return {error: $"Captured source digest differs: ($entry.source)"}
  }

  let original_exists = $entry.source | path exists
  let destination_exists = $entry.destination | path exists
  if not $original_exists and not $destination_exists {
    return {error: $"Source has no owner: ($entry.source) -> ($entry.destination)"}
  }

  let mode = $entry.mode? | default relocation
  if $mode not-in [relocation in_place replacement] {
    return {error: $"Unknown source migration mode: ($entry.source)"}
  }
  if $entry.gate in $completed_gates and (not $destination_exists or ($mode != "in_place" and $original_exists)) {
    return {error: $"Completed gate has an incomplete relocation: ($entry.source)"}
  }

  {error: null}
}

# Every baseline source retains a concrete owner until its cutover gate passes.
def main [] {
  cd $repository_root
  let inventory = open nvim/rust/forge/migration.toml
  if $inventory.format_version != 2 { error make {msg: "Unsupported migration inventory format"} }
  let baseline = ^git ls-tree -r '--format=%(objectname)%x09%(path)' $inventory.baseline_revision -- ...$inventory.baseline_source_root | complete
  if $baseline.exit_code != 0 { error make {msg: $baseline.stderr} }
  let baseline_entry = $baseline.stdout | lines | where {|line| $line != "" } | split column "\t" blob source
  let source_path = $baseline_entry | get source
  let recorded_path = $inventory.source | get source
  if ($recorded_path | uniq | length) != ($recorded_path | length) {
    error make {msg: "Migration inventory repeats a baseline source"}
  }
  let missing = $source_path | where {|path| $path not-in $recorded_path }
  let extra = $recorded_path | where {|path| $path not-in $source_path }
  if not ($missing | is-empty) or not ($extra | is-empty) {
    error make {msg: $"Baseline inventory differs. Missing: ($missing | to json --raw). Extra: ($extra | to json --raw)"}
  }
  let source_result = $inventory.source | par-each --threads $baseline_hash_threads --keep-order {|entry|
    verify-source $baseline_entry $inventory.completed_gates $entry
  }
  let failed_source = $source_result | where $it.error != null | first
  if not ($failed_source | is-empty) {
    error make {msg: $failed_source.error}
  }
  mut asset_count = 0
  for manifest_path in $inventory.asset_manifest {
    let manifest = open $manifest_path
    let root = $manifest_path | path dirname
    if $manifest.version != 1 { error make {msg: $"Unsupported asset manifest: ($manifest_path)"} }
    if "grammar" in ($manifest | columns) {
      for grammar in $manifest.grammar {
        for entry in $grammar.files {
          verify-asset ($root | path join $grammar.name) $entry
          $asset_count += 1
        }
      }
    } else {
      for entry in $manifest.file {
        verify-asset ($root | path dirname) $entry
        $asset_count += 1
      }
    }
  }
  let unavailable = $inventory.source | where capture_provenance == dirty_overlay_unavailable | get source
  print ({baseline: $inventory.baseline_revision, preserved_source_files: ($source_path | length), verified_baseline_hashes: ($source_path | length), verified_assets: $asset_count, unavailable_dirty_capture: $unavailable, completed_gates: $inventory.completed_gates} | to json --raw)
}

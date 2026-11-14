# Export the pinned Harper release's expanded vocabulary without a runtime dependency.
const asset_directory = path self | path dirname

def main [cli: path] {
    let version = ^$cli --version | complete
    if $version.exit_code != 0 or ($version.stdout | str trim) != "harper-cli 3.2.0" {
        error make {msg: $"Expected harper-cli 3.2.0: ($version.stdout)($version.stderr)"}
    }
    let exported = ^$cli words | complete
    if $exported.exit_code != 0 {
        error make {msg: $"Harper dictionary export failed: ($exported.stderr)"}
    }
    let words = $exported.stdout | lines | each {|line| $line | from json | str lowercase } | uniq | sort
    if ($words | is-empty) or not ("oh" in $words) or not ("movements" in $words) {
        error make {msg: "Harper export is missing the expanded vocabulary"}
    }
    let output = $asset_directory | path join harper-3.2.0.txt
    (($words | str join "\n") + "\n") | save --raw --force $output
    print {path: $output, words: ($words | length), sha256: (open --raw $output | hash sha256)}
}

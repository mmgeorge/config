export def run-git-command [git_arguments: list<string>] {
  do {
    ^git ...$git_arguments
  }
  | tee { print -n }
  | tee --stderr { print --stderr -n }
  | complete
}

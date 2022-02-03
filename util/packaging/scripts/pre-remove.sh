#!/bin/sh

has_cmd() {
  command -v "$1" >/dev/null 2>&1
}

info() {
  echo "  [KOKA] $@"
}

wrap_output() {
  eval "$@" 2>&1 | sed 's/^/  [KOKA] /'
}


exec_code() {
  user="$1"

  if has_cmd sudo; then
    wrap_output sudo -u "$user" code $@
    return 0
  elif has_cmd su; then
    wrap_output su "$user" -c code $@
    return 0
  else
    info "Failed to install VSCode Koka extension for $user"
  fi

  return 1
}

main() {
  # Exit if first param is 1
  # RPM will pass 1 as the first param if it is being called whilst upgrading
  # For some reason it runs the install script before the uninstall script
  if [ "$1" = "1" ]; then
    exit 0
  fi

  # For each dir in /home
  for dir in /home/*; do
    user="$(basename "$dir")"

    target_atom_dir="$dir/.atom/packages/language-koka"
    if [ -d "$target_atom_dir" ]; then
      info "Removing Atom Koka extension for $user"

      rm -r $target_atom_dir
    fi

    if [ -d "$dir/.vscode/" ]; then
      if has_cmd code; then
        info "Removing VSCode Koka extension for $user"
        exec_code "$user" --uninstall-extension koka.language-koka
        if [ $? -ne 0 ]; then
          info "Failed to remove VSCode Koka extension for $user"
        fi
      fi
    fi
  done
}

main "$@"

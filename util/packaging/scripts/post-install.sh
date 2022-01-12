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
  # For each dir in /home
  for dir in /home/*; do
    user="$(basename "$dir")"

    if [ -d "$dir/.atom/" ]; then
      info "Installing Atom Koka extension for $user"

      target_atom_dir="$dir/.atom/packages/language-koka"
      mkdir -p "$target_atom_dir"

      koka_atom_dir="/usr/local/share/koka/*/contrib/atom"
      # limit to one dir
      koka_atom_dir="$(ls -d $koka_atom_dir | head -n 1)"

      if ! cp -p -r $koka_atom_dir/* $target_atom_dir; then
        info "Failed to install Atom Koka extension for $dir"
      fi
    fi

    if [ -d "$dir/.vscode/" ]; then
      if has_cmd code; then
        info "Installing VSCode Koka extension for $user"
        exec_code "$user" --force --install-extension koka.language-koka
        if [ $? -ne 0 ]; then
          info "Failed to install VSCode Koka extension for $user"
        fi
      fi
    fi
  done

  if has_cmd emacs; then
    echo "Emacs Koka syntax mode installed at: /usr/local/bin/{version}/contrib/emacs"
  fi
}

main "$@"

#!/bin/sh -e

#-----------------------------------------------------------------------------
# Installation script for Koka; use -h to see command line options.
#-----------------------------------------------------------------------------

VERSION="v3.1.1"
MODE="install"          # or uninstall
PREFIX="/usr/local"
QUIET=""
FORCE=""
MINIMAL="no"            # compiler only? (or also vscode integration etc)
OSARCH=""
OSNAME=""
OSDISTRO=""
VSCODE=""               # set if installing from the vscode extension

KOKA_DIST_BASE_URL="https://github.com/koka-lang/koka/releases/download"
KOKA_DIST_URL=""        # $KOKA_DIST_BASE_URL/$VERSION
KOKA_DIST_SOURCE=""     # $KOKA_DIST_URL/koka-$VERSION-<os>-<arch>.tar.gz
KOKA_TEMP_DIR=""        # empty creates one dynamically

# for tier-2 platforms adjust the default version to latest binary release
adjust_version() {  # <osarch>
  case "$1" in
    unix-freebsd-x64)
      VERSION="v2.4.0";;
  esac
}

#---------------------------------------------------------
# Helper functions
#---------------------------------------------------------

make_temp_dir() {
  if [ -z "$KOKA_TEMP_DIR" ] ; then
    KOKA_TEMP_DIR="$(mktemp -d 2>/dev/null || mktemp -d -t koka)"
  fi
}

cleanup_temp_dir() {
  if [ -n "$KOKA_TEMP_DIR" ] ; then
    rm -rf "$KOKA_TEMP_DIR"
    KOKA_TEMP_DIR=
  fi
}

info() {
  if [ -z "$QUIET" ] ; then
    echo "$@"
  fi
}

warn() {
  echo "$@" >&2
}

stop() {
  warn $@
  exit 1
}

has_cmd() {
  command -v "$1" > /dev/null 2>&1
}

on_path() {
  echo ":$PATH:" | grep -q :"$1":
}


#---------------------------------------------------------
# Detect OS and cpu architecture for download bundle
#---------------------------------------------------------

contains() {
  if echo "$1" | grep -i -E "$2" > /dev/null; then
    return 0
  else
    return 1
  fi
}

detect_osarch() {
  arch="$(uname -m)"
  case "$arch" in
    x86_64*|amd64*)
      arch="x64";;
    x86*|i[35678]86*)
      arch="x86";;
    arm64*|aarch64*|armv8*)
      arch="arm64";;
    arm*)
      arch="arm";;
    parisc*)
      arch="hppa";;
  esac

  OSNAME="linux"
  case "$(uname)" in
    [Ll]inux)
      OSNAME="linux";;
    [Dd]arwin)
      OSNAME="macos";;
    [Ff]ree[Bb][Ss][Dd])
      OSNAME="unix-freebsd";;
    *)
      info "Warning: unable to detect OS, assuming generic Linux"
  esac
  OSARCH="$OSNAME-$arch"

  if [ "$OSNAME" = "linux" ]; then
    distrocfg=`cat /etc/*-release`
    if contains "$distrocfg" "rhel"; then
      OSDISTRO="rhel"
    elif contains "$distrocfg" "opensuse"; then
      OSDISTRO="opensuse"
    elif contains "$distrocfg" "alpine"; then
      OSDISTRO="alpine"
    elif contains "$distrocfg" "arch"; then
      OSDISTRO="arch"
    elif contains "$distrocfg" "ubuntu|debian"; then
      OSDISTRO="ubuntu"
    else
      OSDISTRO="ubuntu" # default
    fi
  fi

  # For tier-2 platforms, adjust the default version
  adjust_version $OSARCH

  info "Installing koka $VERSION for $OSDISTRO $OSARCH"
}


# ---------------------------------------------------------
# detect previous koka installation
# ---------------------------------------------------------

KOKA_PREV_EXE=
KOKA_PREV_VERSION=
KOKA_PREV_PREFIX=

detect_previous_install() {
  if which koka > /dev/null ; then
    KOKA_PREV_EXE="$(which koka)"
    if [ -e "$KOKA_PREV_EXE" ] ; then
      KOKA_PREV_PREFIX="${KOKA_PREV_EXE%/bin/koka*}"   # remove trailing "/bin/koka*"
      KOKA_PREV_VERSION="$($KOKA_PREV_EXE --version --console=raw)"  # get version info
      KOKA_PREV_VERSION="${KOKA_PREV_VERSION%%,*}"     # remove everything after the first ",*"
      KOKA_PREV_VERSION="v${KOKA_PREV_VERSION#Koka }"  # remove "Koka " prefix
      # echo "found previous koka version $KOKA_PREV_VERSION (installed at: $KOKA_PREV_PREFIX)"
    fi
  fi
}


#---------------------------------------------------------
# Command line options
#---------------------------------------------------------

process_options() {
  while : ; do
    flag="$1"
    case "$flag" in
    *=*)  flag_arg="${flag#*=}";;
    *)    flag_arg="yes" ;;
    esac
    # echo "option: $flag, arg: $flag_arg"
    case "$flag" in
      "") break;;
      -q|--quiet)
          QUIET="yes";;
      -f|--force)
          FORCE="yes";;
      -p) shift
          PREFIX="$1";;
      -p=*|--prefix=*)
          PREFIX=`eval echo $flag_arg`;; # no quotes so ~ gets expanded (issue #412)
      -u=*|--url=*)
          KOKA_DIST_URL="$flag_arg";;
      -b) shift
          KOKA_DIST_SOURCE="$1";;
      -b=*|--bundle=*)
          KOKA_DIST_SOURCE="$flag_arg";;
      -v) shift
          VERSION="v${1#v}";;         # always prefix with a v
      -v=*|--version=*)
          VERSION="v${flag_arg#v}";;  # always prefix with a v
      -m|--minimal)
          MINIMAL="yes";;
      --vscode)
          VSCODE="yes";;
      -u|--uninstall)
          # FORCE="yes"
          MODE="uninstall";;
      -h|--help|-\?|help|\?)
          MODE="help";;
      *) case "$flag" in
           -*) warn "warning: unknown option \"$1\".";;
           *)  KOKA_DIST_SOURCE="$1";;
         esac;;
    esac
    shift
  done

  # adjust x64 arch for older versions to amd64
  case "$VERSION" in
    v2.0.*|v2.1.[0123456])
      case "$OSARCH" in
        *-x64) OSARCH="${OSARCH%-x64}-amd64";;
      esac;;
  esac

  # adjust macos for older versions to osx
  case "$VERSION" in
    v2.0.*|v2.1.[012345678])
      case "$OSARCH" in
        macos-*) OSARCH="osx-${OSARCH#macos-}";;
      esac;;
  esac

  # initialize distribution url
  if [ -z "$KOKA_DIST_URL" ] ; then
    KOKA_DIST_URL="$KOKA_DIST_BASE_URL/$VERSION"
  fi

  if [ -z "$KOKA_DIST_SOURCE" ] ; then
    KOKA_DIST_SOURCE="$KOKA_DIST_URL/koka-$VERSION-$OSARCH.tar.gz"
  fi
}

# ---------------------------------------------------------
# sudo
# ---------------------------------------------------------
USE_SUDO=""    # so only the first sudo forces a prompt.

sudocmd() {
  if [ -z "$USE_SUDO" ] ; then
    # echo "sudo cmd: not set: $USE_SUDO: $@"
    if command -v sudo >/dev/null; then
      info ""
      info "Need to use 'sudo' for further $MODE at: $PREFIX"
      USE_SUDO="always"
      sudo -k  # -k: Disable cached credentials (force prompt for password).
    else
      USE_SUDO="never"
    fi
  fi
  if [ "$USE_SUDO" = "never" ] ; then
    #echo "sudo cmd: never: $USE_SUDO: $@"
    "$@"
  else
    #echo "sudo cmd: always: $USE_SUDO: $@"
    sudo "$@"
  fi
}


# ---------------------------------------------------------
# Install required packages
# ---------------------------------------------------------

apt_get_install() {
  missing=
  for pkg in $*; do
    if ! dpkg -s $pkg 2>/dev/null | grep '^Status:.*installed' >/dev/null; then
      missing="$missing $pkg"
    fi
  done
  if ! [ "$missing" = "" ]; then
    if ! sudocmd apt-get install -y ${QUIET:+-qq}$missing; then
      stop "installing apt packages failed ($@).  Please run 'apt-get update' and try again."
    fi
  fi
}

dnf_install() {
  if ! sudocmd dnf install -y ${QUIET:+-q} "$@"; then
    stop "installing dnf packages failed ($@).  Please run 'dnf check-update' and try again."
  fi
}

dnf_groupinstall() {
  if ! sudocmd dnf groupinstall -y ${QUIET:+-q} "$@"; then
    stop "installing dnf package group failed ($@).  Please run 'dnf check-update' and try again."
  fi
}

pacman_install() {
  if ! sudocmd pacman -S --noconfirm --needed ${QUIET:+-q} "$@"; then
    stop "installing pacman packages failed ($@).  Please run 'pacman -Sy' and try again."
  fi
}

yum_install() {
  if ! sudocmd yum install -y ${QUIET:+-q} "$@"; then
    stop "installing yum packages failed ($@).  Please run 'yum check-update' and try again."
  fi
}

apk_install() {
  if ! sudocmd apk add --update ${QUIET:+-q} "$@"; then
    stop "installing apk packages failed ($@).  Please run 'apk update' and try again."
  fi
}

pkg_install() {
  if ! sudocmd pkg install -y "$@"; then
    stop "installing pkg packages failed ($@).  Please run 'pkg update' and try again."
  fi
}

install_dependencies() {
  info "Installing dependencies.."
  deps="gcc make tar curl cmake ninja-build pkg-config"  # cmake, ninja, and pkg-config are needed by vcpkg
  if has_cmd apt-get ; then
    apt_get_install build-essential $deps
  elif has_cmd dnf ; then
    dnf_groupinstall "Development Tools" # this is for Fedora 32+， CentOS 8 and CentOS Stream
    dnf_install $deps
  elif has_cmd yum ; then
    yum_install build-essential $deps
  elif has_cmd apk ; then
    deps="gcc make tar curl cmake ninja"
    apk_install $deps
  elif has_cmd pacman; then
    deps="gcc make tar curl cmake ninja pkg-config"     # ninja-build -> ninja
    pacman_install base-devel $deps
  else
    case "$OSARCH" in
      macos-*|osx-*)
        ;;  # macos already has all dependencies pre-installed
      *)
        info "Unable to install dependencies; continuing..";;
    esac
  fi
}


#---------------------------------------------------------
# Download distribution
#---------------------------------------------------------

download_failed() { # <program> <url>
  warn ""
  warn "Unable to download: $2"
  warn "  It may be that there is no binary installer available for this platform ($OSARCH)"
  warn "  Either specify another version using the '--version=<version>' flag,"
  warn "  or build Koka from source: <https://github.com/koka-lang/koka/#build-from-source>"
  stop ""
}

download_file() {  # <url|file> <destination file>
  case "$1" in
    ftp://*|http://*|https://*)
      info "Downloading: $1"
      if has_cmd curl ; then
        if ! curl ${QUIET:+-sS} --proto =https --tlsv1.2 -f -L -o "$2" "$1"; then
          download_failed "curl" $1
        fi
      elif has_cmd wget ; then
        if ! wget ${QUIET:+-q} --https-only "-O$2" "$1"; then
          download_failed "wget" $1
        fi
      else
        stop "Neither 'curl' nor 'wget' is available; install one to continue."
      fi;;
    *)
      # echo "cp $1 to $2"
      info "Copying: $1"
      if ! cp $1 $2 ; then
        stop "Unable to copy from $1"
      fi;;
  esac
}

download_available() {  # <url|file>
  case "$1" in
    ftp://*|http://*|https://*)
      if has_cmd curl ; then
        if ! curl -sS --proto =https -L -I "$1" | grep -E "^HTTP/2 200" ; then  # -I is headers only
          return 1
        fi
      fi;;
    *)
      if ! [ -f "$1" ] ; then
        return 1
      fi;;
  esac
  return 0
}

download_dist() {  # <bundle url|file> <destination file>
  download_url="$1"
  if [ -n "$OSDISTRO" ] && [ "$OSNAME" = "linux" ] ;  then
    distro_url=`echo $1 | sed "s/linux/$OSDISTRO/"`
    if download_available "$distro_url" ; then
      info "Using $OSDISTRO bundle"
      download_url="$distro_url"
    else
      info "Using generic linux bundle"
    fi
  fi
  download_file "$download_url" "$2"
}

#-----------------------------------------------------
# Install a distribution
#-----------------------------------------------------

install_dist() {  # <prefix> <version>
  # set parameters
  prefix="$1"
  version="$2"
  koka_share_dir="$prefix/share/koka"
  koka_lib_dir="$prefix/lib/koka"
  koka_bin_dir="$prefix/bin"
  koka_exe="$koka_bin_dir/koka-$version"
  koka_symlink="$koka_bin_dir/koka"

  # download/copy
  download_dist "$KOKA_DIST_SOURCE" "$KOKA_TEMP_DIR/koka-dist.tar.gz"
  info "Unpacking.."
  if ! tar -xzf "$KOKA_TEMP_DIR/koka-dist.tar.gz" -C "$KOKA_TEMP_DIR"; then
    stop "Extraction failed."
  fi

  info "Installing to prefix: $prefix"

  # install the exe and figure out whether to use sudo for the rest
  if [ ! -d "$koka_bin_dir" ] ; then
    if ! mkdir -p "$koka_bin_dir" ; then
      if ! sudocmd mkdir -p "$koka_bin_dir" ; then
        stop "Cannot create $koka_bin_dir installation directory"
      fi
    fi
  fi
  if ! install -c -m 0755 "$KOKA_TEMP_DIR/bin/koka" "$koka_exe" 2>/dev/null; then
    if ! sudocmd install -c -o 0 -g 0 -m 0755 "$KOKA_TEMP_DIR/bin/koka" "$koka_exe"; then
      stop "Installation of koka to $koka_exe has failed"
    fi
  else
    USE_SUDO="never"
  fi
  info "- install executable            : $koka_exe"

  # install symlink
  info "- install executable symlink    : $koka_symlink"
  if [ -e "$koka_symlink" ]; then
    if ! sudocmd rm -f "$koka_symlink"; then
      info "unable to remove old koka executable; continuing.."
    fi
  fi
  if ! sudocmd ln -s "$koka_exe" "$koka_symlink"; then
    info "Unable to create symbolic link to koka-$version executable; continuing.."
  fi

  # copy libraries
  info "- install pre-compiled libraries: $koka_lib_dir/$version"
  if [ -d "$KOKA_TEMP_DIR/lib" ] ; then
    if ! sudocmd cp -p -r "$KOKA_TEMP_DIR/lib" "$prefix/" ; then
      stop "Cannot copy pre-compiled libraries to $KOKA_TEMP_DIR/lib"
    fi
  else
    info "  (generic distribution does not contain precompiled libraries)"
  fi
  info "- install source libraries      : $koka_share_dir/$version"
  if ! sudocmd cp -p -r "$KOKA_TEMP_DIR/share" "$prefix/" ; then
    stop "Cannot copy libraries to $KOKA_TEMP_DIR/share"
  fi

  # if not minimal, install editor integration
  if ! [ "$MINIMAL" = "yes" ]; then
    # install Atom editor support
    if [ -d ~/.atom/packages ] ; then
      koka_atom_dir="$KOKA_TEMP_DIR/share/koka/$version/contrib/atom"
      if [ -d $koka_atom_dir ] ; then
        info "- install atom editor support"
        if [ -d ~/.atom/packages/language-koka ] ; then
          need_restart=""
        else
          mkdir ~/.atom/packages/language-koka
          need_restart="yes"
        fi
        if ! cp -p -r $koka_atom_dir/* ~/.atom/packages/language-koka/ ; then
          info "  (failed to copy atom support files)"
        elif [ ! -z "$need_restart" ] ; then
          info "  Please restart Atom for Koka syntax highlighting to take effect."
        fi
      fi
    fi

    if ! [ "$VSCODE" = "yes" ]; then  # --vscode is passed from the vs code extension
      # install Visual Studio Code editor support
      export NODE_NO_WARNINGS=1
      vscode="code"
      if ! which "$vscode" > /dev/null ; then
        if [ "$(uname)" = "Darwin" ] ; then
          vscode="/Applications/Visual Studio Code.app/Contents/Resources/app/bin/code" # macOS may not have code in the PATH
        fi
      fi
      if which "$vscode" > /dev/null ; then
        info "- install vscode editor support.."
        if "$vscode" --list-extensions | grep "koka-lang.language-koka" > /dev/null ; then
          # already installed
          info "  vscode extension is already installed"
        else
          if ! "$vscode" --force --install-extension koka.language-koka > /dev/null ; then  # latest one from vs code marketplace
            info "  failed to install vscode editor support!"
          fi
        fi
      fi
    fi

    # emacs message
    if which emacs ; then
      info "- emacs syntax mode installed at: $koka_share_dir/$version/contrib/emacs"
    fi
  fi
}


#---------------------------------------------------------
# Uninstall a previous version
#---------------------------------------------------------

uninstall_dist() {  # <prefix> <version>
  # set parameters
  prefix="$1"
  version="$2"
  koka_share_dir="$prefix/share/koka"
  koka_lib_dir="$prefix/lib/koka"
  koka_bin_dir="$prefix/bin"
  koka_exe="$koka_bin_dir/koka-$version"
  koka_symlink="$koka_bin_dir/koka"

  # uninstall share
  info "- uninstall source libraries: $koka_share_dir/$version"
  if [ -d "$koka_share_dir/$version" ] ; then
    if ! rm -rf "$koka_share_dir/$version" 2>/dev/null ; then
      if ! sudocmd rm -rf "$koka_share_dir/$version" ; then
        info "Unable to remove $koka_share_dir/$version; continuing.."
      fi
    fi
    if [ -z "$(ls -A $koka_share_dir)" ]; then
      info "- remove $koka_share_dir"
      sudocmd rmdir "$koka_share_dir" 2>/dev/null # remove if empty
    fi
  else
    info "  (already uninstalled)"
  fi

  # uninstall lib
  info "- uninstall pre-compiled libraries: $koka_lib_dir/$version"
  if [ -d "$koka_lib_dir/$version" ] ; then
    if ! rm -rf "$koka_lib_dir/$version" 2>/dev/null ; then
      if ! sudocmd rm -rf "$koka_lib_dir/$version" ; then
        info "Unable to remove $koka_lib_dir/$version; continuing.."
      fi
    fi
    if [ -z "$(ls -A $koka_lib_dir)" ]; then
      info "- remove $koka_lib_dir"
      sudocmd rmdir "$koka_lib_dir" 2>/dev/null # remove if empty
    fi
  else
    info "  (already uninstalled)"
  fi


  # uninstall executable
  info "- uninstall executable: $koka_exe"
  if [ -f "$koka_exe" ] ; then
    if ! rm -f "$koka_exe" 2>/dev/null ; then
      if ! sudocmd rm -f "$koka_exe" ; then
        info "Unable to remove $koka_exe; continuing.."
      fi
    fi
  else
    info "  (already uninstalled)"
  fi

  if [ -L "$koka_symlink" ] ; then
    symlink_target="`readlink $koka_symlink`"
    # echo "links to: $symlink_target vs. $koka_exe"
    if [ "$symlink_target" = "$koka_exe" ] ; then
      info "- uninstall symbolic link: $koka_symlink"
      if ! rm -f "$koka_symlink" 2>/dev/null ; then
        if ! sudocmd rm -f "$koka_symlink" ; then
          info "Unable to remove $koka_symlink; continuing.."
        fi
      fi
    fi
  fi
}

#---------------------------------------------------------
# Main
#---------------------------------------------------------

main_uninstall() {
  # confirm uninstall
  if [ -z "$FORCE" ] ; then
    read -p "Uninstalling koka version $VERSION. Are you sure? [yN] " choice </dev/tty
    case $choice in
      [yY][eE][sS]|[yY])
         info "Uninstalling..";;
      *) info "No"
         stop "Uninstall canceled";;
    esac
  fi

  # uninstall
  uninstall_dist $PREFIX $VERSION
  info ""
  info "--------------------------------------------------"
  info "Uninstall successful of $PREFIX/bin/koka-$VERSION"
  info ""
}

main_install() {
  # install
  install_dependencies
  make_temp_dir
  trap cleanup_temp_dir EXIT
  install_dist $PREFIX $VERSION
  info "Install successful."

  # remove previous install?
  if [ ! -z "$KOKA_PREV_VERSION" ] && [ ! "$KOKA_PREV_VERSION" = "$VERSION" ] ; then
    info ""
    if [ -z "$FORCE" ] ; then
      info "Found previous koka version $KOKA_PREV_VERSION."
      read -p "Would you like to remove this version? [yN] " choice </dev/tty
    else
      choice="Y"
    fi
    case $choice in
      [yY][eE][sS]|[yY])
        info "Uninstalling previous koka version $KOKA_PREV_VERSION"
        uninstall_dist $KOKA_PREV_PREFIX $KOKA_PREV_VERSION
        info "Uninstall successful of koka $KOKA_PREV_VERSION";;
      *)
        info "Uninstall of previous koka version is canceled";;
    esac
  elif [ "$KOKA_PREV_PREFIX,$KOKA_PREV_VERSION" = "$PREFIX,$VERSION" ] ; then
    info "Updated koka version $VERSION in-place"
  fi

  info ""
  info "--------------------------------------------------"
  info "Installed Koka $VERSION at $PREFIX/bin/koka"
  info ""

  if ! [ "$VSCODE" = "yes" ]; then
    info "Type 'koka' to enter the interactive compiler"
    info ""
  fi
}


main_help() {
  info "command:"
  info "  ./install.sh [options] [bundle file]"
  info ""
  info "options:"
  info "  -q, --quiet              suppress output"
  info "  -f, --force              continue without prompting"
  info "  -u, --uninstall          uninstall koka ($VERSION)"
  info "  -p, --prefix=<dir>       prefix directory ($PREFIX)"
  # info "  -b, --bundle=<file|url>  full bundle location (.../koka-$VERSION-$OSARCH.tar.gz)"
  info "      --vscode             set when installing from within vscode"
  info "  -m, --minimal            minimal install without editor support etc."
  info "      --version=<ver>      version tag ($VERSION)"
  info "      --url=<url>          download url"
  info "                           ($KOKA_DIST_URL)"
  info ""
  info "notes:"
  info "  the bundle file can optionally be given explicitly, e.g.: bundle/koka-$VERSION-$OSARCH.tar.gz"
  info ""
}

main_start() {
  detect_osarch
  detect_previous_install
  process_options $@
  if [ "$MODE" = "help" ] ; then
    main_help
  elif [ "$MODE" = "uninstall" ] ; then
    main_uninstall
  else
    main_install
  fi
}

# note: only start executing commands now to guard against partial downloads
main_start $@

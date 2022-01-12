#!/bin/sh -e
#
# Stack installation script.
#
# This script is meant for quick & easy install via:
#   'curl -sSL https://get.haskellstack.org/ | sh'
# or:
#   'wget -qO- https://get.haskellstack.org/ | sh'
#
# By default, this installs 'stack' to '/usr/local/bin'.
#
# Arguments (use `... | sh -s - ARGUMENTS`)
#
# -q: reduce script's output
# -f: force over-write even if 'stack' already installed
# -d DESTDIR: change destination directory
#
# Make pull requests at:
# https://github.com/commercialhaskell/stack/blob/master/etc/scripts/get-stack.sh
#
# Note that this script will ask for root access using `sudo` in order to use
# your platform's package manager to install dependencies and to install to
# `/usr/local/bin`.  If you prefer more control, follow the manual
# installation instructions for your platform at:
# https://docs.haskellstack.org/en/stable/install_and_upgrade/
#

HOME_LOCAL_BIN="$HOME/.local/bin"
DEFAULT_DEST="/usr/local/bin/stack"
# Windows doesn't have a good place for DEST, but all CI systems (Appveyor, Travis, Azure) support /bin
DEFAULT_DEST_WINDOWS="/bin/stack"
DEST=""
QUIET=""
FORCE=""
STACK_TEMP_DIR=

INSTALL_VERSION="2.7.1"
BASE_URL="https://github.com/commercialhaskell/stack/releases/download/v$INSTALL_VERSION/stack-$INSTALL_VERSION-"

# creates a temporary directory, which will be cleaned up automatically
# when the script finishes
make_temp_dir() {
  STACK_TEMP_DIR="$(mktemp -d 2>/dev/null || mktemp -d -t stack)"
}

# cleanup the temporary directory if it's been created.  called automatically
# when the script exits.
cleanup_temp_dir() {
  if [ -n "$STACK_TEMP_DIR" ] ; then
    rm -rf "$STACK_TEMP_DIR"
    STACK_TEMP_DIR=
  fi
}

# print a message to stderr and exit with error code
die() {
  echo "$@" >&2
  exit 1
}

# print a message to stdout unless '-q' passed to script
info() {
  if [ -z "$QUIET" ] ; then
    echo "$@"
  fi
}

# print a separator for post-install messages
post_install_separator() {
  info ""
  info "-------------------------------------------------------------------------------"
  info ""
}

# determines the the CPU's instruction set
get_isa() {
  if arch | grep -Eq 'armv[78]l?' ; then
    echo arm
  elif arch | grep -q aarch64 ; then
    echo aarch64
  else
    echo x86
  fi
}

# exits with code 0 if arm ISA is detected as described above
is_arm() {
  test "$(get_isa)" = arm
}

# exits with code 0 if aarch64 ISA is detected as described above
is_aarch64() {
  test "$(get_isa)" = aarch64
}


# determines 64- or 32-bit architecture
# if getconf is available, it will return the arch of the OS, as desired
# if not, it will use uname to get the arch of the CPU, though the installed
# OS could be 32-bits on a 64-bit CPU
get_arch() {
  if has_getconf ; then
    if getconf LONG_BIT | grep -q 64 ; then
      echo 64
    else
      echo 32
    fi
  else
    case "$(uname -m)" in
      *64)
        echo 64
        ;;
      *)
        echo 32
        ;;
    esac
  fi
}

# exits with code 0 if a x86_64-bit architecture is detected as described above
is_x86_64() {
  test "$(get_arch)" = 64 -a "$(get_isa)" = "x86"
}

# prints a generic bindist notice
print_bindist_notice() {
  if [ -z "$1" ] ; then
    info ""
    info "Using generic bindist..."
    info ""
  else
    info ""
    info "Using generic $1 bindist..."
    info ""
  fi
}

# Adds a 'sudo' prefix if sudo is available to execute the given command
# If not, the given command is run as is
# When requesting root permission, always show the command and never re-use cached credentials.
sudocmd() {
  reason="$1"; shift
  if command -v sudo >/dev/null; then
    echo
    echo "About to use 'sudo' to run the following command as root:"
    echo "    $@"
    echo "in order to $reason."
    echo
    # -k: Disable cached credentials (force prompt for password).
    sudo -k "$@"
  else
    "$@"
  fi
}

# Install dependencies for distros that use Apt
apt_install_dependencies() {
    info "Installing dependencies..."
    info ""
    apt_get_install_pkgs "$@"
}

# Attempts an install on Ubuntu via apt, if possible
# Expects the version (in Major.Minor format, with any sub-minor removed)
# as the first and only argument
# If the version of Ubuntu is unsupported, it attempts to copy the binary
# and install the necessary dependencies explicitly.
do_ubuntu_install() {

  install_dependencies() {
    apt_install_dependencies g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase
  }

  if is_arm ; then
    install_dependencies
    print_bindist_notice
    install_arm_linux_binary
  elif is_aarch64 ; then
    install_dependencies
    print_bindist_notice
    install_aarch64_linux_binary
  elif is_x86_64 ; then
    install_dependencies
    print_bindist_notice
    install_x86_64_linux_binary
  else
    die "Sorry, currently only 64-bit (x86_64) Linux binary is available."
    #install_dependencies
    #print_bindist_notice
    #install_i386_linux_binary
  fi

}

# Attempts an install on Debian.
# Expects the single-number version as the first and only argument
# If the version of Debian is unsupported, it attempts to copy the binary
# and install the necessary dependencies explicitly.
do_debian_install() {

  install_dependencies() {
    apt_install_dependencies g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase
  }

  if is_arm ; then
    install_dependencies
    print_bindist_notice
    install_arm_linux_binary
  elif is_aarch64 ; then
    install_dependencies
    print_bindist_notice
    install_aarch64_linux_binary
  elif is_x86_64 ; then
    install_dependencies
    print_bindist_notice
    install_x86_64_linux_binary
  else
    die "Sorry, currently only 64-bit (x86_64) Linux binary is available."
  #  install_dependencies
  #  print_bindist_notice
  #  install_i386_linux_binary
  fi
}

# Attempts an install on Fedora.
# Expects the single-number version as the first and only argument
# If the version of Fedora is unsupported, it attempts to copy the binary
# and install the necessary dependencies explicitly.
do_fedora_install() {
  install_dependencies() {
    dnf_install_pkgs perl make automake gcc gmp-devel libffi zlib-devel xz tar git gnupg
  }

  if is_arm ; then
    install_dependencies "$1"
    print_bindist_notice
    install_arm_linux_binary
  elif is_aarch64 ; then
    install_dependencies "$1"
    print_bindist_notice
    install_aarch64_linux_binary
  elif is_x86_64 ; then
    install_dependencies "$1"
    print_bindist_notice
    install_x86_64_linux_binary
  else
    die "Sorry, currently only 64-bit (x86_64) Linux binary is available."
    #install_dependencies "$1"
    #print_bindist_notice
    #install_i386_linux_binary
  fi
}

# Attempts an install on CentOS.
# Expects the single-number version as the first and only argument
# If the version of CentOS is unsupported, it attempts to copy the binary
# and install the necessary dependencies explicitly.
do_centos_install() {
  install_dependencies() {
    yum_install_pkgs perl make automake gcc gmp-devel libffi zlib xz tar git gnupg
  }

  if is_arm ; then
    install_dependencies
    print_bindist_notice
    install_arm_linux_binary
  elif is_aarch64 ; then
    install_dependencies
    print_bindist_notice
    install_aarch64_linux_binary
  elif is_x86_64 ; then
    install_dependencies
    print_bindist_notice
    install_x86_64_linux_binary
  else
    die "Sorry, currently only 64-bit (x86_64) Linux binary is available."
    #case "$1" in
    #  "6"*)
    #    die "Sorry, there is currently no Linux 32-bit gmp4 binary available."
    #    ;;
    #  *)
    #    install_dependencies
    #    print_bindist_notice
    #    install_i386_linux_binary
    #    ;;
    #esac
  fi
}

# Attempts to install on Windows, designed for CI scripts (tested on Appveyor, Travis, Azure)
do_windows_install() {
  info "Using Windows install.."
  info ""
  make_temp_dir
  dl_to_file "https://get.haskellstack.org/stable/windows-x86_64.zip" "$STACK_TEMP_DIR/stack.zip"
  if [ "$(basename $DEST)" != "stack" ]; then
    # should never happen, the -d flag appends stack itself
    die "Currently the destination must always end with 'stack' on Windows, got: $DEST"
  fi
  if ! 7z x $STACK_TEMP_DIR/stack.zip stack.exe "-o$(dirname $DEST)" -y; then
    die "Extract zip file failed, you probably don't have 7z installed"
  fi
  post_install_separator
  info "Stack has been installed to: $DEST"
  info ""
  check_dest_on_path
}

# Attempts to install on macOS.
# If 'brew' exists, installs using Homebrew.  Otherwise, installs
# the generic bindist.
do_osx_install() {
  info "Using generic bindist..."
  info ""
  install_64bit_osx_binary
  info "NOTE: You may need to run 'xcode-select --install' and/or"
  info "      'open /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg'"
  info "      to set up the Xcode command-line tools, which Stack uses."
  info ""
}

# # Attempts to insall on FreeBSD.  Installs dependencies with
# # 'pkg install' and then downloads bindist.
# do_freebsd_install() {
#   install_dependencies() {
#     pkg_install_pkgs devel/gmake perl5 lang/gcc misc/compat8x misc/compat9x converters/libiconv ca_root_nss
#   }
#   if is_64_bit ; then
#     install_dependencies
#     install_64bit_freebsd_binary
#   else
#     die "Sorry, there is currently no 32-bit FreeBSD binary available."
#   fi
# }

# Alpine distro install
do_alpine_install() {
  install_dependencies() {
    apk_install_pkgs gmp libgcc xz make
  }
  install_dependencies
  if is_arm ; then
    print_bindist_notice
    install_arm_linux_binary
  elif is_aarch64 ; then
    print_bindist_notice
    install_aarch64_linux_binary
  elif is_x86_64 ; then
    print_bindist_notice
    install_x86_64_linux_binary
  else
    die "Sorry, currently only 64-bit (x86_64) Alpine Linux binary is available."
  fi
}

# Attempts to install on unsupported Linux distribution by downloading
# the bindist.
do_sloppy_install() {
  info "This installer doesn't support your Linux distribution, trying generic"
  info "bindist..."
  info ""

  if is_arm ; then
    install_arm_linux_binary
  elif is_aarch64 ; then
    install_aarch64_linux_binary
  elif is_x86_64 ; then
    install_x86_64_linux_binary
  else
    die "Sorry, currently only 64-bit (x86_64) Linux binary is available."
    #install_i386_linux_binary
  fi
  info "Since this installer doesn't support your Linux distribution,"
  info "there is no guarantee that 'stack' will work at all!  You may"
  info "need to manually install some system info dependencies for GHC:"
  info "  gcc, make, libffi, zlib, libgmp and libtinfo"
  info "Please see http://docs.haskellstack.org/en/stable/install_and_upgrade/"
  info "Pull requests to add support for this distro would be welcome!"
  info ""
}

# Attempts to determine the running Linux distribution.
# Prints "DISTRO;VERSION" (distribution name and version)"."
distro_info() {
  parse_lsb() {
    lsb_release -a 2> /dev/null | perl -ne "$1"
  }

  try_lsb() {
    if has_lsb_release ; then
      TL_DIST="$(parse_lsb 'if(/Distributor ID:\s+([^ ]+)/) { print "\L$1"; }')"
      TL_VERSION="$(parse_lsb 'if(/Release:\s+([^ ]+)/) { print "\L$1"; }')"
      echo "$TL_DIST;$TL_VERSION"
    else
      return 1
    fi
  }

  try_release() {
    parse_release() {
      perl -ne "$1" /etc/*release 2>/dev/null
    }

    parse_release_id() {
      parse_release 'if(/^(DISTRIB_)?ID\s*=\s*"?([^"]+)/) { print "\L$2"; exit 0; }'
    }

    parse_release_version() {
      parse_release 'if(/^(DISTRIB_RELEASE|VERSION_ID)\s*=\s*"?([^"]+)/) { print $2; exit 0; }'
    }

    TR_RELEASE="$(parse_release_id);$(parse_release_version)"

    if [ ";" = "$TR_RELEASE" ] ; then
      if [ -e /etc/arch-release ] ; then
        # /etc/arch-release exists but is often empty
        echo "arch;"
      elif [ -e /etc/centos-release ] && grep -q "\<6\>" /etc/centos-release ; then
        # /etc/centos-release has a non-standard format before version 7
        echo "centos;6"
      else
        return 1
      fi
    else
      echo "$TR_RELEASE"
    fi
  }

  try_issue() {
    case "$(cat /etc/issue 2>/dev/null)" in
      "Arch Linux"*)
        echo "arch;" # n.b. Version is not available in /etc/issue on Arch
        ;;
      "Ubuntu"*)
        echo "ubuntu;$(perl -ne 'if(/Ubuntu (\d+\.\d+)/) { print $1; }' < /etc/issue)"
        ;;
      "Debian"*)
        echo "debian;$(perl -ne 'if(/Debian GNU\/Linux (\d+(\.\d+)?)/) { print $1; }' < /etc/issue)"
        ;;
      *"SUSE"*)
        echo "suse;$(perl -ne 'if(/SUSE\b.* (\d+\.\d+)/) { print $1; }' < /etc/issue)"
        ;;
      *"NixOS"*)
        echo "nixos;$(perl -ne 'if(/NixOS (\d+\.\d+)/) { print $1; }' < /etc/issue)"
        ;;
      "CentOS"*)
        echo "centos;$(perl -ne 'if(/^CentOS release (\d+)\./) { print $1; }' < /etc/issue)"
        ;;
      *)
    esac
    # others do not output useful info in issue, return empty
  }

  try_lsb || try_release || try_issue
}

# Attempt to install on a Linux distribution
do_distro() {
  if ! has_perl ; then
    if ! try_install_pkgs perl ; then
      #TODO: remove dependence on 'perl', which is not installed by default
      #on some distributions (Fedora and RHEL, in particular).
      die "This script requires 'perl', please install it to continue."
    fi
  fi

  IFS=";" read -r DISTRO VERSION <<GETDISTRO
$(distro_info)
GETDISTRO

  if [ -n "$DISTRO" ] ; then
    info "Detected Linux distribution: $DISTRO"
    info ""
  fi

  case "$DISTRO" in
    ubuntu|linuxmint|elementary|neon|pop)
      do_ubuntu_install "$VERSION"
      ;;
    debian|kali|raspbian|mx)
      do_debian_install "$VERSION"
      ;;
    fedora)
      do_fedora_install "$VERSION"
      ;;
    centos|rhel|redhatenterpriseserver)
      do_centos_install "$VERSION"
      ;;
    alpine)
      do_alpine_install "$VERSION"
      ;;
    *)
      do_sloppy_install
  esac
}

set_default_dest() {
  [ "$DEST" != "" ] || DEST="$DEFAULT_DEST"
}

# Determine operating system and attempt to install.
do_os() {
  case "$(uname)" in
    "Linux")
      set_default_dest
      do_distro
      ;;
    "Darwin")
      set_default_dest
      do_osx_install
      ;;
    #"FreeBSD")
    #  set_default_dest
    #  do_freebsd_install
    #  ;;
    MINGW64_NT-*|MSYS_NT-*)
      DEFAULT_DEST="$DEFAULT_DEST_WINDOWS"
      set_default_dest
      do_windows_install
      ;;
    *)
      die "Sorry, this installer does not support your operating system: $(uname).
See http://docs.haskellstack.org/en/stable/install_and_upgrade/"
  esac
}

# Download a URL to file using 'curl' or 'wget'.
dl_to_file() {
  if has_curl ; then
    if ! curl ${QUIET:+-sS} -L -o "$2" "$1"; then
      die "curl download failed: $1"
    fi
  elif has_wget ; then
    if ! wget ${QUIET:+-q} "-O$2" "$1"; then
      die "wget download failed: $1"
    fi
  else
    # should already have checked for this, otherwise this message will probably
    # not be displayed, since dl_to_stdout will be part of a pipeline
    die "Neither wget nor curl is available, please install one to continue."
  fi
}

# Check for 'curl' or 'wget' and attempt to install 'curl' if neither found,
# or fail the script if that is not possible.
check_dl_tools() {
  if ! has_curl && ! has_wget ; then
    if ! try_install_pkgs curl ; then
      die "Neither wget nor curl is available, please install one to continue."
    fi
  fi
}

# Download a Stack bindist and install it in /usr/local/bin/stack.
install_from_bindist() {
    IFB_URL="$BASE_URL$1"
    check_dl_tools
    make_temp_dir

    dl_to_file "$IFB_URL" "$STACK_TEMP_DIR/$1.bindist"
    mkdir -p "$STACK_TEMP_DIR/$1"
    if ! tar xzf "$STACK_TEMP_DIR/$1.bindist" -C "$STACK_TEMP_DIR/$1"; then
      die "Extract bindist failed"
    fi
    STACK_TEMP_EXE="$STACK_TEMP_DIR/$(basename "$DEST")"
    mv "$STACK_TEMP_DIR/$1"/*/stack "$STACK_TEMP_EXE"
    destdir="$(dirname "$DEST")"
    if [ ! -d "$destdir" ]; then
        info "$destdir directory does not exist; creating it..."
        # First try to create directory as current user, then try with sudo if it fails.
        if ! mkdir -p "$destdir" 2>/dev/null; then
            if ! sudocmd "create the destination directory" mkdir -p "$destdir"; then
                die "Could not create directory: $DEST"
            fi
        fi
    fi
    # First attempt to install 'stack' as current user, then try with sudo if it fails
    info "Installing Stack to: $DEST..."
    if ! install -c -m 0755 "$STACK_TEMP_EXE" "$destdir" 2>/dev/null; then
      if ! sudocmd "copy 'stack' to the destination directory" install -c -o 0 -g 0 -m 0755 "$STACK_TEMP_EXE" "$destdir"; then
        die "Install to $DEST failed"
      fi
    fi

    post_install_separator
    info "Stack has been installed to: $DEST"
    info ""

    check_dest_on_path
}

install_arm_linux_binary() {
 install_from_bindist "linux-arm.tar.gz"
}

install_i386_linux_binary() {
 install_from_bindist "linux-i386.tar.gz"
}

install_x86_64_linux_binary() {
  install_from_bindist "linux-x86_64.tar.gz"
}

install_aarch64_linux_binary() {
 install_from_bindist "linux-aarch64.tar.gz"
}

install_64bit_osx_binary() {
  install_from_bindist "osx-x86_64.tar.gz"
}

#install_64bit_freebsd_binary() {
#  install_from_bindist "freebsd-x86_64.tar.gz"
#}

# Attempt to install packages using whichever of apt-get, dnf, yum, or apk is
# available.
try_install_pkgs() {
  if has_apt_get ; then
    apt_get_install_pkgs "$@"
  elif has_dnf ; then
    dnf_install_pkgs "$@"
  elif has_yum ; then
    yum_install_pkgs "$@"
  elif has_apk ; then
    apk_install_pkgs "$@"
  else
    return 1
  fi
}

# Install packages using apt-get
apt_get_install_pkgs() {
  missing=
  for pkg in $*; do
    if ! dpkg -s $pkg 2>/dev/null |grep '^Status:.*installed' >/dev/null; then
      missing="$missing $pkg"
    fi
  done
  if [ "$missing" = "" ]; then
    info "Already installed!"
  elif ! sudocmd "install required system dependencies" apt-get install -y ${QUIET:+-qq}$missing; then
    die "\nInstalling apt packages failed.  Please run 'apt-get update' and try again."
  fi
}

# Install packages using dnf
dnf_install_pkgs() {
  if ! sudocmd "install required system dependencies" dnf install -y ${QUIET:+-q} "$@"; then
    die "\nInstalling dnf packages failed.  Please run 'dnf check-update' and try again."
  fi
}

# Install packages using yum
yum_install_pkgs() {
  if ! sudocmd "install required system dependencies" yum install -y ${QUIET:+-q} "$@"; then
    die "\nInstalling yum packages failed.  Please run 'yum check-update' and try again."
  fi
}

# Install packages using apk
apk_install_pkgs() {
  if ! sudocmd "install required system dependencies" apk add --update ${QUIET:+-q} "$@"; then
    die "\nInstalling apk packages failed.  Please run 'apk update' and try again."
  fi
}

# Install packages using pkg
pkg_install_pkgs() {
    if ! sudocmd "install required system dependencies" pkg install -y "$@"; then
        die "\nInstalling pkg packages failed.  Please run 'pkg update' and try again."
    fi
}

# Get installed Stack version, if any
installed_stack_version() {
  stack --version | grep -o 'Version \([[:digit:]]\|\.\)\+' | tr A-Z a-z
}

# Get installed Stack's path
stack_location() {
  command -v stack
}

# Check whether 'stack' command exists
has_stack() {
  if [ "$DEST" != "" ] ; then
    has_cmd "$DEST"
  else
    has_cmd stack
  fi
}

# Check whether 'wget' command exists
has_wget() {
  has_cmd wget
}

# Check whether 'curl' command exists
has_curl() {
  has_cmd curl
}

# Check whether 'lsb_release' command exists
has_lsb_release() {
  has_cmd lsb_release
}

# Check whether 'sudo' command exists
has_sudo() {
  has_cmd sudo
}

# Check whether 'getconf' command exists
has_getconf() {
  has_cmd getconf
}

# Check whether 'brew' command exists
has_brew() {
  has_cmd brew
}

# Check whether 'perl' command exists
has_perl() {
  has_cmd perl
}

# Check whether 'apt-get' command exists
has_apt_get() {
  has_cmd apt-get
}

# Check whether 'yum' command exists
has_yum() {
  has_cmd yum
}

# Check whether 'apk' command exists
has_apk() {
  has_cmd apk
}

# Check whether 'dnf' command exists
has_dnf() {
  has_cmd dnf
}

# Check whether the given command exists
has_cmd() {
  command -v "$1" > /dev/null 2>&1
}

# Check whether the given path is listed in the PATH environment variable
on_path() {
  echo ":$PATH:" | grep -q :"$1":
}

# Check whether ~/.local/bin is on the PATH, and print a warning if not.
check_home_local_bin_on_path() {
  if ! on_path "$HOME_LOCAL_BIN" ; then
    #TODO: offer to add it for the user (pull requests welcome!)
    info "WARNING: '$HOME_LOCAL_BIN' is not on your PATH."
    info "    Stack will place the binaries it builds in '$HOME_LOCAL_BIN' so"
    info "    for best results, please add it to the beginning of PATH in your profile."
    info ""
  fi
}

# Check whether $DEST is on the PATH, and print a warning if not.
check_dest_on_path() {
  if ! on_path "$(dirname $DEST)" ; then
    info "WARNING: '$(dirname $DEST)' is not on your PATH."
    info ""
  fi
}

# Check whether Stack is already installed, and print an error if it is.
check_stack_installed() {
  if has_stack ; then
    if [ "$FORCE" = "true" ] ; then
      [ "$DEST" != "" ] || DEST="$(stack_location)"
    else
      if has_curl; then
        get="curl -sSL"
      else
        get="wget -qO-"
      fi
      [ "$DEST" != "" ] && location=$(realpath "$DEST") || location=$(stack_location)
      die "Stack $(installed_stack_version) already appears to be installed at:
  $location

Use 'stack upgrade' or your OS's package manager to upgrade,
or pass '-f' to this script to over-write the existing binary, e.g.:
  $get https://get.haskellstack.org/ | sh -s - -f

To install to a different location, pass '-d DESTDIR', e.g.:
  $get https://get.haskellstack.org/ | sh -s - -d /opt/stack/bin"
    fi
  fi
}

trap cleanup_temp_dir EXIT

while [ $# -gt 0 ]; do
  case "$1" in
    -q|--quiet)
      # This tries its best to reduce output by suppressing the script's own
      # messages and passing "quiet" arguments to tools that support them.
      QUIET="true"
      shift
      ;;
    -f|--force)
      FORCE="true"
      shift
      ;;
    -d|--dest)
      DEST="$2/stack"
      shift 2
      ;;
    *)
      echo "Invalid argument: $1" >&2
      exit 1
      ;;
  esac
done

check_stack_installed
do_os
check_home_local_bin_on_path

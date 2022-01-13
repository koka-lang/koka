# Build installable package

> The package builder should be fully functional. But if you encounter any errors please submit an issue.

The `package.sh` script automatically configures various Docker images and
generates from that various Koka installation packages for various Linux
platforms.

Currently this option is x64 linux only, support for arm64 linux is being worked on.

Right now you can generate packages that work for these distributions:

- Fedora 34, 35
- RHEL 8
- Debian 10, 11
- Ubuntu 18.04, 20.04, 22.04
- PopOS 20.04, 21.10
- Linux Mint 19.3, 20.2
- Arch Linux
- OpenSUSE Tumbleweed, Leap 15.3

There might be more that are also compatible, so feel free to experiment.

## Requirements

- Docker or Podman
- tar
- bash

## How to build

To build every possible package automatically you can run this script from the root of this repository.

```sh
$ ./util/packaging/build.sh
```

To specify which versions you want to build and package you can use `--target="target1,target2"`.
You can also specify whether to only build or only package with `--package="no"` or `--package="only"`.

After running the script there should now be distro specific bundles in `./bundle/$version/archives`, and installable packages in `./bundle/$version/packages`.

## Notes

If OpenSuse throws this when trying to install the built package

```sh
# zypper in -t package /data/koka-2.3.7-opensuse.rpm
Loading repository data...
Reading installed packages...
'_tmpRPMcache_:koka=0:2.3.7-1' not found in package names. Trying capabilities.
No provider of '_tmpRPMcache_:koka=0:2.3.7-1' found.
Resolving package dependencies...
Nothing to do.
```

try

```sh
# zypper ref -f
```

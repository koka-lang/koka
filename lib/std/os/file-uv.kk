/*---------------------------------------------------------------------------
  Copyright 2012-2023, Microsoft Research, Daan Leijen. Brigham Young University, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module std/os/file-uv
pub import std/os/uv
pub import std/num/int32
pub import std/num/int64

extern import
  c file "file-uv-inline.c"

pub value struct timespec
  sec: int64
  nsec: int32;

pub fun show(t: timespec): string
  t.sec.show ++ "." ++ t.nsec.show.pad-left(9, '0')

pub struct fstat
  dev: int64
  mode: int64
  nlink: int64
  uid: int64
  gid: int64
  rdev: int64
  ino: int64
  size: int64
  blksize: int64
  blocks: int64
  flags: int64
  atim: timespec
  mtim: timespec
  ctim: timespec
  birthtim: timespec

pub fun show(f: fstat): string
  "{dev=" ++ f.dev.show ++ ", mode=" ++ f.mode.show ++ ", nlink=" ++ f.nlink.show ++ ", uid=" ++ f.uid.show ++
   ", gid=" ++ f.gid.show ++ ", rdev=" ++ f.rdev.show ++ ", ino=" ++ f.ino.show ++ ", size=" ++ f.size.show ++ 
   ", blksize=" ++ f.blksize.show ++ ", blocks=" ++ f.blocks.show ++ ", flags=" ++ f.flags.show ++ ", atim=" ++ 
   f.atim.show ++ ", mtim=" ++ f.mtim.show ++ ", ctim=" ++ f.ctim.show ++ ", birthtim=" ++ f.birthtim.show ++ "}"

pub struct statfs
  filetype: int64
  bsize: int64
  blocks: int64
  bfree: int64
  bavail: int64
  files: int64
  ffree: int64
  fspare: vector<int64> // Length 4 Convert to tuple?

pub fun show(f: statfs): string
  "{filetype=" ++ f.filetype.show ++ ", bsize=" ++ f.bsize.show ++ ", blocks=" ++ f.blocks.show ++ ", bfree=" ++ 
  f.bfree.show ++ ", bavail=" ++ f.bavail.show ++ ", files=" ++ f.files.show ++ ", ffree=" ++ f.ffree.show ++ 
  ", fspare=[" ++ f.fspare.map(fn(s) s.show).list.join(",") ++ "]}"

type dirent-type
  UNKNOWN_DIRECTORY_ENTRY
  FILE
  DIR
  LINK
  FIFO
  SOCKET
  CHAR
  BLOCK

pub value struct dirent
  name: string
  entity-type: dirent-type;

pub alias create-mode = int32
// Octal 0o700 // Read, write, execute by owner.
pub val s_IRWXU = (7 * 64 + 0 + 0).int32 
// Octal 0o400 // Read permission, owner.
pub val s_IRUSR = (4 * 64 + 0 + 0).int32 
// Octal 0o200 // Write permission, owner.
pub val s_IWUSR = (2 * 64 + 0 + 0).int32 
// Octal 0o100 // Execute/search permission, owner.
pub val s_IXUSR = (1 * 64 + 0 + 0).int32 
// Octal 0o070 // Read, write, execute by group.
pub val s_IRWXG = (0 + 7 * 8 + 0).int32 
// Octal 0o040 // Read permission, group
pub val s_IRGRP = (0 + 4 * 8 + 0).int32 
// Octal 0o020 // Write permission, group..
pub val s_IWGRP = (0 + 2 * 8 + 0).int32 
// Octal 0o010 // Execute/search permission, group.
pub val s_IXGRP = (0 + 1 * 8 + 0).int32 
// Octal 0o007 // Read, write, execute by others.
pub val s_IRWXO = (0 + 0 + 7).int32 
// Octal 0o004 // Read permission, others.
pub val s_IROTH = (0 + 0 + 4).int32 
// Octal 0o002 // Write permission, others.
pub val s_IWOTH = (0 + 0 + 2).int32 
// Octal 0o001 // Execute/search permission, others.
pub val s_IXOTH = (0 + 0 + 1).int32 

// Octal 0o4000 // Set-user-ID
pub val s_ISUID = (4 * 512 + 0 +  0 + 0).int32
// Octal 0o2000 // Set-group-ID (see inode(7))
pub val s_ISGID = (0 + 2 * 512 + 0 + 0).int32 
// Octal 0o1000 // Sticky bit (see inode(7))
pub val s_ISVTX = (0 + 0 + 1 * 512 + 0).int32 

// Read-only access.
pub val o_RDONLY = oRDONLY()
extern oRDONLY(): int32
  c inline "UV_FS_O_RDONLY"
// Write-only access. 
pub val o_WRONLY = oWRONLY()
extern oWRONLY(): int32
  c inline "UV_FS_O_WRONLY"
// Read-write access.
pub val o_RDWR = oRDWR()
extern oRDWR(): int32
  c inline "UV_FS_O_RDWR"
// Create the file if it does not exist. 
pub val o_CREAT = oCREAT()
extern oCREAT(): int32
  c inline "UV_FS_O_CREAT"
// If pathname already exists, then fail the open with the error EEXIST.
pub val o_EXCL = oEXCL()
extern oEXCL(): int32
  c inline "UV_FS_O_EXCL"
// If pathname refers to a terminal device, don't allocate controlling terminal for this process.
pub val o_NOCTTY = oNOCTTY()
extern oNOCTTY(): int32
  c inline "UV_FS_O_NOCTTY"
// If the file exists and is a regular file, and the file is successfully opened O_RDWR or O_WRONLY, its length shall be truncated to 0. (Octal 0o1000)
pub val o_TRUNC = oTRUNC()
extern oTRUNC(): int32
  c inline "UV_FS_O_TRUNC"
// Before each write(2), the file offset is positioned at the end of the file, as if with lseek(2).
pub val o_APPEND = oAPPEND()
extern oAPPEND(): int32
  c inline "UV_FS_O_APPEND"
// Open the file in nonblocking mode if possible.
pub val o_NONBLOCK = oNONBLOCK()
extern oNONBLOCK(): int32
  c inline "UV_FS_O_NONBLOCK"
// The file is opened for synchronous I/O. Write operations will complete once all data and a minimum of metadata are flushed to disk.
pub val o_DSYNC = oDSYNC()
extern oDSYNC(): int32
  c inline "UV_FS_O_DSYNC"
// direct disk access hint
pub val o_DIRECT = oDIRECT()
extern oDIRECT(): int32
  c inline "UV_FS_O_DIRECT"
// must be a directory
pub val o_DIRECTORY = oDIRECTORY()
extern oDIRECTORY(): int32
  c inline "UV_FS_O_DIRECTORY"
// don't follow links
pub val o_NOFOLLOW = oNOFOLLOW()
extern oNOFOLLOW(): int32
  c inline "UV_FS_O_NOFOLLOW"
// Do not update the file access time when the file is read
pub val o_NOATIME = oNOATIME()
extern oNOATIME(): int32
  c inline "UV_FS_O_NOATIME"
// Create an unnamed temporary file. pathname specifie a directory
pub val o_TMPFILE = oTMPFILE()
extern oTMPFILE(): int32
  c inline "UV_FS_O_TEMPORARY"

pub value struct uvFsReq { internal : intptr_t };
pub value struct uvFile { internal : intptr_t };
pub value struct uvDir { internal : intptr_t };

pub extern uvFsReq(): iouv-noexn uvFsReq
  c inline "kk_uv_fs_init(kk_context())"

// Cleanup request. Must be called after a request is finished to deallocate any memory libuv might have allocated.
pub extern req_cleanup(req: uvFsReq): iouv-noexn ()
  c "kk_uv_fs_req_cleanup" // ?? Is there a better way?

// Equivalent to close(2)
pub extern close(file: uvFile, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_close"

pub extern close-sync(file: uvFile): iouv-noexn error<()>
  c "kk_uv_fs_close_sync"

// Equivalent to open(2).
// WARNING: On Windows libuv uses CreateFileW and thus the file is always opened in binary mode. Because of this the O_BINARY and O_TEXT flags are not supported.
pub extern open(path: string, flags: int32, mode: int32, cb: (error<uvFile>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_open"

pub extern open-sync_(path: string, flags: int32, mode: int32): iouv-noexn error<uvFile>
  c "kk_uv_fs_open_sync"

pub fun open-sync(path: string, flags: int32, mode: int32=0.int32): iouv-noexn error<uvFile>
  if mode == 0.int32 && (flags == o_TMPFILE || flags == o_CREAT) then
    Error(Exception("Cannot open with no permission flags when creating " ++ path, ExnAssert))
  else open-sync_(path, flags, mode)

pub fun open(path: string, flags: int32, cb: (error<uvFile>) -> iouv-noexn (), mode: int32 = 0.int32): iouv-noexn ()
  if mode == 0.int32 && (flags == o_TMPFILE || flags == o_CREAT) then
    cb(Error(Exception("Cannot open with no permission flags when creating " ++ path, ExnAssert)))
  open(path, flags, mode, cb)

// Equivalent to preadv(2). If the offset argument is -1, then the current file offset is used and updated.
// WARNING: On Windows, under non-MSVC environments (e.g. when GCC or Clang is used to build libuv), files opened using UV_FS_O_FILEMAP may cause a fatal crash if the memory mapped read operation fails.
pub extern read(file: uvFile, length: int32, offset: int32, cb: (error<(bytes, int)>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_read"

pub fun read(file: uvFile, length: int32, cb: (error<(bytes, int)>) -> iouv-noexn ()): iouv-noexn ()
  read(file, 0.int32, length, cb)
// TODO: Version of read that takes in bytes to reuse?

pub extern read-sync_(file: uvFile, length: int32, offset: int32): iouv-noexn error<(bytes, int)>
  c "kk_uv_fs_read_sync"

pub fun read-sync(file: uvFile, length: int32, offset: int32 = 0.int32): iouv-noexn error<(bytes, int)>
  read-sync_(file, length, offset)

pub extern unlink(path: string, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_unlink"

pub extern unlink-sync(path: string): iouv-noexn error<()>
  c "kk_uv_fs_unlink_sync"

pub extern write(file: uvFile, data: bytes, offset: int64, cb: (error<int>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_write"

pub extern write-sync(file: uvFile, data: bytes, offset: int64): iouv-noexn error<int>
  c "kk_uv_fs_write_sync"

pub extern mkdir(path: string, mode: int32, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_mkdir"

pub extern mkdir-sync(path: string, mode: int32): iouv-noexn error<()>
  c "kk_uv_fs_mkdir_sync"

pub extern mkdtemp(path: string, cb: (error<string>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_mkdtemp"

pub extern mkdtemp-sync(path: string): iouv-noexn error<string>
  c "kk_uv_fs_mkdtemp_sync"

pub extern mkstemp(path: string, cb: (error<(uvFile, string)>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_mkstemp"

pub extern mkstemp-sync(path: string): iouv-noexn error<(uvFile, string)>
  c "kk_uv_fs_mkstemp_sync"

pub extern rmdir(path: string, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_rmdir"

pub extern rmdir-sync(path: string): iouv-noexn error<()>
  c "kk_uv_fs_rmdir_sync"

pub extern opendir(path: string, cb: (error<uvDir>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_opendir"

pub extern opendir-sync(path: string): iouv-noexn error<uvDir>
  c "kk_uv_fs_opendir_sync"

pub extern closedir(dir: uvDir, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_closedir"

pub extern closedir-sync(dir: uvDir): iouv-noexn error<()>
  c "kk_uv_fs_closedir_sync"

pub extern readdir(dir: uvDir, cb: (error<vector<dirent>>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_readdir"

pub extern readdir-sync(dir: uvDir): iouv-noexn error<vector<dirent>>
  c "kk_uv_fs_readdir_sync"

pub extern scandir(path: string, cb: (error<uvFsReq>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_scandir"

pub extern scandir-sync(path: string): iouv-noexn error<uvFsReq>
  c "kk_uv_fs_scandir_sync"

pub extern scandir-next(req: uvFsReq): iouv-noexn error<dirent>
  c "kk_uv_fs_scandir_next"

pub extern stat(path: string, cb: (error<fstat>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_stat"

pub extern stat-sync(path: string): iouv-noexn error<fstat>
  c "kk_uv_fs_stat_sync"

pub extern fstat(path: uvFile, cb: (error<fstat>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_fstat"

pub extern fstat-sync(path: uvFile): iouv-noexn error<fstat>
  c "kk_uv_fs_fstat_sync"

// Returns the same as stat(), except that if the path is a symbolic link, it returns information about the link itself
pub extern lstat(path: string, cb: (error<fstat>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_lstat"

pub extern lstat-sync(path: string): iouv-noexn error<fstat>
  c "kk_uv_fs_lstat_sync"

pub extern rename(path: string, new_path: string, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_rename"

pub extern rename-sync(path: string, new_path: string): iouv-noexn error<()>
  c "kk_uv_fs_rename_sync"

pub extern fsync(file: uvFile, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_fsync"

pub extern fsync-sync(file: uvFile): iouv-noexn error<()>
  c "kk_uv_fs_fsync_sync"

pub extern fdatasync(file: uvFile, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_fdatasync"

pub extern fdatasync-sync(file: uvFile): iouv-noexn error<()>
  c "kk_uv_fs_fdatasync_sync"

pub extern ftruncate(file: uvFile, offset: int64, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_ftruncate"

pub extern ftruncate-sync(file: uvFile, offset: int64): iouv-noexn error<()>
  c "kk_uv_fs_ftruncate_sync"

pub val uv_fs_COPYFILE_EXCL = uv_fs_COPYFILE_EXCL_()
extern uv_fs_COPYFILE_EXCL_(): int32
  c inline "UV_FS_COPYFILE_EXCL"

pub val uv_fs_COPYFILE_FICLONE = uv_fs_COPYFILE_FICLONE_()
extern uv_fs_COPYFILE_FICLONE_(): int32
  c inline "UV_FS_COPYFILE_FICLONE"

pub val uv_fs_COPYFILE_FICLONE_FORCE = uv_fs_COPYFILE_FICLONE_FORCE_()
extern uv_fs_COPYFILE_FICLONE_FORCE_(): int32
  c inline "UV_FS_COPYFILE_FICLONE_FORCE"

pub extern copyfile(path: string, new_path: string, flags: int32, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_copyfile"

pub extern copyfile-sync(path: string, new_path: string, flags: int32): iouv-noexn error<()>
  c "kk_uv_fs_copyfile_sync"

pub extern sendfile(out_file: uvFile, in_file: uvFile, in_offset: int64, length: ssize_t, cb: (error<int>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_sendfile"

pub extern sendfile-sync(out_file: uvFile, in_file: uvFile, in_offset: int64, length: ssize_t): iouv-noexn error<int>
  c "kk_uv_fs_sendfile_sync"

pub extern access(path: string, mode: int32, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_access"

pub extern access-sync(path: string, mode: int32): iouv-noexn error<()>
  c "kk_uv_fs_access_sync"

pub extern chmod(path: string, mode: int32, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_chmod"

pub extern chmod-sync(path: string, mode: int32): iouv-noexn error<()>
  c "kk_uv_fs_chmod_sync"

pub extern fchmod(file: uvFile, mode: int32, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_fchmod"

pub extern fchmod-sync(file: uvFile, mode: int32): iouv-noexn error<()>
  c "kk_uv_fs_fchmod_sync"

pub extern utime(path: string, atime: float64, mtime: float64, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_utime"

pub extern utime-sync(path: string, atime: float64, mtime: float64): iouv-noexn error<()>
  c "kk_uv_fs_utime_sync"

pub extern futime(file: uvFile, atime: float64, mtime: float64, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_futime"

pub extern futime-sync(file: uvFile, atime: float64, mtime: float64): iouv-noexn error<()>
  c "kk_uv_fs_futime_sync"

pub extern lutime(path: string, atime: float64, mtime: float64, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_lutime"

pub extern lutime-sync(path: string, atime: float64, mtime: float64): iouv-noexn error<()>
  c "kk_uv_fs_lutime_sync"

pub extern link(path: string, new_path: string, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_link"

pub extern link-sync(path: string, new_path: string): iouv-noexn error<()>
  c "kk_uv_fs_link_sync"

pub extern symlink(path: string, new_path: string, flags: int32, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_symlink"

pub extern symlink-sync(path: string, new_path: string, flags: int32): iouv-noexn error<()>
  c "kk_uv_fs_symlink_sync"

pub extern readlink(path: string, cb: (error<string>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_readlink"

pub extern readlink-sync(path: string): iouv-noexn error<string>
  c "kk_uv_fs_readlink_sync"

pub extern realpath(path: string, cb: (error<string>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_realpath"

pub extern realpath-sync(path: string): iouv-noexn error<string>
  c "kk_uv_fs_realpath_sync"

pub extern chown(path: string, uid: int32, gid: int32, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_chown"

pub extern chown-sync(path: string, uid: int32, gid: int32): iouv-noexn error<()>
  c "kk_uv_fs_chown_sync"

pub extern fchown(file: uvFile, uid: int32, gid: int32, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_fchown"

pub extern fchown-sync(file: uvFile, uid: int32, gid: int32): iouv-noexn error<()>
  c "kk_uv_fs_fchown_sync"

pub extern lchown(path: string, uid: int32, gid: int32, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_fs_lchown"

pub extern lchown-sync(path: string, uid: int32, gid: int32): iouv-noexn error<()>
  c "kk_uv_fs_lchown_sync"

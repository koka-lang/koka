/*---------------------------------------------------------------------------
  Copyright 2026, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
const _fs_module = ($std_core.host()=="node" ? await import("fs") : null);

function _fs() {
  if (_fs_module==null) throw new Error("directory operations are only supported on node");
  return _fs_module;
}

function _fs_error( action ) {
  try {
    return $std_core_types.Ok( action(_fs()) );
  }
  catch(exn) {
    return $std_core_exn._error_from_exception(exn);
  }
}

function _fs_stat_is( path, is ) {
  try {
    return is(_fs().statSync(path));
  }
  catch(exn) {
    return false;
  }
}

function _os_ensure_dir_error( path, mode ) {
  const m = $std_core_types._int_clamp32(mode);
  return _fs_error( (fs) => { fs.mkdirSync(path, { recursive: true, mode: (m < 0 ? 0o755 : m) }); return $std_core_types.Unit; } );
}

function _os_copy_file_error( from, to, preserve_mtime ) {
  return _fs_error( (fs) => {
    fs.copyFileSync(from, to);
    if (preserve_mtime) {
      const st = fs.statSync(from);
      fs.utimesSync(to, st.atime, st.mtime);
    }
    return $std_core_types.Unit;
  });
}

function _os_list_directory( dir ) {
  return _fs_error( (fs) => fs.readdirSync(dir) );
}

function _os_is_directory( path ) {
  return _fs_stat_is(path, (st) => st.isDirectory());
}

function _os_is_file( path ) {
  return _fs_stat_is(path, (st) => st.isFile());
}

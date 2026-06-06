/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

function kk_result_error(jsError) {
  return $std_core_exn._error_from_exception(jsError); 
}

function kk_result_ok(x) {
  return $std_core_types.Ok(x); 
}

function kk_result_ok_unit() {
  return $std_core_types.Ok($std_core_types.Unit); 
}

function kk_result_ok_nodispose() {
  return $std_core_types.Ok(() => $std_core_types.Unit); 
}

function kk_result(err,x) {
  if (err) { return kk_result_error(err); }
      else { return kk_result_ok(x); }
}

function kk_result_unit(err) {
  if (err) { return kk_result_error(err); }
      else { return kk_result_ok_unit(); }
}

function kk_enotsupp() { 
  return kk_result_error(new Error("operation is not supported")); 
}

const fs = ($std_core_console._host === "node" ? await import('fs') : null);


export function kk_fd_flags_from_filemode( fmode ) {
  if (!fs) return 0;
  if (fmode === 1)       return fs.constants.O_WRONLY | fs.constants.O_CREAT | fs.constants.O_TRUNC;
  else if (fmode === 2)  return fs.constants.O_WRONLY | fs.constants.O_CREAT | fs.constants.O_APPEND;
  else if (fmode === 3)  return fs.constants.O_RDWR   | fs.constants.O_CREAT | fs.constants.O_TRUNC;
  else return fs.constants.O_RDONLY;
}

function kk_fd_open_setup( evloop, path, flags, mode, cb ) {
  if (!fs) return kk_enotsupp();
  var callback = cb;
  fs.open(path,flags,mode,(err,fd) => { if (callback) callback(kk_result(err,fd)); });
  return kk_result_ok( () => callback = null );  // set callback to null in the dispose function as we cannot really cancel NodeJS requests
}

function kk_fd_close_setup( evloop, fd, cb ) {
  if (!fs) return kk_enotsupp();
  var callback = cb;
  fs.close(fd,(err) => { if (callback) callback(kk_result_unit(err)); });
  return kk_result_ok( () => callback = null );
}

function kk_fd_read_setup( evloop, fd, bytes, ofs, cb ) {
  if (!fs) return kk_enotsupp();
  var callback = cb
  let buf = Buffer.from( bytes.buffer )  // create a view without copying the UInt8Array
  fs.read(fd,{ buffer: buf, position: ofs }, (err,nread,_buf) => { if (callback) callback(kk_result(err,nread)); });
  return kk_result_ok( () => callback = null );
}

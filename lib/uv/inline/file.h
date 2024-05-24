
static kk_uv_file__fstat kk_uv_stat_from_uv_stat(uv_stat_t* uvstat, kk_context_t* _ctx) {
  return kk_uv_file__new_Fstat(
    kk_reuse_null,
    0, // cpath
    uvstat->st_dev,
    uvstat->st_mode,
    uvstat->st_nlink,
    uvstat->st_uid,
    uvstat->st_gid,
    uvstat->st_rdev,
    uvstat->st_ino,
    uvstat->st_size,
    uvstat->st_blksize,
    uvstat->st_blocks,
    uvstat->st_flags,
    kk_uv_file__new_Timespec(uvstat->st_atim.tv_sec, uvstat->st_atim.tv_nsec, _ctx),
    kk_uv_file__new_Timespec(uvstat->st_mtim.tv_sec, uvstat->st_mtim.tv_nsec, _ctx),
    kk_uv_file__new_Timespec(uvstat->st_ctim.tv_sec, uvstat->st_ctim.tv_nsec, _ctx),
    kk_uv_file__new_Timespec(uvstat->st_birthtim.tv_sec, uvstat->st_birthtim.tv_nsec, _ctx),
    _ctx
  );
}
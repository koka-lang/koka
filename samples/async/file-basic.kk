import std/os/file-uv

val readCount = 1024
fun readFile(fd: uvFile, offset: int = 0, str: ref<global, string>)
  read(fd, readCount.int32, offset.int32) fn(bytes1)
    match bytes1
      Ok((bytes, len)) ->
        str := !str ++ bytes.to-string
        if len == readCount then
          readFile(fd, offset+readCount, str)
        else 
          (!str).println
          println("\n\nDone!")
      _ -> ()

fun main()
  // Low Level Async API example (see async-read-file for example using async library)
  open("stack.yaml", o_RDONLY) fn(fd1)
    match fd1
      Ok(fd) ->
        println("Got fd " ++ fd.internal.int.show ++ "\n")
        readFile(fd, 0, ref(""))
      _ -> ()
  // Sync API Example
  val stackerr = open-sync("stack.yaml", o_RDONLY, 0.int32)
  match stackerr
    Ok(stack) ->
      val reserr = stack.read-sync(2048.int32)
      match reserr
        Ok((bytes, _)) ->
          println("Got fd sync " ++ stack.internal.int.show ++ "\n")
          println(bytes.to-string)
          println("\n\nDone!")
        _ -> ()
    _ -> ()
  val dir = mkdir-sync("scratch/test", s_IRWXU)
  val dir2 = mkdtemp-sync("scratch/test/tmpXXXXXX")
  match dir2 
    Ok(d) -> 
      println("Created dir " ++ d ++ "\n")
      val tmpf = mkstemp-sync(d ++ "/tmpXXXXXX")
      val tmpfile = open-sync("scratch/test/tmpfile.txt", o_CREAT.or(o_RDWR), s_IRWXU)
      match tmpfile
        Ok(f) ->
          match f.write-sync("Hello World!".from-string, 0.int64)
            Ok(n) -> ("Wrote " ++ n.show ++ " bytes").println
            Error(e) -> throw-exn(e)
        Error(e) -> throw-exn(e)
      match tmpf
        Ok((f, name)) ->
          println("Created tmp file " ++ name ++ "\n")
          match f.write-sync("Hello World!".from-string, 0.int64)
            Ok(n) -> 
              ("Wrote " ++ n.show ++ " bytes").println
              unlink-sync(name)
              ()
            Error(e) -> throw-exn(e)
        Error(e) -> throw-exn(e)
      rmdir-sync(d)
      ()
    _ -> ()
  val x = stat-sync("stack.yaml")
  match x
    Ok(s) -> println(s.show)
    Error(e) -> throw-exn(e)
  rename-sync("scratch/test.kk", "scratch/test2.kk")
  copyfile-sync("scratch/test2.kk", "scratch/test.kk", uv_fs_COPYFILE_EXCL)
  unlink-sync("scratch/test2.kk")
  ()
  

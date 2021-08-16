module base

public fun raise(s) { throw-exn(Exception(s, ExnError)) }

// Don't use the exn effect for better performance
public fun wont-happen() { 
  unsafe-noexn(fn() { raise("Won't happen") })
}
module ev3

public fun f( g : () -> bool ) : io int {
  //print(1)
  if (g()) return 1
  3
}

import std/num/int32

val player-size = 16
val pink = 10

// take a type that has a position, and a change in position, and update the position
fun location/update<locatable>(a: locatable, dx: int, dy: int, 
                              ?x: locatable -> int, ?y: locatable -> int, 
                              ?@copy: (locatable, x: int, y: int) -> locatable): locatable
  a.@copy(x = a.x + dx, y = a.y + dy)

// take a type that has a velocity, and a change in velocity, and update the velocity
fun velocity/update<movable>(a: movable, ddx: int, ddy: int, 
                    ?dx: movable -> int, ?dy: movable -> int, 
                    ?@copy: (movable, dx: int, dy: int) -> movable): movable
  a.@copy(dx = a.dx + ddx, dy = a.dy + ddy)

// take a type that has a position and velocity, and apply the velocity to the position
fun apply-velocity<movable>(a: movable, 
                          ?x: movable -> int, ?y: movable -> int, 
                          ?dx: movable -> int, ?dy: movable -> int, 
  // ERROR: This copy function should not match the @copy parameter expected by `location/update`!
                          ?@copy: (movable, dx: int, dy: int) -> movable): movable
  a.location/update(a.dx, a.dy)

// Let's make some specific types for player and enemy
value struct player
  x: int
  y: int
  dx: int
  dy: int

value struct enemy
  x: int
  y: int
  dx: int
  dy: int

fun draw-rectangle(x: int32, y: int32, width: int32, height: int32, color: int)
  // Implementation of drawing a rectangle on the screen
  // This is a placeholder function. Replace with actual drawing logic.
  println("Drawing rectangle at (" ++ x.show ++ ", " ++ y.show ++  ") with size (" ++ width.show ++ ", " ++ height.show ++ ") and color " ++ color.show)

fun up-pressed(): bool
  // Placeholder for checking if the up key is pressed
  // Replace with actual input handling logic.
  return True

fun positioned/draw<locatable>(^a: locatable, ?x: locatable -> int, ?y: locatable -> int): <io> ()
  draw-rectangle(
    int32(a.x),
    int32(a.y),
    player-size.int32, player-size.int32, pink)

fun main()
  var p := Player(0, 0, 0, 0)
  var e := Enemy(100, 100, 0, 0)
  // ... somewhere in the update loop
  if up-pressed() then
    p := p.velocity/update(0, 1).apply-velocity()
  e := e.velocity/update(0, -1).apply-velocity()
  p.draw()
  e.draw()
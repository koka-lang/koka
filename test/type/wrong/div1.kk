val wrong : int = recursive()

fun recursive()  {
  recursive()
  return wrong
}
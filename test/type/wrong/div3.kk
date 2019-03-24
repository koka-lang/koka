val wrong : int = recursive(1)

fun recursive(x)   {
  recursive(wrong)
}
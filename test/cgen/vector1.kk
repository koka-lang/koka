pub fun vector/append(first : vector<a>, second : vector<a>) : _ vector<a>
  vector-init(first.length + second.length, fn(i) index-two(first, second, i))

fun vector/index-two(first : vector<a>, second : vector<a>, i: int) : _ a
  if i < first.length then first[i] else second[i]

pub fun main()
  val x = [1,2,3].vector
  val y = x.append(x)
  ()
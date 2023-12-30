// Had an issue where hidden names of operators were creating invalid identifiers for parsing the kki files.
fun (+)(l: list<int>, x: int): list<int>
  match l
    Cons(n, rs) -> Cons(n + x, rs + x)
    Nil -> Nil
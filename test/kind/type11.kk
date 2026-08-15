struct multi-box<element>(value : element)
struct multi-pair<left,right>(fst : left, snd : right)

alias multi-id<element> = element
alias multi-apply<constructor :: V -> V, element> = constructor<element>
alias multi-rank2 = forall<element> element -> element
alias int-shadow<int> = int

effect multi-cell<element>
  fun multi-get() : element

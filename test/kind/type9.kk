// test name clash on type synonyms
alias id = forall<a> a -> a
alias tt  = forall<a> a -> id
alias choose = id -> id

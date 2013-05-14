type int
alias equal<a,b> = forall<f> f<a> -> f<b>
alias tt = equal<int,int>
alias uu<a>  = equal<int,a>
alias vv<a>  = equal<a,a>

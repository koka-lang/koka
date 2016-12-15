type int
type tt1<a,b> { con TT1(x: tt2<a,int>) }
type tt2<a,b> { con TT2(y: tt1<int,b>) }

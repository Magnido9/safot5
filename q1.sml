datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);
fun filterq pred Nil = Nil
| filterq pred (Cons(x,xf)) =
if pred x then Cons(x,fn()=>filterq pred (xf()))
else filterq pred (xf());
fun from k = Cons(k,fn()=>from(k+1));
fun mapq f Nil = Nil
| mapq f (Cons(x,xf)) =
Cons( f(x), fn()=>mapq f (xf()) );
fun add a= mapq (fn n => n+a);
fun sieve (Cons(p,nf)) =
Cons(p, fn () => sieve(add p (nf())));
fun  arithmeticSeq a1 jump = mapq (fn n => n+a1-jump-jump) (filterq (fn n => n mod jump=0) (from jump) );
val a = arithmeticSeq 3 4;
fun head(Cons(x,_)) = x;
fun tail(Cons(_,xf)) = xf();
head(tail(tail(tail(tail(a)))));
fun getSub a b c = arithmeticSeq (a+b*c) b;
fun showSeqRec seq d :int list= if(d=0) then (nil) else ((head seq)::(showSeqRec (tail seq) (d-1) ));
fun showSeq seq d= tl(showSeqRec seq (d+1));
fun getSubSeq a b c d=showSeq (getSub a b (c-1)) (d-1);
getSubSeq 3 3 2 5;
fun getKDivElems a b n k=getSubSeq (a-b) (b*k) 2 (n+1);
getKDivElems 3 3 4 2;
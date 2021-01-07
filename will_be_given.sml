   datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;
   datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);


(*these three need to be deleted before הגשה !!!!!!!!*)
 fun pow(a,b)=if(b=0) then 1 else a*pow(a,b-1);
fun atoi s = if size(s)=0 then 0 else ((Char.ord(String.sub(s,0))-Char.ord(#"0"))*pow(10,(size(s)-1)))+atoi(String.extract(s,1,NONE)); 
fun isNumber xs = List.all (Char.isDigit) (explode xs);

local

fun getMatchBraAux ([],counter,res)=res
     |getMatchBraAux (("("::xs), counter,res)= if(counter=0) then getMatchBraAux(xs,(counter+1),res) else getMatchBraAux(xs,(counter+1),res@["("])
     |getMatchBraAux((")"::xs),counter,res)=if(counter=1) then res@[")"] else getMatchBraAux(xs,(counter-1),res@[")"])
     |getMatchBraAux((x::xs),counter,res)=getMatchBraAux(xs,counter,res@[x]);

fun getMatchBra lis=getMatchBraAux(lis,0,[]);

fun getIndexAux ([],i_counter, b_counter)=i_counter
    |getIndexAux(("("::xs),i_counter,b_counter)=getIndexAux(xs,(i_counter+1),(b_counter+1))
    |getIndexAux((")"::xs),i_counter,b_counter)=if(b_counter=1)then i_counter else getIndexAux(xs,(i_counter+1),(b_counter-1))
    |getIndexAux((x::xs),i_counter,b_counter)=getIndexAux(xs,(i_counter+1),b_counter);

    (*this function get a list that starts with "(" and returns the index of the mathing ")" to this from the start*)
fun getIndex(lis)=getIndexAux(lis,0,0);

   (*this get the list without the first bracket*)
   fun parseAux []=ATOM(NIL)
      |parseAux[")"]=ATOM(NIL)
     |parseAux ("("::xs)=CONS(parseAux(getMatchBra ("("::xs)) ,parseAux(List.drop(xs,getIndex("("::xs))))
     |parseAux (x::xs)=if (isNumber(x)) then CONS(ATOM(NUMBER(atoi(x))),parseAux(xs)) else CONS(ATOM(SYMBOL(x)),parseAux(xs));


 in
  fun parse []=ATOM(NIL)
  |parse ["(",")"]=ATOM(NIL)
  |parse [x]=if (isNumber(x)) then ATOM(NUMBER(atoi(x))) else ATOM(SYMBOL(x))
  |parse ("("::xs) =parseAux(xs)


end;

   

fun tokenize program = String.tokens (fn (a:char) => if ord(a) = ord(#" ") then true else false) (String.translate (fn (a:char) => if ord(a) = ord(#"(") then " ( " else if ord(a) = ord(#")") then " ) " else str(a)) program);


(*Question 3*)
(*a*)
exception Undefined;
exception Empty;
fun initEnv ()= fn f:string=>(raise Undefined);
fun emptyNestedEnv () = [initEnv ()];

(*b*)
fun define name old_env value= fn x:string => if(x=name) then value else old_env(x);

(*c*)
fun pushEnv env env_list=(env::env_list);
fun popEnv env_list =if List.length(env_list)=1 then raise Empty else tl env_list;
fun topEnv env_list= if List.length(env_list)=1 then raise Empty else hd env_list;

(*d*)
fun defineNested name env_list value=pushEnv (define name (topEnv(env_list)) value) (popEnv(env_list));

(*e*)
fun find (name:string) stack =((topEnv stack) name) handle Undefined=>(find name (popEnv stack) )  handle Empty=>raise Undefined;

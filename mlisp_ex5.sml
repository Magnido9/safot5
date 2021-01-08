    exception MlispError;
    datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;
   datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);


fun eval exp env= 
let
fun semiEval exp env =
case exp of
   CONS(x,ATOM(NIL)) => semiEval x env
 | _ => #1(eval exp env);(*just the S_Exp*)
 

fun pushNew (name)  (env_list) value= pushEnv (define name (initEnv()) value) env_list;
fun plus (CONS(ATOM(NUMBER(x)),CONS(ATOM(NUMBER(y)),ATOM(NIL))))=ATOM(NUMBER(x+y))
    |plus _=raise MlispError;

fun plus_string (CONS(ATOM(NUMBER(x)),CONS(ATOM(NUMBER(y)),ATOM(NIL))))=Int.toString(x+y)
    |plus_string _=raise MlispError;

fun min (CONS(ATOM(NUMBER(x)),CONS(ATOM(NUMBER(y)),ATOM(NIL))))=ATOM(NUMBER(x-y))
    |min _=raise MlispError;

fun mult (CONS(ATOM(NUMBER(x)),CONS(ATOM(NUMBER(y)),ATOM(NIL))))=ATOM(NUMBER(x*y))
    |mult _=raise MlispError;

fun divied (CONS(ATOM(NUMBER(x)),CONS(ATOM(NUMBER(y)),ATOM(NIL))))=ATOM(NUMBER(x div y))
    |divied _=raise MlispError;

  fun cons (CONS(hd,tl)) env = CONS(semiEval hd env ,semiEval tl env)
    |cons _  _= raise MlispError;
  
   fun car (CONS(ATOM(x),xs))=ATOM(x)
     |car (CONS(CONS(x,xs),ys))=(car (CONS(x,xs)))
      |car _ =raise MlispError;(*no list*)

    fun cdr (CONS(x,xs))=xs
        |cdr _=raise MlispError;(*no list*)

 
in
  case exp of

        (ATOM(NIL)) =>(ATOM(NIL),env)
        | (ATOM(NUMBER(x))) => (ATOM(NUMBER(x)),env)
        | (ATOM(SYMBOL(x)))  => (find x env ,env) 
        | (CONS(ATOM(SYMBOL("+")), sExp)) =>(plus sExp,env)
        | (CONS(ATOM(SYMBOL("-")),sExp)) =>(min sExp,env)
        | (CONS(ATOM(SYMBOL("*")),sExp)) =>(mult sExp,env)
        | (CONS(ATOM(SYMBOL("div")),sExp)) =>(divied sExp,env)
        | (CONS(ATOM(SYMBOL("cons")),sExp)) =>(cons sExp env ,env)
        |(CONS(ATOM(SYMBOL("car")),sExp)) =>(car (semiEval sExp env),env)
        |(CONS(ATOM(SYMBOL("cdr")),sExp)) =>(cdr (semiEval sExp env),env)
        handle Undefined=>raise MlispError 
        (*handel Empty=> raise MlispError*)


end;

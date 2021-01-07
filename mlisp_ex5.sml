    exception MlispError;
    datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;
   datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);

(*need to hide this:*)
fun pushNew (name)  (env_list) value= pushEnv (define name (initEnv()) value) env_list;


fun plus_sExp (CONS(ATOM(NUMBER(x)),CONS(ATOM(NUMBER(y)),ATOM(NIL))))=ATOM(NUMBER(x+y))
    |plus_sExp _=raise MlispError;

fun plus_string (CONS(ATOM(NUMBER(x)),CONS(ATOM(NUMBER(y)),ATOM(NIL))))=Int.toString(x+y)
    |plus_string _=raise MlispError;

  fun cons (hd,tl)= (CONS(hd,tl));
  
   fun car (CONS(x,xs))=x
      |car _ =raise MlispError;(*no list*)

    fun cdr (CONS(x,xs))=xs
        |cdr _=raise MlispError;(*no list*)

    fun eval (ATOM(NIL)) (env:(string -> SExp) list)=(ATOM(NIL),env)
        |eval (ATOM(NUMBER(x))) env = (ATOM(NUMBER(x)),env)
        |eval (ATOM(SYMBOL(x))) env = (find x env ,env) 
        |eval (CONS(ATOM(SYMBOL("+")), sExp)) env =((plus_sExp sExp),(pushNew (plus_string sExp) env (plus_sExp sExp)))  
        handle Undefined=>raise MlispError;

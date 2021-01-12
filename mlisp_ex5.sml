(*tomer katz- 212234140 tomerkatz@campus.technion.ac.il, ido magner-212324313 idomagner@campus.technion.ac.il*)

(*****Excercise 5 - MLISP Interper*****)
exception MlispError;

fun eval exp env=
let
fun semiEval exp env =
case exp of
CONS(x,ATOM(NIL)) => semiEval x env
| _ => #1(eval exp env);(*just the S_Exp*)


fun pushNew (name)  (env_list) value= pushEnv (define name (initEnv()) value) env_list ;
fun sExp_to_int (ATOM(NUMBER(x)))=x
    |sExp_to_int _=raise MlispError;

fun plus (CONS(x_exp,y_exp)) env =ATOM(NUMBER(sExp_to_int(semiEval x_exp env)+sExp_to_int(semiEval y_exp env)))
    |plus _ _=raise MlispError;


fun min (CONS(x_exp,y_exp)) env =ATOM(NUMBER(sExp_to_int(semiEval x_exp env)-sExp_to_int(semiEval y_exp env)))
    |min _ _=raise MlispError;

fun mult (CONS(x_exp,y_exp)) env =ATOM(NUMBER(sExp_to_int(semiEval x_exp env)*sExp_to_int(semiEval y_exp env)))
    |mult _ _=raise MlispError;

fun divide (CONS(x_exp,y_exp)) env = ATOM(NUMBER( (sExp_to_int(semiEval x_exp env)) div (sExp_to_int(semiEval y_exp env))))
    |divide _ _=raise MlispError;


fun cons (CONS(hd,tl)) env = CONS(semiEval hd env ,semiEval tl env)
 |cons _  _= raise MlispError;

fun car (CONS(ATOM(x),xs))=ATOM(x)
  |car (CONS(CONS(x,xs),ys))=(car (CONS(x,xs)))
   |car _ =raise MlispError;(*no list*)

 fun cdr (CONS(x,xs))=xs
     |cdr _=raise MlispError;(*no list*)

 fun define s_exp  env=
     case s_exp of
     CONS(ATOM (SYMBOL (name)),CONS (ATOM (NUMBER (value)),ATOM NIL)) => (ATOM(NIL), pushNew name env (ATOM(NUMBER(value))))
     | CONS(ATOM (SYMBOL (name)),func)=> (ATOM(NIL), pushNew name env func)
     | _ => (ATOM(NIL),env);
 fun findfunc env func=find func env handle Undefined=>raise MlispError;

(*Function to replace all appearences of a parmeter with his value*)
fun replace_token (CONS(ATOM (x),ATOM(NIL)),a,b)=  if( ATOM(x) = ATOM(a)) then CONS(ATOM(b),ATOM(NIL)) else  CONS(ATOM(x),ATOM(NIL))
 | replace_token   (CONS(ATOM(x),xs),a,b)= if( ATOM(x) = ATOM(a)) then CONS(ATOM(b),replace_token(xs,a,b)) else  CONS(ATOM(x),replace_token(xs,a,b))
 | replace_token (CONS(xs,ATOM(y)),a,b) = if(ATOM(y)=ATOM(a)) then CONS(replace_token(xs,a,b),ATOM(b)) else  CONS(replace_token(xs,a,b),ATOM(y))
 | replace_token _ = raise MlispError;

(*Function to apply when a command that is not system defined was called*)
 fun user_func (CONS(ATOM(x),rest)) (CONS(CONS(ATOM(y),xs),func))=(user_func rest (CONS(xs,(replace_token(func,y,x)))))
 |  user_func (CONS(ATOM(x),rest)) (CONS(ATOM(y),func))=replace_token(func,y,x)
 |  user_func (ATOM(x)) (CONS(ATOM(y),func))=replace_token(func,y,x)
 | user_func _ _= raise MlispError;



in
case exp of

     (ATOM(NIL)) =>(ATOM(NIL),env)
     |(ATOM(SYMBOL("nil")))=>(ATOM(NIL),env)
     | (ATOM(NUMBER(x))) => (ATOM(NUMBER(x)),env)
     | (ATOM(SYMBOL(x)))  => (find x env ,env)
     | (CONS(ATOM(SYMBOL("+")), sExp)) =>(plus sExp env,env)
     | (CONS(ATOM(SYMBOL("-")),sExp)) =>(min sExp env,env)
     | (CONS(ATOM(SYMBOL("*")),sExp)) =>(mult sExp env,env)
     | (CONS(ATOM(SYMBOL("div")),sExp)) =>(divide sExp env,env)
     | (CONS(ATOM(SYMBOL("cons")),sExp)) =>(cons sExp env ,env)
     | (CONS(ATOM(SYMBOL("car")),sExp)) =>(car (semiEval sExp env),env)
     | (CONS(ATOM(SYMBOL("cdr")),sExp)) =>(cdr (semiEval sExp env),env)
     | (CONS(ATOM(SYMBOL("define")),sExp)) =>(define sExp env)
     | (CONS(ATOM(SYMBOL(func)),sExp)) =>((semiEval (user_func sExp (findfunc env func)) env),env)
     | _ => raise MlispError



end;

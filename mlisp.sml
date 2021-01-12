

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

exception MlispError;



fun eval exp env=
let
fun semiEval exp env =
case exp of
CONS(x,ATOM(NIL)) => semiEval x env
| _ => #1(eval exp env);(*just the S_Exp*)


fun pushNew (name)  (env_list) value= pushEnv (define name (initEnv()) value) env_list;
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
 fun findfunc env func=find func env;

 fun replace_token (CONS(ATOM (x),ATOM(NIL)),a,b)=  if( ATOM(x) = ATOM(a)) then CONS(ATOM(b),ATOM(NIL)) else  CONS(ATOM(x),ATOM(NIL))
 | replace_token   (CONS(ATOM(x),xs),a,b)= if( ATOM(x) = ATOM(a)) then CONS(ATOM(b),replace_token(xs,a,b)) else  CONS(ATOM(x),replace_token(xs,a,b))
 | replace_token (CONS(xs,ATOM(y)),a,b) = if(ATOM(y)=ATOM(a)) then CONS(replace_token(xs,a,b),ATOM(b)) else  CONS(replace_token(xs,a,b),ATOM(y))
 | replace_token _ = raise MlispError;

 (*no list*)
 fun user_func (CONS(ATOM(x),rest)) (CONS(CONS(ATOM(y),xs),func))=(user_func rest (CONS(xs,(replace_token(func,y,x)))))
 |  user_func (CONS(ATOM(x),rest)) (CONS(ATOM(y),func))=replace_token(func,y,x)
 |  user_func (ATOM(x)) (CONS(ATOM(y),func))=replace_token(func,y,x)
 | user_func _ _= raise MlispError;



in
case exp of

     (ATOM(NIL)) =>(ATOM(NIL),env)
     |(ATOM(SYMBOL("nil")))=>(ATOM(NIL),env)
     | (ATOM(NUMBER(x))) => (ATOM(NUMBER(x)),env)
     | (ATOM(SYMBOL(x)))  => (find x env ,env) (*maybe should be eval after finding the symbol so if we are asked for function it will work*)
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

     (*handel Empty=> raise MlispError*)


end;
val (res,env) = eval (parse (tokenize "(cons 1 2)")) (emptyNestedEnv ());
(parse (tokenize "(* 3 r))"));
val (res,env) = (eval (parse (tokenize "(define area (r) (* 3  r))")) env);
val (res,env) = (eval (parse (tokenize "(area 5)")) env);

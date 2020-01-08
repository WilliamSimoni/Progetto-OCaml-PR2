exception EmptyEnv;;
exception InvalidIdentifier;;
exception DifferentListLength;;
exception NotNumber;;
exception NotInteger;;
exception NotFloat;;
exception NotBoolean;;
exception MatchFailure;;
exception NotFunction;;
exception NotDictionary;;
exception NotType;;
exception NotArray;;
exception GenericTypeException;;

type ide = string;;

(*Sintassi astratta*)

type exp = 
	|Den of int	
	|Int of int
	|Float of float
	|String of string
	|Boolean of bool
	|Type of string
	|Exception of string 
	|EmptyArray of exp * exp							(*Tipo Array, lunghezza Array*)
	|EmptyArrayWith of exp * exp * exp					(*Tipo Array, lunghezza Array, valore di inizializzazione*)
	|Array of exp * exp list							(*Tipo Array, lista di valori con cui inizializzarlo*)
	|Sum of exp * exp
	|Dif of exp * exp
	|Mul of exp * exp
	|Div of exp * exp
	|Greater of exp * exp	
	|Lower of exp * exp		
	|Eq of exp * exp
	|Ceil of exp
	|Floor of exp
	|Mod of exp * exp
	|Concat of exp * exp
	|Not of exp
	|Or of exp * exp
	|And of exp * exp
	|Imp of exp * exp
	|IfThenElse of exp * exp * exp						
	|Switch of exp list * (exp list * exp) list   		(*Lista di espressioni e1 e lista di lista di espressioni e2 e di espressioni e3 -> switch e1 case (Se primo elemento di e2 = e1) -> eseguo primo elemento di e3 ecc*)
	|Let of exp * exp									(*valore da associare a variabile, body del let*)
	|LetRec of exp * exp 								(*body della funzione, body del let*)
	|Fun of exp 										(*body della funzione*)
	|FunAc of exp * exp list							(*Funzione, parametri*)
	|Dict of (ide * exp) list							(*lista di coppie (identificatore,valore)*)
	|SelectDictName of exp * ide						(*Dizionario, identificatore elemento*)
	|SelectDictIndex of exp * exp						(*Dizionario, Indice*)
	|InsertDict of exp * ide * exp 						(*Dizionario, identificatore nuovo elemento, espressione il cui valore viene a nuovo elemento*)
	|RemoveDict of exp * ide							(*Dizionario, identificatore elemento*)
	|RemoveDictIndex of exp * exp						(*Dizionario, intero i che indica l'i-esimo elemento del dizionario*)
	|Clear of exp										(*Dizionario*)
	|ApplyOver of exp * exp								(*Dizionario,Funzione*)
	|GetIndex of exp * exp								(*Array, Indice*)
	|SetIndex of exp * exp * exp						(*Array, indice, valore*)
	|Length of exp;;									(*Array/Dizionario*)
	
(*Ambiente*)

type env = evT array
and evT =
	|EInt of int
	|EFloat of float
	|EBool of bool
	|EString of string
	|Unbound
	|Funval of exp * env
	|FunRecVal of exp * env
	|Dictionary of (ide * evT) list
	|EType of string 
	|EArray of evT * evT list ;;
	
(*Operazioni sull'ambiente*)

let senv = [||];;

let bind (e,v) = Array.append e [|v|] ;;(*e = ambiente, i = identificatore, valore*)

let bindList (e,v) = let a = Array.of_list v in Array.append e a;;

let lookIde (e,i) = if i >= Array.length e then failwith(string_of_int i ^ " Non valido") else Array.get e i;;

(*Type checker*)

let typeCheck(t,v) =  match t with
	|"Int" -> (match v with
			|EInt u -> EBool(true)
			|_ -> EBool(false))
	|"Boolean" -> (match v with
			|EBool b -> EBool(true)
			|_ -> EBool(false))
	|"Float" -> (match v with
			|EFloat f -> EBool(true)
			|_ -> EBool(false))
	|"String" -> (match v with
			|EString s -> EBool(true)
			|_ -> EBool(false))
	|"Type" -> (match v with
			|EType t -> EBool(true)
			|_ -> EBool(false))
	|"Array" -> (match v with
			|EArray (_,_) -> EBool(true)
			|_ -> EBool(false))
	|"Dictionary" -> (match v with
			|Dictionary d -> EBool(true)
			|_ -> EBool(false))		
	|"FunVal" -> (match v with
			|Funval(_,_) -> EBool(true)
			|_ -> EBool(false))	
	|"RecFun" -> (match v with
			|FunRecVal(_,_) -> EBool(true)
			|_ -> EBool(false))	
	|_ -> raise MatchFailure;;
	
(*Eval*)	

let rec eval ((e:exp),(r:env)) : evT = match e with
	|Den(i) -> lookIde(r,i)
	|Int(n) -> EInt(n)
	|Float(n) -> EFloat(n)
	|Boolean(b) -> EBool(b)
	|String(s) -> EString(s)
	|Type(s) -> EType(s)
	|Exception(s) -> failwith(s)
	|EmptyArray(e1,e2) -> let v = eval(e1,r) in (match v with
						|EType t -> (match eval(e2,r) with
							|EInt len -> (match t with
								|"Int" -> let q = init (len,(fun x -> EInt(0))) in EArray(EType t, q)
								|"Float" -> let q = init (len,(fun x -> EFloat(0.0))) in EArray(EType t, q)
								|"Boolean" -> let q = init (len,(fun x -> EBool(true))) in EArray(EType t, q)
								|"String" -> let q = init (len,(fun x -> EString(" "))) in EArray(EType t, q)
								|_-> raise GenericTypeException)
							|_-> raise NotInteger)
						|_ -> raise NotType)
	|EmptyArrayWith(e1,e2,e3) -> let v = eval(e1,r) in (match v with
						|EType t -> (match eval(e2,r) with
							|EInt len -> let vi = eval(e3,r) in 
											if typeCheck(t,vi) = EBool(true) then 
												let q = init (len,(fun x -> vi)) in EArray(EType t, q)
											else
												raise GenericTypeException
							|_-> raise NotInteger)
						|_ -> raise NotType)
	|Array(e1,e2) -> let v = eval(e1,r) in (match v with
						|EType t -> let q = evalQA(e2,t,r) in EArray(EType t ,q)
						|_ -> raise NotType)
	|Sum(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval (e2,r) in (match (v1,v2) with
						|(EInt v1, EInt v2) -> EInt(v1 + v2)
						|(EFloat v1, EFloat v2) -> EFloat(v1 +. v2)
						|(EInt v1, EFloat v2) -> EFloat(float_of_int(v1) +. v2)
						|(EFloat v1, EInt v2) -> EFloat(v1 +. float_of_int(v2))
						|_ -> raise NotNumber)				
	|Dif(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EInt v1, EInt v2) -> EInt(v1 - v2)
						|(EFloat v1, EFloat v2) -> EFloat(v1 -. v2)
						|(EInt v1, EFloat v2) -> EFloat(float_of_int(v1) -. v2)
						|(EFloat v1, EInt v2) -> EFloat(v1 -. float_of_int(v2))
						|_ -> raise NotNumber)
	|Mul(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EInt v1, EInt v2) -> EInt(v1 * v2)
						|(EFloat v1, EFloat v2) -> EFloat(v1 *. v2)
						|(EInt v1, EFloat v2) -> EFloat(float_of_int(v1) *. v2)
						|(EFloat v1, EInt v2) -> EFloat(v1 *. float_of_int(v2))
						|_ -> raise NotNumber)
	|Div(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EInt v1, EInt v2) -> EInt(v1 / v2)
						|(EFloat v1, EFloat v2) -> EFloat(v1 /. v2)
						|(EInt v1, EFloat v2) -> EFloat(float_of_int(v1) /. v2)
						|(EFloat v1, EInt v2) -> EFloat(v1 /. float_of_int(v2))
						|_ -> raise NotNumber)
	|Greater(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EInt v1, EInt v2) -> EBool(v1 > v2)
						|(EFloat v1, EFloat v2) -> EBool(v1 > v2)
						|(EInt v1, EFloat v2) -> EBool(float_of_int(v1) > v2)
						|(EFloat v1, EInt v2) -> EBool(v1 > float_of_int(v2))
						|_ -> EBool(false))
	|Lower(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EInt v1, EInt v2) -> EBool(v1 < v2)
						|(EFloat v1, EFloat v2) -> EBool(v1 < v2)
						|(EInt v1, EFloat v2) -> EBool(float_of_int(v1) < v2)
						|(EFloat v1, EInt v2) -> EBool(v1 < float_of_int(v2))
						|_ -> EBool(false))
	|Eq(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EInt v1, EInt v2) -> EBool(v1 = v2)
						|(EFloat v1, EFloat v2) -> EBool(v1 = v2)
						|(EInt v1, EFloat v2) -> EBool(float_of_int(v1) = v2)
						|(EFloat v1, EInt v2) -> EBool(v1 = float_of_int(v2))
						|(EBool v1, EBool v2) -> EBool(v1 = v2)
						|(EString v1, EString v2) -> EBool(v1 = v2)
						|(Dictionary(q1), Dictionary(q2)) -> eqDict(q1,q2)
						|(EArray(t1,q1),EArray(t2,q2)) -> eqArray(t1,t2,q1,q2)
						|(v1, EType t1) -> typeCheck(t1,v1)
						|_ -> EBool(false))
	|Ceil(e1) -> let v = eval(e1,r) in (match v with
						|EFloat v -> EFloat(ceil(v))
						|EInt v ->EFloat(float_of_int(v))
						|_ -> raise NotNumber)
	|Floor(e1) -> let v = eval(e1,r) in (match v with
						|EFloat v -> EFloat(floor(v))
						|EInt v ->EFloat(float_of_int(v))
						|_ -> raise NotNumber)
	|Mod(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EInt v1, EInt v2) -> let v = v1 mod v2 in EInt(v)
						|(EFloat v1, EFloat v2) -> let v = mod_float v1 v2 in EFloat(v)
						|(EInt v1, EFloat v2) -> let v1f = float_of_int(v1) in let v = mod_float v1f v2 in EFloat(v)
						|(EFloat v1, EInt v2) -> let v2f = float_of_int(v2) in let v = mod_float v1 v2f in EFloat(v)
						|_ -> raise NotNumber)
	|Concat(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EString v1, EString v2) -> EString(v1^v2)
						|(EString v1, EInt v2) -> let v2s = string_of_int v2 in EString(v1^v2s)
						|(EInt v1, EString v2) -> let v1s = string_of_int v1 in EString(v1s^v2)
						|(EString v1, EFloat v2) -> let v2s = string_of_float v2 in EString(v1^v2s)
						|(EFloat v1, EString v2) -> let v1s = string_of_float v1 in EString(v1s^v2)
						|(EString v1, EBool v2) -> let v2s = string_of_bool v2 in EString(v1^v2s) 
						|(EBool v1, EString v2) -> let v1s = string_of_bool v1 in EString(v1s^v2) 
						|(EInt v1, EInt v2) -> let v1s = string_of_int v1 in let v2s = string_of_int v2 in EString(v1s^v2s)
						|(EFloat v1, EFloat v2) -> let v1s = string_of_float v1 in let v2s = string_of_float v2 in EString(v1s^v2s)
						|(EBool v1, EBool v2) -> let v1s = string_of_bool v1 in let v2s = string_of_bool v2 in EString(v1s^v2s) 
						|(EInt v1, EFloat v2) -> let v1s = string_of_int v1 in let v2s = string_of_float v2 in EString(v1s^v2s)
						|(EFloat v1, EInt v2) -> let v1s = string_of_float v1 in let v2s = string_of_int v2 in EString(v1s^v2s)
						|(EInt v1, EBool v2) -> let v1s = string_of_int v1 in let v2s = string_of_bool v2 in EString(v1s^v2s)
						|(EBool v1, EInt v2) -> let v1s = string_of_bool v1 in let v2s = string_of_int v2 in EString(v1s^v2s)
						|(EBool v1, EFloat v2) -> let v1s = string_of_bool v1 in let v2s = string_of_float v2 in EString(v1s^v2s)
						|(EFloat v1, EBool v2) -> let v1s = string_of_float v1 in let v2s = string_of_bool v2 in EString(v1s^v2s)
						|(EArray (EType t1, q1), EArray(EType t2, q2)) -> if t1 = t2 then EArray(EType t1, q1@q2) else raise  GenericTypeException
						|_-> raise GenericTypeException)
	|Not(e1) -> let v = eval(e1,r) in (match v with
						|EBool b -> if b = true then EBool(false) else EBool(true)
						|_ -> raise NotBoolean)
	|Or(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EBool true, _) -> EBool(true)
						|(_, EBool true) -> EBool(true)
						|(EBool false, EBool false) -> EBool(false)
						|_ -> raise NotBoolean)
	|And(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EBool true, EBool true) -> EBool(true)
						|(EBool b1, EBool b2) -> EBool(false)
						|_ -> raise NotBoolean)
	|Imp(e1,e2) -> let v1 = eval(e1,r) in
						let v2 = eval(e2,r) in (match (v1,v2) with
						|(EBool true, EBool false) -> EBool(false)
						|(EBool b1, EBool b2) -> EBool(true)
						|_ -> raise NotBoolean)
	|IfThenElse(e1,e2,e3) -> let v1 = eval(e1,r) in (match v1 with
						|EBool true -> eval(e2,r)
						|EBool false -> eval(e3,r)
						|_ -> raise NotBoolean)
	|Switch(e1,e2) -> evalQS(e1,e2,r)
	|Let(e1,b) -> let v1 = eval(e1,r) in eval(b,bind(r,v1))
	|LetRec(bf,bl) -> eval(bl,bind(r,FunRecVal(bf,r)))
	|Fun(b) -> Funval(b,r)
	|FunAc(e1,e2) -> let fc = eval(e1,r) in (match fc with
						|Funval(b,r1) -> let qe = evalQE(e2,r) in 
											eval(b,bindList(r1,qe))
						|FunRecVal(b,r1) -> let qe = evalQE(e2,r) in
													let nenv = bind(r1,fc) in
														eval(b,bindList(nenv,qe))
						|_ -> raise NotFunction)
	|Dict(e1) -> let q = evalQD(e1,r) in Dictionary(q)
	|SelectDictName(e1,s) -> (match eval(e1,r) with
						|Dictionary(q) -> searchAndGet(q,s)
						|_ -> raise NotDictionary)
	|SelectDictIndex(e1,e2) -> let vi = eval(e2,r) in (match (eval(e1,r),vi) with
						|(Dictionary(q),EInt v) -> searchAndGetIndex(q,v)
						|_ -> raise GenericTypeException)
	|InsertDict(e1,s,e2) -> (match eval(e1,r) with 
						|Dictionary(q) -> let v = eval(e2,r) in 
											if searchAndGet(q,s) = Unbound then Dictionary((s,v)::q) 
											else let newq = modD(q,s,v) in Dictionary(newq) 
(*Se l'identificatore è già presente allora il vecchio valore associato a quell'identificatore è sovrascritto con il nuovo valore*)
						|_ -> raise NotDictionary)
	|RemoveDict(e1,s) -> (match eval(e1,r) with 
						|Dictionary(q) -> let newq = rmD(q,s) in Dictionary(newq)
						|_ -> raise NotDictionary)
	|RemoveDictIndex(e1,e2) -> let vi = eval(e2,r) in (match (eval(e1,r),vi) with
						|(Dictionary(q),EInt v) -> let newq = rmIndex(q,v) in Dictionary(newq)
						|_ -> raise GenericTypeException)
	|Clear(e1) -> (match eval(e1,r) with 
						|Dictionary(q) -> Dictionary([])
						|_ -> raise NotDictionary)
	|ApplyOver(e1,e2) -> ( match eval(e1,r) with 
						|Dictionary(q) -> let newq = applyFun(q,eval(e2,r),r) in Dictionary(newq)
						|_ -> raise NotDictionary)
	|GetIndex(e1,e2) -> (match eval(e1,r) with
						|EArray(t,q) -> (match eval(e2,r) with
							|EInt(i) -> let a = Array.of_list q
											in Array.get a i
							|_ -> raise NotInteger)
						|_-> raise NotArray)
	|SetIndex(e1,e2,e3) -> (match eval(e1,r) with
						|EArray(t,q) ->  (match (eval(e2,r),t) with 
							|(EInt i,EType t) -> let v = eval(e3,r) in 
								if typeCheck(t,v) = EBool(true) then
									let a = Array.of_list q in
										    Array.set a i v;
											Array.set a i v;
											EArray(EType t, Array.to_list a )
								else
									raise GenericTypeException
							|_ -> raise NotInteger)
						|_-> raise NotArray)
	|Length(e1) -> (match eval(e1,r) with
					|EArray(t,q) -> EInt(List.length q)
					|Dictionary(q) -> EInt(List.length q)
					|_-> raise GenericTypeException)
and
	evalQS (v,q,r) =  (match q with																		(*Funzione per la Switch*)																	
		|(x,y)::qs -> if forAllS(v,x,r) = EBool(true) then eval(y,r) else evalQS(v,qs,r)
		|_ -> raise MatchFailure)
and
	forAllS(v,q,r) = (match (v,q) with
		|([],[]) -> EBool(true)
		|(y::ys,z::zs)-> if eval(Eq(z,String("Default")),r) = EBool(true) then EBool(true) 				(*Caso di default*)
						else if eval(Eq(y,z),r) = EBool(true) then forAllS(ys,zs,r) else EBool(false)
		|_ -> raise MatchFailure)
and
	evalQE (q,r) = (match q with																		(*Funzione per valutazione parametri attuali funzioni*)		
		|[] ->  []
		|x::xs -> eval(x,r) :: evalQE(xs,r))
and
	evalQD (q,r) = (match q with																		(*Funzione per valutare lista Dizionario*)
		|[]->[]
		|(i,e)::qs -> let v = eval(e,r) in 
						(i,v)::evalQD(qs,r))
and 
	evalQA (q,t,r) = (match q with 																		(*Funzione per valutare lista Array*)																		
		|[] -> []
		|x::xs -> let v = eval(x,r) in if typeCheck(t,v) = EBool(true) then v::evalQA(xs,t,r)
									   else raise GenericTypeException)
and
	searchAndGet (q,s) = (match q with																	(*Ricerca s nel dizionario q*)
	|[] -> Unbound
	|(i,v)::qs -> if i = s then v else searchAndGet(qs,s))
and
	searchAndGetIndex (q,vi) = (match q with															(*Ricerca il vi-esimo valore del dizionario*)									
	|[] -> Unbound
	| (i,v)::qs -> if (vi = 0) then v else searchAndGetIndex(qs,vi-1))
and
	modD (q,s,v) = (match q with																		(*Modifica coppia del dizionario*)
	|[] -> []
	|(i,v1)::qs -> if s = i then (i,v)::modD(qs,s,v) else (i,v1)::modD(qs,s,v))
and
	rmD(q,s) = (match q with																			(*Eliminazione coppia del dizionario per nome*)
	|[] -> []
	|(i,v)::qs -> if s = i then rmD(qs,s) else (i,v)::rmD(qs,s))
and
	rmIndex(q,vi) = (match q with																		(*Eliminazione coppia del dizionario per indice*)
	|[] -> []
	| q::qs -> if (vi = 0) then rmIndex(qs,vi-1) else q::rmIndex(qs,vi-1))
and	
	applyFun(q,f,r) = ( match q with																	(*Applicazione funzione ad ogni elemento del dizionario*)
	|[] -> []
	|(i,v)::qs -> (match f with
	       |Funval(b,r1) -> (i,eval(b,bind(r1,v))):: applyFun(qs,f,r)
	       |FunRecVal(b,r1) -> let nenv = bind(r1,f) in
	                               	(i,eval(b,bind(nenv,v))):: applyFun(qs,f,r)
	       |_ -> raise NotFunction))
and
	eqDict(q1,q2) = ( match (q1,q2) with																(*Nozione di uguaglianza dizionario*)
	|([],[]) -> EBool(true)
	|([],_) -> EBool(false)
	|(_,[]) -> EBool(false)
	|((i1,v1)::qs1,(i2,v2)::qs2) -> if v1 = v2 && i1 = i2 then eqDict(qs1,qs2) else EBool(false))
and
	eqArray(t1,t2,q1,q2) = (match(t1,t2) with															(*Nozione di uguaglianza array*)
	|(EType t1, EType t2) -> if t1 = t2 then 
			let rec eqArray2(qa1,qa2) = (match (qa1,qa2) with
				|([],[]) -> EBool(true)
				|([],_) -> EBool(false)
				|(_,[]) -> EBool(false)
				|(qa1::qas1,qa2::qas2) -> if qa1 = qa2 then eqArray2(qas1,qas2) else EBool(false))
			in eqArray2(q1,q2)
		else EBool(false)
	|_ -> raise GenericTypeException)
and 
	init(l,f) = if l > 0 then 																			(*Inizializzazione array con un determinato valore*)
					f l :: init (l-1,f)
				else
					[];;
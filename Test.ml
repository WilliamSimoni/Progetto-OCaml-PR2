(*Test Funzionamento*)

(*Somma: la somma restituisce 10.5, quindi il ceil della somma restituisce 11*)

eval(Ceil(Sum(Float(2.5),Sum(Int(3),Int(5)))),senv);; 

(*Uguaglianza fra due espressioni algebriche (5*2) + 10 =? floor(147/7.23) e (5*2)+10 =? 147/7*)	

eval(Eq(Sum(Mul(Int(5),Int(2)),Int(10)),Floor(Div(Int(147),Float(7.23)))),senv);;
eval(Eq(Sum(Mul(Int(5),Int(2)),Int(10)),Div(Int(147),Int(7))),senv);;

(*La seguente formula logica è una tautologia ((p or q) and (p -> r) and (q -> s)) -> (r or s)*)

eval(Let(Boolean(true),Let(Boolean(false),Let(Boolean(true),Let(Boolean(false),Imp(And(And(Or(Den(0),Den(1)),Imp(Den(0),Den(2))),Imp(Den(1),Den(3))),Or(Den(2),Den(3))))))),senv);;

(*La seguente formula logica è invece una contraddizione (p and ¬p) *)

eval(Let(Boolean(true),And(Den(0),Not(Den(0)))),senv);;
eval(Let(Boolean(false),And(Den(0),Not(Den(0)))),senv);;



(*Test per lo switch (NOTA: il caso 4 solleva un'eccezione perchè i tipi inseriti non sono Int e Boolean)
	1) let p = true in
	   let q = false in
	   let r = true in
	   let s = false in 
	   switch ((p or q) and (p -> r) and (q -> s)) -> (r or s) case
	   |true -> "Forse è una tautologia"
	   |false -> "Non è una tautologia"
	2) let p = true in
	   let ris = (p and ¬p) in
	   switch ris case
	   |true -> 50
	   |false -> 0
	3) let f b x = switch b x case
	   |Int Int -> b * x
	   |Default -> Exception("Tipi non corretti")
	   in f 5 3
	4) let f b x = switch b x case
	   |Int Int -> b * x
	   |Default -> Exception("Tipi non corretti")
	   in f 5 true
	5) let imply e1 e2 = switch e1 e2 with
	   |true false -> false
	   |Boolean Boolean -> true
	   |Default -> Exception("Not a Boolean")
	   in true false
*)

eval(Let(Boolean(true),
	 Let(Boolean(false),
	 Let(Boolean(true),
	 Let(Boolean(false),Switch([Imp(And(And(Or(Den(0),Den(1)),Imp(Den(0),Den(2))),Imp(Den(1),Den(3))),Or(Den(2),Den(3)))],[([Boolean(true)],String("Forse è una tautologia"));([Boolean(false)],String("Non è una tautologia"))]))))),senv);;

eval(Let(Boolean(true),
	 Let(And(Den(0),Not(Den(0))),Switch([Den(1)],[([Boolean(true)],Int(50));([Boolean(false)],Int(0))]))),senv);;
	 
eval(Let(Fun(Switch([Den(0);Den(1)],[([Type("Int");Type("Int")],Mul(Den(0),Den(1)));([String("Default")],Exception("Tipi non corretti"))])),FunAc(Den(0),[Int(5);Int(3)])),senv);;

eval(Let(Fun(Switch([Den(0);Den(1)],[([Type("Int");Type("Int")],Mul(Den(0),Den(1)));([String("Default")],Exception("Tipi non corretti"))])),FunAc(Den(0),[Int(5);Boolean(true)])),senv);;

eval(Let(Fun(Switch([Den(0);Den(1)],[([Boolean(true);Boolean(false)],Boolean(false));([Type("Boolean");Type("Boolean")],Boolean(true));([String("Default")],Exception("Not a Boolean"))])),FunAc(Den(0),[Boolean(true);Boolean(false)])),senv);;

(*Esecuzione funzione : let plusgreatn x y n = let ris = x + y in if ris > n then true else false in plusgreatn 5 6 12 (restituisce false)*) 

eval(Let(Fun(Let(Sum(Den(0),Den(1)),IfThenElse(Greater(Den(3),Den(2)),Boolean(true),Boolean(false)))),FunAc(Den(0),[Int(5);Int(6);Int(12)])),senv);;

(*Esecuzione funzione ricorsiva: let rec fact n = if n <=1 then 1 else n*fact(n-1) in fact 5 (restituisce 120)*)

eval(LetRec(IfThenElse(Not(Greater(Den(1),Int(1))),Int(1),Mul(Den(1),FunAc(Den(0),[Dif(Den(1),Int(1))]))),FunAc(Den(0),[Int(5)])),senv);;

(*Operazioni sul dizionario*)

(*Creazione Dizionario*)

eval(Let(Dict([]),Den(0)),senv);;

(*creazione di un dizionario con valori*)

eval(Let(Dict([("name",String("Giovanni"));("matricola",Int(123456))]),Den(0)),senv);;

(*Accesso a un elemento del dizionario*)

eval(Let(Dict([("name",String("Giovanni"));("matricola",Int(123456))]),SelectDictName(Den(0),"name")),senv);;

eval(Let(Dict([("name",String("Giovanni"));("matricola",Int(123456))]),SelectDictName(Den(0),"matricola")),senv);;

(*Inserimento nel dizionario di età*)

eval(Let(Dict([("name",String("Giovanni"));("matricola",Int(123456))]),
	 Let(InsertDict(Den(0),"età",Int(22)),Den(1))),senv);;

(*Inserimento nel dizionario di name = "Massimo"*)

eval(Let(Dict([("name",String("Giovanni"));("matricola",Int(123456))]),
	 Let(InsertDict(Den(0),"name",String("Massimo")),Den(1))),senv);;

(*Eliminazione dal dizionario di name*)

eval(Let(Dict([("name",String("Giovanni"));("matricola",Int(123456))]),
	 Let(InsertDict(Den(0),"età",Int(22)),
	 Let(RemoveDict(Den(1),"name"),Den(2)))),senv);;
	 
(*Esecuzione di clear*)
eval(Let(Dict([("name",String("Giovanni"));("matricola",Int(123456))]),
	 Let(InsertDict(Den(0),"età",Int(22)),
	 Let(RemoveDict(Den(1),"name"),
	 Let(Clear(Den(2)),Den(3))))),senv);;
	 
(*Esecuzione ApplyOver((fun x -> x+1), my_dict3)*)
eval(Let(Dict([("name",String("Giovanni"));("matricola",Int(123456))]),
	 Let(InsertDict(Den(0),"età",Int(22)),
	 Let(RemoveDict(Den(1),"name"),
	 Let(ApplyOver(Den(2),Fun(Sum(Den(3),Int(1)))),Den(3))))),senv);;
	
(*True solo se il dizionario contiene solo numeri pari
		let MyDict = ["Int1":10;"Int2":22;"Int3":26] in					
			let rec tuttiPari d = switch d match
				|Dictionary([]) -> true
				|Dictionary -> if selectIndex(d,0) mod 2 = 0 then tuttiPari(Remove(d,0)) else false)
				|Default -> Exception("Not a Dictionary")
		in tuttiPari(MyDict)*)

eval(Let(Dict([("int1",Int(10));("int2",Int(22));("int3",Int(26))]),
			LetRec(Switch([Den(2)],[([Dict([])],Boolean(true));
			([Type("Dictionary")],IfThenElse(Eq(Mod(SelectDictIndex(Den(2),Int(0)),Int(2)),Int(0)),FunAc(Den(1),[RemoveDictIndex(Den(2),Int(0))]),Boolean(false)));
			([String("Default")],Exception("Not a Dictionary"))]),FunAc(Den(1),[Den(0)]))),senv);;		

(*	Restituisce un dizionario di booleani = true solo se il valore della coppia era pari
	let t = 2 in 
		let isDivT n = n mod t = 0
			in let MyDict = ["Int1":9;"float1":21.0;"Int2":25]
				in let t = 3 in 
					in Let MyDict2 = ApplyOver(MyDict, Fun x = x + t)
						in ApplyOver(MyDict2, isDivT)
*)
	
eval(Let(Int(2),
	Let(Fun(Eq(Mod(Den(1),Den(0)),Int(0))),
		Let(Dict([("int1",Int(9));("float1",Float(21.0));("int2",Int(25))]),
			Let(Int(3),
				Let(ApplyOver(Den(2),Fun(Sum(Den(4),Den(3)))),
					ApplyOver(Den(4),Den(1))))))),senv);;

(*Fattoriale su ogni elemento del dizionario ["Int1":3;"float1":6.0;"Int2":9]*)

eval(Let(Dict([("int1",Int(3));("float1",Float(6.0));("int2",Int(9))]),
		LetRec(IfThenElse(Not(Greater(Den(2),Int(1))),Int(1),Mul(Den(2),FunAc(Den(1),[Dif(Den(2),Int(1))]))),
			ApplyOver(Den(0),Den(1)))),senv);;
			
(* Crea una matrice di 0 e pone a[4][3] = 10
let a = Array[5] initialized to Int[5]
	let a2 = Set a 4 3 10
		in Get a2 4 3
	*)

eval(Let(EmptyArrayWith(Type("Array"),Int(5),EmptyArray(Type("Int"),Int(5))),SetIndex(Den(0),Int(4),SetIndex(GetIndex(Den(0),Int(4)),Int(3),Int(10)))),senv);;

(* CRea una matrice di 2 e per ogni elemento sulla diagonale i = j a[i][j] = 1
let a = Array[5] initialized to Int[5] initialized to 2
		in letrec f (a,i,j) = if i > 4 then a else f(Set a i j 1, i+1, j+1) 
			in f (a,0,0);
*)

eval(Let(EmptyArrayWith(Type("Array"),Int(5),EmptyArrayWith(Type("Int"),Int(5),Int(2))),
	LetRec(IfThenElse(Greater(Den(3),Int(4)),Den(2),FunAc(Den(1),[SetIndex(Den(2),Den(3),SetIndex(GetIndex(Den(2),Den(3)),Den(4),Int(1)));Sum(Den(3),Int(1));Sum(Den(4),Int(1))])),
		FunAc(Den(1),[Den(0);Int(0);Int(0)]))),senv);;

(*	(Prodotto Scalare tra due array)
	Let a1 = [1;2;3;4;5;6;7;8;9;10] in
		Let a2 = Int[10] initialized to 5 in
			letrec f a1 a2 i= if i < 10 then Int[1] initialized to Get a1 i * Get a2 i ^ f a1 a2 i + 1 
								else Int[0] in
				f a1 a2 0;;
*)		

eval(Let(Array(Type("Int"),[Int(1);Int(2);Int(3);Int(4);Int(5);Int(6);Int(7);Int(8);Int(9);Int(10)]),
	Let(EmptyArrayWith(Type("Int"),Int(10),Int(5)),
	LetRec(IfThenElse(Lower(Den(5),Int(10)),
	Concat(EmptyArrayWith(Type("Int"),Int(1),Mul(GetIndex(Den(3),Den(5)),GetIndex(Den(4),Den(5)))),FunAc(Den(2),[Den(3);Den(4);Sum(Den(5),Int(1))])),
	EmptyArray(Type("Int"),Int(0))),
	FunAc(Den(2),[Den(0);Den(1);Int(0)])))),senv);;
		
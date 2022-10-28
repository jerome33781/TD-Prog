

open Format
open X86_64


(* choses a faire : 
    
    déclaration de variables

*)




(* définition du type *)

type exp = Int of int | Float of float | Plus of exp * exp | Moins of exp * exp | Mult of exp * exp |F_to_I of exp | I_to_F of exp 
         |Div of exp * exp | Mod of exp * exp | Plus_p of exp * exp | Moins_p of exp * exp | Mult_p of exp * exp 


(* dictionnaire pour les variables *)

let dico = ref [] 
let ajout_dico s =	(* s est de la forme "let nom = valeur" *)
	let len = String.length s in
	let res = ref 0 in
	for i = 0 to len-1 do 
		if s.[i] = '=' then res := i ;
	done ;
	let nom = String.sub s 4 (!res-1-4) in
	let valeur = String.sub s (!res+2) (len-(2+(!res)) ) in
	dico := (nom,valeur) :: !dico

let rec var_in_dico dic nom =
	match dic with
	[] -> false 
	| t::q when (fst t) = nom -> true
	| t::q -> var_in_dico q nom
	
let rec val_dico dic nom =
	match dic with
	| t::q when (fst t) = nom -> snd t
	| t::q -> val_dico q nom


(* analyseur lexical à la main *)

let is_int_in_char x = 
  let a = Char.code x in
  ( (a <= 57) && (a >= 48) ) ||a = 46 

let nombre_int string i = (* pour que 33 devienne ["33"] au lieu de ["3", "3"] *)
  let compteur = ref 1 in
  let lim = String.length string - i in
  while (!compteur < lim) && ( is_int_in_char string.[i+ !compteur] ) do 
    compteur := !compteur + 1 ;
  done ;
  if is_int_in_char string.[i] then !compteur else 1
  

let string_to_list s = 
  let rec aux i l = 
    if i >= String.length s then l else 
      let j = nombre_int s i in
      aux (i+j) ((String.sub s i j) :: l )
  in List.rev (aux 0 []) ;;

(* On obtient alors une liste de string de longueur variable : 
    "33 + 5" donnera ["33";" ";"+";" ";"5"]    
    " (5 + 5.) " donnera [" ";"(";"5";" ";"+";" ";"5.";")";" "]  *) 




let rec split_liste l =
  match l with 
    [] -> []
  | t :: q when t = " " ->  split_liste q
  | t :: q when var_in_dico !dico t -> (val_dico !dico t) :: split_liste q (* pour la gestion des variables *)
  | t1 :: t2 :: q  when t2 = "." -> (t1^t2) :: split_liste q
  | t1 :: t2 :: t3 :: q when (t1 ^ t2 ^ t3 = "int") -> "int" :: split_liste q
  | t1 :: t2 :: t3 :: t4 :: t5 :: q when (t1 ^ t2 ^ t3 ^ t4 ^ t5 = "float") -> "float" :: split_liste q
  | t1 :: q -> t1 :: split_liste q 
  
let indice_dot s = 
  let l = String.length s in
  let i = ref 0 in 
  while (!i < l) && (s.[!i] != '.') do
    i := !i + 1
  done ;
  !i 
               
let rec identification liste = (* pour reconnaître directement les int, float et les opérations *)
  match liste with 
  |[] -> [] 
  | t :: q when (indice_dot t <> String.length t) -> 
      begin 
        match t with 
        | t when t = "+." -> t :: identification q
        | t when t = "-." -> t :: identification q
        | t when t = "*." -> t :: identification q
        | t -> ("f"^t) :: identification q
        | t -> t :: identification q
      end
  | t :: q when (is_int_in_char t.[0]) -> ("i"^t) :: identification q 
  | t :: q -> t :: identification q ;;
               

let final_str_to_l s = identification (split_liste ( string_to_list s) );;

final_str_to_l "(33 + 5 -. 6)*7" ;;



(* fin analyseur lexical *) 

(* analyseur syntaxique *)


let float_of_string x = float_of_string x 
    
    
let rec gestion_parenthese liste formule compteur = 
  match formule with 
  | t :: q when (t = ")") && (compteur = 0) -> liste,q
  | t :: q when t ="(" -> gestion_parenthese (liste@[t]) q (compteur+1)
  | t :: q when t =")" -> gestion_parenthese (liste@[t]) q (compteur-1)
  | t :: q  -> gestion_parenthese (liste@[t]) q (compteur) ;;
                 
gestion_parenthese [] ["33";"+";"(";"2";"+";"3";")";")"] 0 ;;
                
(* ordre de priorité utilisé : + = - < % < / < *    *) 
      
let rec syntaxe f =
  match f with 
  
    (* gestion des ints et floats *)

  | [t] when t.[0] = 'i' -> Int ( int_of_string ( String.sub t 1 (String.length t -1))) ;
  | [t] when t.[0] = 'f' -> Float ( float_of_string ( String.sub t 1 (String.length t -1))) ;
      
      
  (* gestion des opérations d'entiers *)
  
      (* gestion de Plus *)

  | t :: op :: f1 when (t.[0] = 'i') && (op = "+") -> Plus(syntaxe [t], syntaxe f1);
      
      (* gestion de Moins *)
  
  
  | t :: op :: f1 when (t.[0] = 'i') && (op = "-") -> Moins(syntaxe [t], syntaxe f1);
 
      
      
        (* gestion de Mult *)
            
  | t :: op :: p :: f1 when ((t.[0] = 'i')&&(op = "*")&&(p = "(")) -> let f2,f3 = gestion_parenthese [] f1 0 in	(* gestion du cas x * (f2) f3     *)
      begin 
        match f3 with 
        | [] -> Mult(syntaxe [t], syntaxe f2) ; (* apres une parenthèse fermante on a forcément soit le vide soit une opération du style +,-,*,/,%   *)
        | op :: f4 when op = "+" -> Plus( Mult (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ;
        | op :: f4 when op = "-" -> Moins( Mult (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ;
        | op :: f4 when op = "*" -> Mult (syntaxe[t], Mult ( syntaxe f2 , syntaxe f4 ) ); 
        | op :: f4 when op = "/" -> Div( Mult (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ;
        | op :: f4 when op = "%" -> Mod( Mult (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ; (* priorité de la multiplication sur le reste *)
      end
      
  | t1 :: op :: t2 :: p:: f1 when ((t1.[0] = 'i')&&(op = "*")&&(t2 = "int")&&(p = "(") )-> let f2,f3 = gestion_parenthese [] f1 0 in (* gestion du cas x * int ou float(f2) f3     *)
      begin 
        match f3 with 
        | [] -> Mult (syntaxe [t1], F_to_I (syntaxe f2)) ; 
        | op :: f4 when op = "+" -> Plus( Mult (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ;
        | op :: f4 when op = "-" -> Moins( Mult (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ;
        | op :: f4 when op = "*" -> Mult (syntaxe[t1], Mult ( F_to_I (syntaxe f2) , syntaxe f4 ) );  
        | op :: f4 when op = "/" -> Div ( Mult (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ; 
        | op :: f4 when op = "%" -> Mod (Mult (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ; (* priorité de la multiplication sur le reste *)
      end
      
  | t1 :: op :: t2 :: f1 when ((t1.[0] = 'i')&&(op = "*")&&(t2.[0] = 'i')) -> (* gestion du cas x * y ... f3     *)
      begin 
        match f1 with 
        | [] -> Mult(syntaxe [t1], syntaxe [t2]) ; 
        | op :: f2 when op = "+" -> Plus( Mult (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
        | op :: f2 when op = "-" -> Moins( Mult (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
        | op :: f2 when op = "*" -> Mult(syntaxe[t1], Mult ( syntaxe [t2] , syntaxe f2 ) );
        | op :: f2 when op = "/" -> Div( Mult (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
        | op :: f2 when op = "%" -> Mod( Mult (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
      end 

      
      
      (* gestion de Div *)
            
  | t :: op :: p :: f1 when ((t.[0] = 'i')&&(op = "/")&&(p = "(")) -> let f2,f3 = gestion_parenthese [] f1 0 in
      begin 
        match f3 with 
        | [] -> Div(syntaxe [t], syntaxe f2) ; (* apres une parenthèse fermante on a forcément soit le vide soit une opération du style +,-,*,/,%   *)
        | op :: f4 when op = "+" -> Plus( Div (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ;
        | op :: f4 when op = "-" -> Moins( Div (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ;
        | op :: f4 when op = "*" -> Div (syntaxe[t], Mult ( syntaxe f2 , syntaxe f4 ) ); 
        | op :: f4 when op = "/" -> Div( Div (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ;
        | op :: f4 when op = "%" -> Mod( Div (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ; (* respect des ordres de priorité *)
      end
      
  | t1 :: op :: t2 :: p:: f1 when ((t1.[0] = 'i')&&(op = "/")&&(t2 = "int")&&(p = "(") )-> let f2,f3 = gestion_parenthese [] f1 0 in
      begin 
        match f3 with 
        | [] -> Div (syntaxe [t1], F_to_I (syntaxe f2)) ; 
        | op :: f4 when op = "+" -> Plus( Div (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ;
        | op :: f4 when op = "-" -> Moins( Div (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ;
        | op :: f4 when op = "*" -> Div (syntaxe[t1], Mult ( F_to_I (syntaxe f2) , syntaxe f4 ) );  
        | op :: f4 when op = "/" -> Div ( Div (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ; 
        | op :: f4 when op = "%" -> Mod (Div (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ; (* respect des ordres de priorité *)
      end
      
  | t1 :: op :: t2 :: f1 when ((t1.[0] = 'i')&&(op = "/")&&(t2.[0] = 'i')) -> 
      begin 
        match f1 with 
        | [] -> Div(syntaxe [t1], syntaxe [t2]) ; 
        | op :: f2 when op = "+" -> Plus( Div (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
        | op :: f2 when op = "-" -> Moins( Div (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
        | op :: f2 when op = "*" -> Mult(Div (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 );
        | op :: f2 when op = "/" -> Div( Div (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
        | op :: f2 when op = "%" -> Mod( Div (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
      end 
      
      
      
(* gestion de Mod *)
            
  | t :: op :: p :: f1 when ((t.[0] = 'i')&&(op = "%")&&(p = "(")) -> let f2,f3 = gestion_parenthese [] f1 0 in
      begin 
        match f3 with 
        | [] -> Mod(syntaxe [t], syntaxe f2) ; (* apres une parenthèse fermante on a forcément soit le vide soit une opération du style +,-,*,/,%  *)
        | op :: f4 when op = "+" -> Plus( Mod (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ;
        | op :: f4 when op = "-" -> Moins( Mod (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ;
        | op :: f4 when op = "*" -> Mod (syntaxe[t], Mult ( syntaxe f2 , syntaxe f4 ) ); 
        | op :: f4 when op = "/" -> Mod (syntaxe[t], Div ( syntaxe f2 , syntaxe f4 ) ); 
        | op :: f4 when op = "%" -> Mod( Mod (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ; (* respect des ordres de priorité *)
      end
      
  | t1 :: op :: t2 :: p:: f1 when ((t1.[0] = 'i')&&(op = "%")&&(t2 = "int")&&(p = "(") )-> let f2,f3 = gestion_parenthese [] f1 0 in
      begin 
        match f3 with 
        | [] -> Mod (syntaxe [t1], F_to_I (syntaxe f2)) ; 
        | op :: f4 when op = "+" -> Plus( Mod (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ;
        | op :: f4 when op = "-" -> Moins( Mod (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ;
        | op :: f4 when op = "*" -> Mod (syntaxe[t1], Mult ( F_to_I (syntaxe f2) , syntaxe f4 ) );  
        | op :: f4 when op = "/" -> Mod (syntaxe[t1], Div ( F_to_I (syntaxe f2) , syntaxe f4 ) );
        | op :: f4 when op = "%" -> Mod (Mod (syntaxe[t1], F_to_I (syntaxe f2) ) , syntaxe f4 ) ; (* respect des ordres de priorité *)
      end
      
  | t1 :: op :: t2 :: f1 when ((t1.[0] = 'i')&&(op = "%")&&(t2.[0] = 'i')) -> 
      begin 
        match f1 with 
        | [] -> Mod(syntaxe [t1], syntaxe [t2]) ; 
        | op :: f2 when op = "+" -> Plus( Mod (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
        | op :: f2 when op = "-" -> Moins( Mod (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
        | op :: f2 when op = "*" -> Mod(syntaxe[t1], Mult ( syntaxe [t2] , syntaxe f2 ) );
        | op :: f2 when op = "/" -> Mod(syntaxe[t1], Div ( syntaxe [t2] , syntaxe f2 ) );
        | op :: f2 when op = "%" -> Mod( Mod (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
      end 
   
  

  (*gestion des opérations de flottants *)
      
      (* gestion de Plus_p *)

  | t :: op :: f1 when (t.[0] = 'f') && (op = "+.") -> Plus_p(syntaxe [t], syntaxe f1);

      
      (* gestion de Moins_p *)
  
  | t :: op :: f1 when (t.[0] = 'f') && (op = "-.") -> Moins_p(syntaxe [t], syntaxe f1);
      
 
      
        (* gestion de Mult_p *)
            
  | t :: op :: p :: f1 when ((t.[0] = 'f')&&(op = "*.")&&(p = "(")) -> let f2,f3 = gestion_parenthese [] f1 0 in
      begin 
        match f3 with 
        | [] -> Mult_p(syntaxe [t], syntaxe f2) ; (* apres une parenthèse fermante on a forcément soit le vide soit une opération du style +,-,* *)
        | op :: f4 when op = "+." -> Plus_p( Mult_p (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ;
        | op :: f4 when op = "-." -> Moins_p( Mult_p (syntaxe[t], syntaxe f2 ) , syntaxe f4 ) ;
        | op :: f4 when op = "*." -> Mult_p (syntaxe[t], Mult_p ( syntaxe f2 , syntaxe f4 ) ); 
      end
      
  | t1 :: op :: t2 :: p:: f1 when ((t1.[0] = 'f')&&(op = "*.")&&(t2 = "float")&&(p = "(") )-> let f2,f3 = gestion_parenthese [] f1 0 in
      begin 
        match f3 with 
        | [] -> Mult_p(syntaxe [t1], I_to_F (syntaxe f2)) ; 
        | op :: f4 when op = "+." -> Plus_p( Mult_p (syntaxe[t1], I_to_F (syntaxe f2) ) , syntaxe f4 ) ;
        | op :: f4 when op = "-." -> Moins_p( Mult_p (syntaxe[t1], I_to_F (syntaxe f2) ) , syntaxe f4 ) ;
        | op :: f4 when op = "*." -> Mult_p (syntaxe[t1], Mult_p ( I_to_F (syntaxe f2) , syntaxe f4 ) ); (* priorité de la multiplication *)  
      end
  
  | t1 :: op :: t2 :: f1 when ((t1.[0] = 'f')&&(op = "*.")&&(t2.[0] = 'f')) -> 
      begin 
        match f1 with 
        | [] -> Mult_p(syntaxe [t1], syntaxe [t2]) ; 
        | op :: f2 when op = "+." -> Plus_p( Mult_p (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
        | op :: f2 when op = "-." -> Moins_p( Mult_p (syntaxe[t1], syntaxe [t2] ) , syntaxe f2 ) ;
        | op :: f2 when op = "*." -> Mult_p(syntaxe[t1], Mult_p ( syntaxe [t2] , syntaxe f2 ) ); 
      end   
      

            
      
        (* gestion du reste *)

        (* gestion des parenthese en début d'expression *)

  | p :: f1 when p = "(" -> let f2,f3 = gestion_parenthese [] f1 0 in
      begin 
        match f3 with 
        | [] -> syntaxe f2 ;
        | op :: f4 when op = "+" -> Plus ( syntaxe f2 ,  syntaxe f4 ) ;
        | op :: f4 when op = "-" -> Moins ( syntaxe f2 ,  syntaxe f4 ) ;
        | op :: f4 when op = "*" -> Mult ( syntaxe f2 ,  syntaxe f4 ) ;
        | op :: f4 when op = "/" -> Div ( syntaxe f2 ,  syntaxe f4 ) ;
        | op :: f4 when op = "%" -> Mod ( syntaxe f2 ,  syntaxe f4 ) ;
        | op :: f4 when op = "+." -> Plus_p ( syntaxe f2 ,  syntaxe f4 ) ;
        | op :: f4 when op = "-." -> Moins_p ( syntaxe f2 ,  syntaxe f4 ) ;
        | op :: f4 when op = "*." -> Mult_p ( syntaxe f2 ,  syntaxe f4 ) ; 
      end
      
        (* gestion des opérateurs float et int *)

  | t :: p :: f1 when (t = "int") && (p = "(" ) -> let f2,f3 = gestion_parenthese [] f1 0 in
      begin 
        match f3 with 
        | [] -> F_to_I(syntaxe f2);  (* Float_to_Int *)
        | op :: f4 when op = "+" -> Plus (  F_to_I ( syntaxe f2  ), syntaxe f4 ) 
        | op :: f4 when op = "-" -> Moins (  F_to_I ( syntaxe f2) , syntaxe f4 )  
        | op :: f4 when op = "*" -> (* gestion du cas int(f1) * x + y  et int(f1) * int(f2) +....  *)
            begin 
              match f4 with 
              | [t] -> Mult (  F_to_I ( syntaxe f2 ) , syntaxe [t] )
              | t :: f5 when t = "int" -> Mult (  F_to_I ( syntaxe f2 ) , F_to_I ( syntaxe f5 ) ) 
              | t :: op :: f5 when op = "+" -> Plus ( Mult (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "-" -> Moins ( Mult (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "*" -> Mult ( Mult (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "/" -> Div ( Mult (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "%" -> Mod ( Mult (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
            end
            
        | op :: f4 when op = "/" -> (* gestion du cas int(f1) / x + y  et int(f1) / int(f2) +....  *)
            begin 
              match f4 with 
              | [t] -> Div (  F_to_I ( syntaxe f2 ) , syntaxe [t] )
              | t :: f5 when t = "int" -> Div (  F_to_I ( syntaxe f2 ) , F_to_I ( syntaxe f5 ) ) 
              | t :: op :: f5 when op = "+" -> Plus ( Div (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "-" -> Moins ( Div (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "*" -> Div (  F_to_I ( syntaxe f2 ) , Mult (syntaxe [t] , syntaxe f5 ))
              | t :: op :: f5 when op = "/" -> Div ( Div (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "%" -> Mod ( Div (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
            end 
            
        | op :: f4 when op = "%" -> (* gestion du cas int(f1) % x + y  et int(f1) % int(f2) +....  *)
            begin 
              match f4 with 
              | [t] -> Mod (  F_to_I ( syntaxe f2 ) , syntaxe [t] )
              | t :: f5 when t = "int" -> Div (  F_to_I ( syntaxe f2 ) , F_to_I ( syntaxe f5 ) ) 
              | t :: op :: f5 when op = "+" -> Plus ( Mod (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "-" -> Moins ( Mod (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "*" -> Mod (  F_to_I ( syntaxe f2 ) , Mult (syntaxe [t] , syntaxe f5 ))
              | t :: op :: f5 when op = "/" -> Mod (  F_to_I ( syntaxe f2 ) , Div (syntaxe [t] , syntaxe f5 ))
              | t :: op :: f5 when op = "%" -> Mod ( Mod (  F_to_I ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
            end 
        
        
      end 
      
  | t :: p :: f1 when (t = "float") && (p = "(" ) -> let f2,f3 = gestion_parenthese [] f1 0 in
      begin 
        match f3 with 
        | [] -> I_to_F (syntaxe f2);  (* int_to_Float *)
        | op :: f4 when op = "+." -> Plus_p (  I_to_F ( syntaxe f2  ), syntaxe f4 ) 
        | op :: f4 when op = "-." -> Moins_p (  I_to_F ( syntaxe f2) , syntaxe f4 )  
        | op :: f4 when op = "*." -> 
            begin 
              match f4 with 
              | [t] -> Mult_p (  I_to_F ( syntaxe f2 ) , syntaxe [t] )
              | t :: f5 when t = "float" -> Mult_p (  I_to_F ( syntaxe f2 ) , I_to_F ( syntaxe f5 ) ) 
              | t :: op :: f5 when op = "+." -> Plus_p ( Mult_p (  I_to_F ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "-." -> Moins_p ( Mult_p (  I_to_F ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              | t :: op :: f5 when op = "*." -> Mult_p ( Mult_p (  I_to_F ( syntaxe f2 ) , syntaxe [t] ), syntaxe f5 )
              
            end
      end
    
  | t :: f1 when t = "int" -> I_to_F(syntaxe f1); 
  | t :: f1 when t = "float" -> F_to_I(syntaxe f1);

      (* gestion de +(exp) et -(exp) *)
  | t :: f1 when t = "+" -> syntaxe f1 
  | t :: f1 when t = "-" -> Moins (Int 0, syntaxe f1 )
                                                    
                                                    
;; 
  
let string_to_expression s = syntaxe ( final_str_to_l s ) ;;



(* Calcul des expressions *)


(* calcul exp renvoie la partie texte du code *)
let data_glob = ref ""
let liste_ind = ref []

let rec pas_dans liste elt = 
	match liste with 	
	[]-> true
	|t::q when t = elt -> false
	|t::q -> pas_dans q elt 

let calcul_total exp = 
	let i = ref 0 in
	let rec calcul exp = 
	
		match exp with 
		
		| Int x -> movq (imm x) (reg rdi)

		| Plus(x,y) ->  calcul x ++
				pushq ( reg rdi ) ++
				calcul y ++
				popq (rsi) ++
				addq (reg rsi) (reg rdi) 

		| Moins(x,y) ->  calcul y ++
				pushq ( reg rdi ) ++
				calcul x ++
				popq (rsi) ++
				subq (reg rsi) (reg rdi) 

		| Mult(x,y) ->  calcul x ++
				pushq ( reg rdi ) ++
				calcul y ++
				popq (rsi) ++
				imulq (reg rsi) (reg rdi) 

		| Div(x,y) -> 	calcul y ++
				pushq ( reg rdi ) ++
				calcul x ++
				movq (reg rdi) (reg rax) ++
				popq (rbx) ++
				xorq (reg rdx) (reg rdx) ++
				idivq (reg rbx) ++
				movq (reg rax) (reg rdi) 

		| Mod(x,y) -> 	calcul y ++
				pushq ( reg rdi ) ++
				calcul x ++
				movq (reg rdi) (reg rax) ++
				popq (rbx) ++
				xorq (reg rdx) (reg rdx) ++
				idivq (reg rbx) ++
				movq (reg rdx) (reg rdi)

		| Float x -> 				
				i := !i + 1 ;
				if pas_dans !liste_ind !i then 

					data_glob := !data_glob ^ ("F"^ (string_of_int !i) ^ ":\n\t.double " ^ (string_of_float x)) ^"\n" ;
					liste_ind := !i :: !liste_ind ;
					
				inline ("\tmovsd F"^(string_of_int !i)^" ,%xmm0 \n")

		| Plus_p(x,y) -> calcul x ++
				 inline "\tmovsd %xmm0, -8(%rsp)\n" ++
				 inline "\tsubq $8, %rsp\n" ++
				 calcul y ++
				 inline "\tmovsd (%rsp), %xmm1\n" ++
				 inline "\taddq $8, %rsp\n" ++
				 addsd (reg xmm1) (reg xmm0)

		| Moins_p(x,y) -> calcul y ++
				 inline "\tmovsd %xmm0, -8(%rsp)\n" ++
				 inline "\tsubq $8, %rsp\n" ++
				 calcul x ++
				 inline "\tmovsd (%rsp), %xmm1\n" ++
				 inline "\taddq $8, %rsp\n" ++
				 subsd (reg xmm1) (reg xmm0)

       		| Mult_p(x,y) -> calcul x ++
				 inline "\tmovsd %xmm0, -8(%rsp)\n" ++
				 inline "\tsubq $8, %rsp\n" ++
				 calcul y ++
				 inline "\tmovsd (%rsp), %xmm1\n" ++
				 inline "\taddq $8, %rsp\n" ++
				 mulsd (reg xmm1) (reg xmm0)

		| F_to_I(x) -> 	calcul x ++
				cvttsd2siq ( reg xmm0 ) ( reg rdi )
				

		| I_to_F(x) ->  calcul x ++
			       	cvtsi2sd ( reg rdi ) (reg xmm0 ) ;
				

    in calcul exp ;;

	

	
(* type_int_expression permet de savoir quel print appeler *)
let type_int_expression exp = 
	match exp with 
	| Float x -> false
	| I_to_F x -> false
	| Plus_p(_,_) -> false 
	| Moins_p(_,_) -> false 
	| Mult_p(_,_) -> false 
	| _ -> true



let aff expression = 
	let code_int = 
		let asm = calcul_total expression in	(* permet de calculer data_glob d'abord car sinon on fait data puis text et dcp data est vide *)
		{ text =  globl "main" ++ label "main" ++ asm ++
		call "print_int" ++ ret ++
		inline "
print_int : 
	movq %rdi, %rsi	
	movq $S_int, %rdi
	movq $0, %rax
	call printf
	ret \n\n";
	
data = inline !data_glob ++ label "S_int" ++ string "%d\n"; } 
		in

	let code_float =
		let asm = calcul_total expression in 
		{ text =  globl "main" ++ label "main" ++ asm ++
		call "print_float" ++ ret ++
		inline "
print_float : 
	movq $S_float, %rdi	
	movq $1, %rax
	call printf
	ret \n\n";

data = inline !data_glob ++ label "S_float" ++ string "%f\n"; } 
	
	in
	
	let c = open_out "resultat.s" in 
	let fmt = formatter_of_out_channel c in 
	let code = if type_int_expression expression then code_int else code_float in
	X86_64.print_program fmt code ;
	close_out c


(* récupération de l'expression *)


let liste_input = ref []

let lecture = 

	let fichier = open_in Sys.argv.(1) in
	try 
		while true do 
			let line = input_line fichier in
			liste_input := !liste_input @ [line]
		done ;
	with End_of_file -> close_in fichier 



let rec traitement l =   (* traitement l ajoute les variables dans un dico via ajout_dico et renvoie l'expression arithmétique *)
	match l with  
	| [x] -> x
	| t :: q -> ajout_dico t ; traitement q


let _ =
	let s = traitement !liste_input in
	let expr = string_to_expression s in  (* où s est le string contenant l'expression *)
  	aff expr 




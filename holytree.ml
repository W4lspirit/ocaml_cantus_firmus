module Treestructure = struct
    type noeud = Notes of int * int * holy_tree and
        holy_tree = noeud list;;

  
 (*Genere la liste des contre-chants possible pour la note cantus, apres la note de contre-chant prev*)
    let noeuds_possibles (cantus:int) (prev:int) =
        let harmo = [0;2;4;5;7;9;11] and
            melo = [0;1;2;3;4;5;7] in
        (*c(antus):int; p(rev):int; i:int; res(ult):int list *)
        let rec aux c p i res =
            match i with
            |3 -> res
            |k when ((List.mem (abs (i-c)) harmo) && (List.mem (abs (i-p)) melo))
                -> (aux c p (i-1) ((Notes (k,c,[]))::res))
            | _ -> (aux c p (i-1) res) 
        in (aux cantus prev 11 []);;

    let init_tree =[ Notes (0,0,[])];;
    

    (*Génere recursivement les contre-chants en fonction du cantus de l holy_tree*)
    let rec magical_aux (cantus:int) (holy_list:holy_tree) =
        match  holy_list with
          | Notes(c_prev,c_f,[]) :: tl -> 
            Notes(c_prev,c_f,(noeuds_possibles cantus c_prev)) :: magical_aux cantus tl
          | Notes(c_prev,c_f,l) :: tl -> 
            Notes(c_prev,c_f,(magical_aux cantus l)) :: magical_aux cantus tl
          |[]->[] 
    ;;

      (*Genere le holy tree des contre-chants possibles d'un cantus passe en param en fonction d'un noeud initial*)
   let ajoute (cantus:int) (h_tree:noeud) =
          match h_tree with
          | Notes (0,0,[])-> Notes (0,cantus,(noeuds_possibles cantus 0))
          | Notes (c_prev,c_f,holy_list)-> Notes (c_prev,c_f,(magical_aux cantus holy_list))
    ;;

    (*Genere le holy tree des contre-chants possibles d'une liste cantus passe en param*)
    let construire_arbre (l : int list) =
      let rec aux l h_tree = 
        match l with
            | [] ->h_tree::[]
            | t::q -> aux q (ajoute t h_tree)
        in aux l (Notes (0,0,[]))
    ;;

    

    let parcours_arbre (a:holy_tree) = [0];;

end;;


module M= Treestructure;;

let v1=M.noeuds_possibles 0 0;;
let v2=M.noeuds_possibles 4 3;;
let v3=M.noeuds_possibles 5 3;;
let v4=M.noeuds_possibles 7 3;;
let v5=M.Notes (0,0,[]);;

let v6=M.ajoute 0 v5;;
let v8=M.construire_arbre [0];;
let v7=M.construire_arbre [0;3];;
let v9=M.construire_arbre [0;3;5];;





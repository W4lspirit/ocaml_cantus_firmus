module type TREESTRUCTURE =
 sig
    type holy_tree
    val init_tree : holy_tree
    val construire_arbre : int list -> holy_tree
    val parcours_arbre : holy_tree -> int list
end;;
    

module Tree : TREESTRUCTURE = struct
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
    let construire_arbre_complet (l : int list) =
      let rec aux l h_tree = 
        match l with
        | [] ->h_tree::[]
        | t::q -> aux q (ajoute t h_tree)
      in aux l (Notes (0,0,[]))
    ;;
    
    
    (*Renvoie le max d'une list d int*)
    let rec max_list (l:int list) = match l with
      | [] -> 0
      | [a] -> a
      | t :: s -> max t (max_list s)
    ;;
    (*renvoie la profondeur max d'un noeud*)
    let rec profondeur_noeud(n:noeud)= match n with
      | Notes(_,_,[]) -> 1
      | Notes(_,_,l) -> 1 + (max_list (List.map profondeur_noeud l))
    ;;
    (*renvoie la profondeur max d'une liste de noeud*)
    let  profondeur_holy_tree h_tree=
      max_list (List.map profondeur_noeud h_tree)
    ;;
    
    (*Genere un  holy tree d'une profondeur_ max donne à partir d'un holy_tree_complet *)
    let rec holy_purge (profondeur_max:int)(profondeur:int) (h_tree:noeud list)(first_time:bool) =
      match h_tree with
      | Notes (c_prev,c_f,[])::tl ->  
        if (profondeur< profondeur_max) then 
          tl
        else 
          Notes (c_prev,c_f,[])::tl
      | Notes (c_prev,c_f,holy_list)::tl-> 
        if first_time then
          (holy_purge profondeur_max profondeur (Notes (c_prev,c_f,(holy_purge profondeur_max (profondeur+1) holy_list true))::tl) false)
        else
          (Notes (c_prev,c_f,holy_list))::(holy_purge profondeur_max profondeur tl true)
      | []->[] 
    ;;
    
    let parcours_arbre (a:holy_tree) = 
        Random.self_init ();
        let rec aux (ht:holy_tree) (res:int list) =
            let size = (List.length ht) in
            let ith_node = (List.nth ht (Random.int size)) in
            match ith_node with
            |Notes (cc,_,[]) -> (List.rev (cc::res))
            |Notes (cc,_,htnext) -> (aux htnext (cc::res))
        in aux a [];;

    (*Construit un holy_tree elague*)
    let construire_arbre (l : int list) =
      let arbre_complet= construire_arbre_complet l in 
      let p_max = profondeur_holy_tree arbre_complet in
      holy_purge p_max 1 arbre_complet true
    ;;
end;;

(*
module M= Tree;;

(*test parcours arbre*)
(*
 * Normalement, c'est sensé donner un output parmi
 * 045 047 048 055 057 058 0510 075 077 078 0710
*)
let v24 = M.construire_arbre [0;3];;
let v25 = (List.map print_int
    (M.parcours_arbre v24));;
*)
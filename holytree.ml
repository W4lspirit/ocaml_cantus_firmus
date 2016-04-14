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
        let rec aux (l : holy_tree) (res : int list) =
            let Notes (_,_,level) = (List.hd l) in
            let size = List.length level in
            match size with
            |0 -> res
            |_ -> let Notes(note,_,next) = List.nth level (Random.int size) in
                aux next (note::res) in
        aux a [];;


    (*Construit un holy_tree elague*)
    let construire_arbre (l : int list) =
      let arbre_complet= construire_arbre_complet l in 
      let p_max = profondeur_holy_tree arbre_complet in
      holy_purge p_max 1 arbre_complet true
    ;;
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

let v10=M.max_list [0;3;5];;
let v11=M.profondeur_noeud v5;;
let v12 =M.profondeur_holy_tree v8;;
let v13 =M.profondeur_holy_tree v7;;

let valid=true;;

let v14=M.holy_purge v12 1 v8 valid;;
let v15=M.holy_purge v13 1 v7 valid;;

(*test purge fin list*)
let v16 = [M.Notes (0, 0,
    [M.Notes (4, 0,[M.Notes (5, 3, []); M.Notes (7, 3, []); M.Notes (8, 3, [])]);
     M.Notes (5, 0,[M.Notes (5, 3, []); M.Notes (7, 3, []); M.Notes (8, 3, []); M.Notes (10, 3, [])]);
     M.Notes (7, 0,[])])];;
let v17 = M.profondeur_holy_tree v16;;
let v18 = M.holy_purge v17 1 v16 true;;

(*test purge debut list*)
let v19 = [M.Notes (0, 0,
    [M.Notes (4, 0,[]);
     M.Notes (5, 0,[M.Notes (5, 3, []); M.Notes (7, 3, []); M.Notes (8, 3, []);M.Notes (10, 3, [])]);
     M.Notes (7, 0,[M.Notes (5, 3, []); M.Notes (7, 3, []); M.Notes (8, 3, []);M.Notes (10, 3, [])])])]
let v20 = M.profondeur_holy_tree v19;;
let v21 = M.holy_purge v20 1 v19 true;;

(*test purge milieu list*)
let v22 = [M.Notes (0, 0,
    [M.Notes (4, 0,[M.Notes (5, 3, []); M.Notes (7, 3, []); M.Notes (8, 3, [])]);
     M.Notes (5, 0,[]);
     M.Notes (7, 0,[M.Notes (5, 3, []); M.Notes (7, 3, []); M.Notes (8, 3, []);M.Notes (10, 3, [])])])]
let v23 = M.profondeur_holy_tree v22;;
let v24 = M.holy_purge v23 1 v22 true;;


(*test parcours arbre*)
(*J'ai toujours le pb de module moi, je sais pas pourquoi
 * du coup, je peux pas tester...
 * Normalement, c'est sensé donner un output parmi
 * 045 047 048 055 057 058 0510 075 077 078 0710
 * Je vais supposer que ça marche... 
 * Pour le foncteur, je ne me souviens plus exactement comment on fait,
 * et il faut qu'on parle des champs à la con, donc j'avance sur la suite *)
let v25 = (List.map print_int
            (M.parcours_arbre [M.Notes (0,0,
                [M.Notes (4,0,
                    [M.Notes (5,3,[]);M.Notes (7,3,[]);M.Notes (8,3,[])]);
                 M.Notes (5,0,
                    [M.Notes (5,3,[]);M.Notes (7,3,[]);M.Notes (8,3,[]);M.Notes (10,3,[])]);
                M.Notes (7,0,
                    [M.Notes (5,3,[]);M.Notes (7,3,[]);M.Notes (8,3,[]);M.Notes (10,3,[])])])]));;

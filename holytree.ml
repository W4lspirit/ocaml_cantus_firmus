module TREESTRUCTURE = struct
    type noeud = Notes of int * int * holy_tree and
        holy_tree = noeud list;;

    (*Genere la liste des contre-chants possible pour la note cantus, apres la note de contre-chant prev*)
    let gen_liste_contre (cantus:int) (prev:int) =
        let harmo = [0;2;4;5;7;9;11] and
            melo = [0;1;2;3;4;5;7] in
        (*c(antus):int; p(rev):int; i:int; res(ult):int list *)
        let rec aux c p i res =
            match i with
            |3 -> res
            |k when ((List.mem (abs (i-c)) harmo) && (List.mem (abs (i-p)) melo))
                -> (aux c p (i-1) (k::res))
            | _ -> (aux c p (i-1) res) 
        in (aux cantus prev 11 []);;

    let init_tree = [Notes (0,0,[])];;

    (*Genere le noeud suivant un noeud node, dont la note sera cantus*)
    let next_node (node:noeud) (cantus:int) =
        let Notes(c_prev,_,_) = node in
        let rec aux tree l_int =
            match l_int with
            |h::t -> aux ((Notes (h,cantus,[]))::tree) t
            |[] -> tree
        in aux (node::[]) (gen_liste_contre cantus c_prev);;

    let construire_arbre (l : int list) = [Notes (0,0,[])];;

    let parcours_arbre (a:holy_tree) = [0];;

end;;


module type TREESTRUCTURE = 
sig
	type holy_tree
	val init_tree : holy_tree
        val construire_arbre:int list -> holy_tree
	val parcours_arbre : holy_tree -> int list
end ;;

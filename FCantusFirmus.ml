module FCantusFirmus (R : READSCORE)(W : WRITESCORE)(T : TREESTRUCTURE)=
struct
  type t ={ mutable c_f: int list;
	    mutable h_l:Y.holy_tree}
  let lire_partition =R.lire_partition ;;
  let ecrire_partition =W.ecrire_partition;;
  let construire_arbre =Y.construire_arbre;;
  let parcours_arbre =Y.parcours_arbre;;
end;;

let R=ReadMidi;;
let W=WriteMidi;;
let T=Tree;;
let FC=FCantusFirmus(R)(W)(T);;

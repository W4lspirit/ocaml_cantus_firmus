module FCantusFirmus (R : Readscore.READSCORE)(W : Writemidi.WRITESCORE)(T : Holytree.TREESTRUCTURE)=
struct
  type t ={ mutable c_f: int list;
	    mutable h_l:T.holy_tree}
  let lire_partition =R.lire_partition ;;
  let ecrire_partition =W.ecrire_partition;;
  let construire_arbre =T.construire_arbre;;
  let parcours_arbre =T.parcours_arbre;;
end;;

module R=Readmidi.ReadMidi;;
module W=Writemidi.WriteMidi;;
module T=Holytree.Tree;;
module FC= FCantusFirmus(R)(W)(T);;
let c_f =[2;6;6;7;4;1;3;7;1;2];;
let a=FC.construire_arbre c_f;; 
let cc=FC.parcours_arbre a;;

let test = (FC.ecrire_partition (0::c_f) cc "output_test.midi");;

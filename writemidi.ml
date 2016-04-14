module type WRITESCORE = sig
    exception ListeIncorrecte of string
    val ecrire_partition : int list -> int list -> string -> unit
end;;

module WriteMidi = struct
    exception ListeIncorrecte of string

    let ecrire_partition (cantus : int list) (cchant : int list) (name:string) =
    if (List.length cantus = List.length cchant)
    then
    else

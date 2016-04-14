open MIDI.MIDI
module type WRITESCORE = sig
    exception ListeIncorrecte of string
    val ecrire_partition : int list -> int list -> string -> unit
end;;

module WriteMidi : WRITESCORE = struct
    exception ListeIncorrecte of string

    let map_to_midi = function
        |0 -> 62
        |1 -> 64
        |2 -> 65
        |3 -> 67
        |4 -> 69
        |5 -> 71
        |6 -> 72
        |7 -> 74
        |8 -> 76
        |9 -> 77
        |10 -> 79
        |_ -> 81

    let delta = match (Random.int 2) with
    |0 -> 200
    |_ -> 300;;


    let ecrire_partition (cantus : int list) (cchant : int list) (name:string) =
    if (List.length cantus = List.length cchant)
    then let rec aux (c:int list) (cc:int list) (res:track) =
        let d = delta in
        match c with
        | [] -> (List.rev res)
        | _ -> (aux (List.tl c) (List.tl cc)
            (*Off du cantus*)
            ((0,0, NoteON (map_to_midi (List.hd c),0))::
                (*Off du contre-chant*)
                (d,0, NoteON (map_to_midi (List.hd cc),0))::
                (*On du cantus*)
                (0,0, NoteON (map_to_midi (List.hd c), 80))::
                (*On du contre-chant*)
                (0,0, NoteON (map_to_midi (List.hd cc),80))::res))
    in let (myMidi : midi) = (100,((aux cantus cchant [])::[]))
    in (write myMidi name)
    else raise (ListeIncorrecte ("Les taille du contre-chant et du cantus firmus ne sont pas les mêmes."));;
end;;

let test = (WriteMidi.ecrire_partition [1;2;3] [2;4;6] "output_test.midi");;

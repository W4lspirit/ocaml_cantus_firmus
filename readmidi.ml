module ReadMidi : Readscore.READSCORE = struct
    exception NonValidFile
    exception AmbitusWrong of int
    exception ChantPolyphonique

    type midievent = MIDI.MIDI.midievent

    let map_from_midi c= match c with
        | 62 -> 0
        | 64 -> 1
        | 65 -> 2
        | 67 -> 3
        | 69 -> 4
        | 71 -> 5
        | 72 -> 6
        | 74 -> 7
        | 76 -> 8
        | 77 -> 9
        | 79 -> 10
        | 81 -> 11
        | _ -> raise (AmbitusWrong c)

    let lire_partition (name:string) =
        if (Sys.file_exists name)
        then let (_,trList) = MIDI.MIDI.read name in
        let track = List.hd trList in
        let rec aux (tr:MIDI.MIDI.track) (res:int list) (isOn:bool) =
            match tr with 
            |[] -> res
            |(_,_, NoteOFF (note,_))::t ->
                    aux t (note::res) false
            |(_,_, NoteON (note,0))::t ->
                    aux t (note::res) false
            |(_,_, NoteON (note,_))::t when isOn ->
                    raise ChantPolyphonique
            |(_,_, NoteON _)::t -> aux t res true
            |_::t -> aux t res isOn 
        in (List.map map_from_midi (aux track [] false))
        else raise NonValidFile;;
end;;

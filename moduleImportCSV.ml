module type READSCORE =
    sig
        exception NonValidFile
        val lire_partition : string -> int list
        (*val lire_partition : string -> unit*)
    end;;



module Readscore : READSCORE =
    struct
    exception NonValidFile;;

    let update_res (res:int list ref) (l:int list) =
        let tmp = !res@l in
        res := tmp;;

    let parse_file (name:string) = 
        let (res : int list ref) = ref [1] in
        res := [];
        let stream = open_in name and
        comma = Str.regexp"," in
        try
            while true do
                let line = input_line stream in
                let tab = List.map int_of_string (Str.split_delim comma line) in
                let tmp = !res@tab in
                res := tmp;
            done;
            !res;
        with End_of_file -> (close_in stream);!res;;

    let lire_partition (name:string) = 
        match (Sys.file_exists name) with
        |false -> raise NonValidFile;
        (*DEBUG*)
        (*|true -> let test = (parse_file name) in
                List.iter print_int test;;*)
        |true -> (parse_file name);;

    end;;

(*let testFct = (List.map print_int (Readscore.lire_partition "test.csv"));;*)

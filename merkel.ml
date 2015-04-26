let merkel_chan chan =
    let buffsize = 256 in
    let chanlen = in_channel_length chan in
    print_endline ("created buffer, channel len: " ^ string_of_int chanlen) ;
    let hash_block take =
      Sha256.to_hex (Sha256.channel chan take) in

    let hash_string (str: string): (string) =
      Sha256.to_hex (Sha256.string str) in

    let current = ref chanlen in
    let hashes = ref [] in

    while !current - buffsize > 0 do
        hashes := List.append !hashes [hash_block buffsize];
        current := !current - buffsize
    done;
    hashes := List.append !hashes [hash_block !current];

    (* val merkel: string list -> string list ref -> string list *)
    let merkel (hashlist: string list): (string list) =
      let output = ref [] in
      let rec merkel_loop (hashlist: string list) (output: string list ref) =
        match hashlist with
          x::y::rest ->
            let hash = x ^ y in
            let hashed = hash_string hash in
            output := List.append !output [hashed];
            merkel_loop rest output
        | x::[] ->
            output := List.append !output [x];
            merkel_loop [] output
        | [] -> !output in

      merkel_loop hashlist output in

    while List.length !hashes > 1 do
        print_endline "round";
        hashes := merkel !hashes
    done;

    List.iter print_endline !hashes;;

let merkel_hash_file filename =
    let chan = open_in filename in
    print_endline "In read fn";
    merkel_chan chan;
    close_in chan;;

let () = merkel_hash_file (Array.get Sys.argv 1);;

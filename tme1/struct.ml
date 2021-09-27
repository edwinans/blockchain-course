open Blake2
open Hex

let hash b = 
    let res = Blake2b.direct b 32 in 
        match res with
            | Hash (h) -> h
            
let hash_id surname name = 
    let id = surname ^ ":" ^ name in
    let res = Blake2b.direct (Bytes.of_string id) 32 in 
    match res with
        | Hash (h) -> h

let encode_int n =
    begin
        let b = Bytes.create 4 in
        Bytes.set_int32_be b 0 (Int32.of_int n);
        b
    end

let hash_value h n = 
    let res = Bytes.cat h (encode_int n)
    in 
        hash res

let string_of_bytes b = show(of_bytes(b)) 

let _ =
    let sn = hash_id "nakamoto" "satoshi" in 
    begin
        print_endline (string_of_bytes(sn));
        print_endline (string_of_bytes(hash_value sn 123))
    end
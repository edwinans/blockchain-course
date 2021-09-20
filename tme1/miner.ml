open Struct

let count_zerop_byte b = 
    let rec aux mask i = 
        if mask = 0 then i 
        else if mask land b = mask then i 
        else aux (mask/2) (i+1) in 
    aux 128 0

let count_zero_prefix b = 
    let rec aux i acc = 
        if i = Bytes.length b then acc
        else 
            let ct = count_zerop_byte (int_of_char(Bytes.get b i)) in 
            if ct < 8 then acc+ct 
            else aux (i+1) (acc+ct) in 
    aux 0 0

let is_valid id nonce n =
    let h = hash_value id nonce in 
    count_zero_prefix(h) >= n

let mine id n = 
    let rec aux nonce = 
        if is_valid id nonce n then nonce 
        else aux (nonce+1) in 
    aux 0

let _ =
    let sn = hash_id "nakamoto" "satoshi" in 
    begin 
        let b = Bytes.make 10 '\000' in 
        Bytes.set b 5 '\001';

        print_int (count_zero_prefix b);
        print_endline "";
        print_int (mine sn 15)
    end
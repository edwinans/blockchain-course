open Struct
open Printf

let encode_account (id, balance) =
    let id_b = Bytes.of_string id and balance_b = encode_int balance in 
    Bytes.cat id_b balance_b

let decode_account e_account = 
    let id_b = Bytes.create 64 and balance_b = Bytes.create 4 in 
        begin
            Bytes.blit e_account 0 id_b 0 64;
            Bytes.blit e_account 64 balance_b 0 4;
            (Bytes.to_string id_b,
                Int32.to_int (Bytes.get_int32_be balance_b 0))
        end

let encode_state (accounts: (string * int) list) =
    let len = List.length accounts in
        let l = (encode_int len) :: (List.map encode_account accounts) in 
            Bytes.concat Bytes.empty l

let decode_state state = 
    let rec aux i len acc = 
        if(i>=len) then List.rev acc else 
        let b = Bytes.create 68 in 
            begin 
                Bytes.blit state (i*68+4) b 0 68;
                aux (i+1) len ((decode_account b) :: acc)
            end in 
    let len_b = Bytes.create 4 in 
        begin
            Bytes.blit state 0 len_b 0 4;
            let len = Int32.to_int (Bytes.get_int32_be len_b 0) in 
                aux 0 len []
        end

let verify state_hash state_string = 
    state_hash = string_of_bytes (hash (
            Hex.to_bytes (`Hex state_string
            )
        ))

let verify_file file = 
    let chan = open_in file in
        let i = ref 1 in
        try while true; do
            let expected = input_line chan in
            let data = input_line chan in
            let _ = input_line chan in
            if(verify expected data) then printf "case %d: ok\n" !i else printf "case %d: nok\n" !i;
            i := !i + 1; 
        done; 
        with End_of_file -> close_in chan

let print_account account = 
    printf "(%s, %d)\n" (fst account) (snd account)

let print_state state =
    begin
        print_string "[";
        List.iter print_account state;
        print_endline "]"
    end


let _ = 
    let ac1 = ("1dc653a1447946592fe2871eeb01d8fd6ae353bf04ab789199e38777da3fd0c7", 1003) and
        ac2 = ("ad415c298389574a24f009671697dd58a717ec04aaa79bd39a130b1ae7a4b2a9", 8532) and
        ac3 = ("b6a46ab620ab41132a7e062bee0bd7ef6af99d5c25b9021edcb949f2cd6c2bbc", 100) and
        ac4 = ("d91340a0a4fc7283117fb7871a95e983455275347662345ffaaa75d674def6ec", 943) and
        ac5 = ("ff9f179535d17c8f29d7eb8ad3432eb8b16ce684b48527b12a1a71f10d3e63ec", 755) in
    let ec = encode_account ac1 in 
    begin 
        print_endline (string_of_bytes ec);
        let id,b = decode_account ec in 
            printf "(%s, %d)\n" id b;
        printf "%s\n" (string_of_bytes (encode_state [ac1;ac2;ac3;ac4;ac5]));
        print_state (decode_state (encode_state [ac1;ac2;ac3;ac4;ac5]));
        verify_file "tme1-data.txt"
    end
open Mirage_crypto_pk.Dsa

let gen_keys () =
    let sk = generate (`Exactly (256, 256)) in 
        let pk = pub_of_priv sk in 
            (pk, sk)



let _ = 
begin 
    let pk,sk = gen_keys () and msg = Cstruct.of_string "foobar" in 
    let signature = sign ~key:sk msg in
    print_endline (Cstruct.to_string (fst signature));
    print_endline (string_of_bool (verify ~key:pk signature msg))
end
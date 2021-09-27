open Blake2

type merkle_tree = 
    | Nil
    | Node of Bytes.t * merkle_tree * merkle_tree

let get_hash mt = 
    match mt with 
    | Nil -> Bytes.empty
    | Node (h, _, _) -> h

let hex_of_bytes b = Hex.show(Hex.of_bytes(b)) 

let bytes_of_hex s = Hex.to_bytes (`Hex s)

let rec print_merkle tree = 
    match tree with 
    | Nil -> print_endline "Nil"
    | Node(h, l, r) -> 
        (print_endline (hex_of_bytes h);
            print_merkle l;
            print_merkle r;
        )

let rec pp fmt = function 
| Nil -> ()
| Node (h, fg, fd) ->
    Format.fprintf fmt "@[<v 2>%a@ %a@ %a@]" 
        Hex.pp (Hex.of_bytes h) pp fg pp fd

let hash b = 
    let res = Blake2b.direct b 32 in 
        match res with
            | Hash (h) -> h

let concat_hash h1 h2 =
    hash (Bytes.cat h1 h2)

let create_merkle_tree l = 
    let rec aux tmp acc = match tmp with 
        | [] -> aux (List.rev acc) []
        | [x] -> x 
        | x :: y :: xs -> 
            let h1 = get_hash x and h2 = get_hash y in 
                let h = concat_hash h1 h2 in 
                    aux xs (Node(h, x, y) :: acc)
        in 
    aux (List.map (fun x -> Node(hash (Bytes.of_string x), Nil, Nil)) l) []


let find_witness tree leaf = 
    let rec aux tree = 
        match tree with 
        | Nil -> Nil, false
        | Node (_, Nil, Nil) as n -> 
            n, n = leaf
        | Node (h, l, r) -> 
            let lw, c1 = aux l and rw, c2 = aux r  in 
                if (c1 || c2) then Node(h, lw, rw), true 
                else Node(h, Nil, Nil), false in 
    fst (aux tree)

let verify witness root = 
    let rec aux tree = 
        match tree with 
        | Nil -> true 
        | Node(_, Nil, Nil) -> true
        | Node(h, l, r) -> 
            concat_hash (get_hash l) (get_hash r) = h &&
                aux l && aux r in

    root = get_hash witness && aux witness


let _ = 
    begin 
        let m = create_merkle_tree ["a";"b";"c";"d";"e";"f";"g";"h"] in 
        Format.printf "merkle:@\n%a@." pp m;
        print_endline (string_of_bool 
            (verify m (bytes_of_hex "99cf46edb69d02e18cce4989225f80386ff39d8b355cdea2971b532f901a055d"))
        );
        let w = find_witness m (Node(hash (Bytes.of_string "c"),Nil,Nil)) in 
        Format.printf "witness:@\n%a@." pp w;
        print_endline (string_of_bool
            (verify w (bytes_of_hex "99cf46edb69d02e18cce4989225f80386ff39d8b355cdea2971b532f901a055d"))
        )
    end
type t =
  {
    order : int;
    coeffs : (int, Q.t) Hashtbl.t
  }

let print { order; coeffs } =
  let n = string_of_int order in
  Seq.fold_left (fun x y -> String.concat "" [x;y]) ""
    (Seq.map (fun (pow, coeff) -> String.concat "" [ Q.to_string coeff
                                                   ;"*e("; n; ")^"
                                                   ; string_of_int pow ])
       (Hashtbl.to_seq coeffs))

let singleton k v = let m = Hashtbl.create 1 in (Hashtbl.add m k v; m)

let mapkeys f m = let new_tbl = Hashtbl.create (Hashtbl.length m) in
  Hashtbl.iter (fun k v -> Hashtbl.add new_tbl (f k) v) m ;
  new_tbl

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let gcd_of_list l = match l with
  | [] -> 1
  | x::xs -> List.fold_left gcd x l

let gcd_of_cyclotomic { order; coeffs } = gcd_of_list (order :: (List.of_seq (Hashtbl.to_seq_keys coeffs)))

let gcd_reduce ({ order = n ; coeffs = c } as z) = match gcd_of_cyclotomic z with
  | 1 -> z
  | d -> { order = n/d ; coeffs = mapkeys (fun k -> k/d) c}

let delete_key k mp = let new_mp = Hashtbl.copy mp in
  Hashtbl.remove new_mp k ;
  new_mp

let insertwith f k new_v m = let new_m = Hashtbl.copy m in
  match (Hashtbl.find_opt m k) with
  | None -> Hashtbl.add new_m k new_v ; new_m
  | Some old_v -> Hashtbl.replace new_m k (f new_v old_v) ; new_m

let replacements n p r = let s = n/p in
  let pair i = [r-s*i, r+s*i] in
  let rec f next so_far = match (List.filter (fun x -> 0 <= x < n) (pair next)) with
    | [] -> so_far
    | next_less -> f (next + 1) (next_less @ so_far) in
  f 1 []

let replace n p r mp = match (Hashtbl.find_opt mp r) with
  | None -> mp
  | Some rat -> List.fold_right (fun k m -> insertwith Q.(+) k (Q.neg rat) m) (delete_key r mp) (replacements n p r)

let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

let factorise n =
  let rec aux d n =
    if n = 1 then [] else
    if n mod d = 0 then
      match aux d (n / d) with
      | (h,n) :: t when h = d -> (h,n+1) :: t
      | l -> (d,1) :: l
    else aux (d+1) n
  in
  aux 2 n

let pq_pairs n = List.map (fun (p, k) -> (p, p^k)) (factorise n)

let range first last = List.init (last-first+1) (fun x -> x + first)

let include_mods

let remove_exps n p q = match p with
  | 2 -> List.concat_map (include_mods n q) (List.map (fun x -> (n/q) * x) (range (q/2) (q-1)))
  | p -> let m = ((q/p) - 1)/2 in
    List.concat_map (include_mods n q) (List.map (fun x -> (n/q) * x) (range (-1*m) m))

let extraneous_powers n = match n with
  | n when n < 0 -> raise (Failure "need n >= 0")
  | n -> uniq (List.map (fun (p,q) -> (List.map (fun r -> (p,r)) (remove_exps n p q))) (pq_pairs n))

let convert_to_base n mp = List.fold_right (fun (p,r) m -> replace n p r m) mp (extraneous_powers n)

let try_rational x = x

let try_reduce x = x

let cyclotomic order coeffs = try_reduce (try_rational (gcd_reduce { order; coeffs }))

(* nth root of unity *)
let e n = match n with
  | n when n < 0 -> raise (Failure "need n >= 0")
  | 1 -> { order = 1;
           coeffs = singleton 0 Q.one
         }
  | _ -> (cyclotomic n (convert_to_base n (singleton 1 Q.one)))

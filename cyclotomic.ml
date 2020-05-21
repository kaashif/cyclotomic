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

let gcd_of_cyclotomic { order; coeffs } = gcd_of_list order :: (List.of_seq (Hashtbl.to_seq_keys coeffs))

let gcd_reduce ({ order = n ; coeffs = c } as z) = match gcd_of_cyclotomic z with
  | 1 -> z
  | d -> { order = n/d ; coeffs = mapkeys (fun k -> k/d) c}

let convert_to_base _ coeffs = coeffs

let try_rational x = x

let try_reduce x = x

let cyclotomic order coeffs = try_reduce (try_rational (gcd_reduce { order; coeffs }))

(* nth root of unity *)
let e n = match n with
  | n when n < 0 -> None
  | 1 -> Some { order = 1;
                coeffs = singleton 0 Q.one
              }
  | _ -> Some (cyclotomic n (convert_to_base n (singleton 1 Q.one)))

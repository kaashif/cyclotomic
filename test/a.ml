(*
   Tests for Cyclotomic
*)

let check msg x = Alcotest.(check bool) msg true x

let test_time () =
  check "now is greater than 1000" (Cyclotomic.now () > 1000.);
  check "now is fix" (Cyclotomic.now () > 1_522_882_648.)

let tests = [
  "time", `Quick, test_time;
]

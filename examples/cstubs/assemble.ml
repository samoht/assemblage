open Assemblage

let date = cstubs "date" []

let date_cmd =
  let date_cmd = unit "date_cmd" [date] in
  bin "date-cmd" [date_cmd]

let test =
  test "test" [] [
    test_bin date_cmd ()
  ]

let () =
  create "date" [test]

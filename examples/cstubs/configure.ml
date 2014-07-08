open Assemblage

let date = cstubs [] "date"

let date_cmd =
  let date_cmd = cu [date] "date_cmd" in
  bin [date_cmd] "date-cmd"

let test =
  test [] [
    test_bin date_cmd (fun _ -> [])
  ] "test"

let () =
  create [test] "date"

open WagonC
open WagonC
open WagonProgram
let () =
  if Array.length Sys.argv < 2 then failwith "usage: wagonc <output.c>"
  else 
    let prog_string = main |> output_prog in
    let out_ch = open_out Sys.argv.(1) in
    output_string out_ch prog_string;
    close_out out_ch
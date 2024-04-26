module Controlwork

let square n =
    let constructLine x =
        if (x = n || x = 1) then (String.replicate n "*")
            else ("*" + (String.replicate (n - 2) " ") + "*")
    String.concat "\n" (List.map constructLine [1..n])
  

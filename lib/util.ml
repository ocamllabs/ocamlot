
let randomish_string k =
  let buf = String.make k '\000' in
  let rec gen x =
    if k=x then buf
    else (buf.[x] <- Char.chr (Random.int 0x100); gen (x+1))
  in gen 0


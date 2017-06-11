
let ilosc_probek = 1000
let nazwa_pliku = "example_file"
let max_x = 30.
let max_y = 30.

let kolo_fun x y =
  if (x-.15.)*.(x-.15.) +. (y-.15.)*.(y-.15.) < 100. then 1 else -1

let prosta_fun x y =
  if 3.*.x+.2.*.y-.5.>30. then 1 else -1

let prosta_z_szumem_fun x y =
  if x-.y>5. then 1 else
    if x-.y<(-5.) then -1 else 
      if Random.bool () then 1 else -1

let generuj ilosc_probek nazwa_pliku funkcja =      
  let file = open_out nazwa_pliku in
  for i = 1 to ilosc_probek do 
    let x = Random.float max_x in
    let y = Random.float max_y in
    let v = funkcja x y in
    Printf.fprintf file "%d 1:%f 2:%f\n" v x y
  done;
  close_out file
  
let _ = 
  Random.self_init ();
  generuj 1000 "prosta1000.trn" prosta_fun;
  generuj 10000 "prosta10000.trn" prosta_fun;
  generuj 10000 "prosta10000.tst" prosta_fun;
  generuj 1000 "kolo1000.trn" kolo_fun;
  generuj 10000 "kolo10000.trn" kolo_fun;
  generuj 10000 "kolo10000.tst" kolo_fun;
  generuj 1000 "prosta_z_szumem1000.trn" prosta_z_szumem_fun;
  generuj 10000 "prosta_z_szumem10000.trn" prosta_z_szumem_fun;
  generuj 10000 "prosta_z_szumem10000.tst" prosta_z_szumem_fun;
  ()

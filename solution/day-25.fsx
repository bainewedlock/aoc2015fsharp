

#time
let rec gen v pos target = 
  let v' = v * (bigint 252533) % (bigint 33554393)
  match pos with
  | _ when pos = target -> v
  | x, y when y = 1     -> gen v' (1  , x+1) target
  | x, y                -> gen v' (x+1, y-1) target
let answer = gen (bigint 20151125) (1, 1) (3029,2947)
#time

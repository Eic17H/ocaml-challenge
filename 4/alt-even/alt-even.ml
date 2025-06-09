let rec alt_even x =
  if (x<100) then (x mod 2 = 0) && (x/10 mod 2 = 1 || x/10 == 0)
  else (alt_even (x mod 100)) && (alt_even (x/100))
;;
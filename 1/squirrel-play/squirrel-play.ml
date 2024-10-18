type season = Spring | Summer | Autumn | Winter;;
let squirrel_play a b = a >= 15 && a <= (if(b==Summer) then 35 else 30);;
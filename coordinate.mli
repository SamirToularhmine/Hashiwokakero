type coordinate
      
val coord_from_pair : (int * int) -> coordinate
val pair_from_coord : coordinate -> (int * int)
val fstcoord : coordinate -> int   
val sndcoord : coordinate -> int
val (>~) : coordinate -> coordinate -> bool
val (>~~) : coordinate -> coordinate -> bool
      
val filtercoord : (coordinate -> coordinate -> bool) -> coordinate -> coordinate -> coordinate
val toString : coordinate -> string


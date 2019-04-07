type coordinate
      
val create : (int * int) -> coordinate
val fstcoord : coordinate -> int   
val sndcoord : coordinate -> int
val (>~) : coordinate -> coordinate -> bool
val (>~~) : coordinate -> coordinate -> bool
      
val filtercoord : (coordinate -> coordinate -> bool) -> coordinate -> coordinate -> coordinate
val toString : coordinate -> string


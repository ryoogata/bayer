require(dplyr)


bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}


onestr <- function(str){
  num <- nchar(str)
  cha <- substr(str, 1,1)
  decimals <- charToRaw(cha) %>% 
    rawToBits(.) %>%
    bitsToInt(.)
  
  for(i in 2:80){
    dec <- substr(str, i,i) %>%
      charToRaw(.) %>%
      rawToBits(.) %>%
      bitsToInt(.)
    
    decimals <- append(decimals, dec)
  }
  return(decimals)
}

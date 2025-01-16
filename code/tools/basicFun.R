s.err <- function(x){
  temp <- sd(x)/sqrt(length(x))
  return(temp)
}

s.err.na <- function(x){
  temp <- sd(x, na.rm = T)/sqrt(length(which(!is.na(x))))
  return(temp)
}


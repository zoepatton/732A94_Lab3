

euclidean <-
function(x,y){
  if (is.integer(x)&is.integer(y)){
    remainder_1<-x
    remainder_2<-y
    while (remainder_2 != 0){
      remainder <- remainder_1%%remainder_2
      remainder_1 <- remainder_2
      remainder_2 <- remainder
    }
  }
    else{
      stop("stop")
    }
    return(remainder_1)
}

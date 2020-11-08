#' Euclidean algorithm
#'
#' \code{euclidean} returns the greatest common divisor of two integers.
#'
#' The algorithm is a method for computing the largest possible number that divides two numbers without a remainder. It works by replacing the larger number by its difference with the smaller of the two numbers and repeating this process until the two numbers become equal and the remainder is zero. 
#'
#' @param x numeric.
#' 
#' @param y numeric.
#' 
#' @return If the two inputs are integer or numeric, then the output
#'   will be an integer, specifically the highest number that will divide the two inputs. Otherwise the function will stop and show an error stating the inputs need to be an integer or numeric. 
#'   
#' @export
#' @source \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Wikipedia}
#'   


euclidean <-
function(x,y){
  if (is.numeric(x)&is.numeric(y)){
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


#' Find the Greatest Common Divisor (GCD) of two integers.
#'
#' The Euclidean Algorithm is a technique used for finding the Greatest Common divisor of two integers.
#'
#' @param a A number.
#' @param b A number.
#' @return The sum of \code{a} and \code{b}.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' 
#' @references 
#' \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Euclidean}
#' 
#' @export


euclidean <- function(a, b)
{ if(!(is.numeric(a)&is.numeric(b)))
  stop("Illegal input!")
  while (!(a==0|b==0))
  {
    if (a>b)
    { remainder = a%%b
    a <- remainder
    }
    else
    {remainder <- b%%a
    b<- remainder
    }
  }
  output <- abs(a-b)
  return(output)
}
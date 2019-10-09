#' Adding Vectors
#'
#' addvecs is used to add multiple vectors elementwise.
#'
#' @param ... a list of vectors. All inputs must be of equal length.
#'
#' @return addvecs returns a vector of equal length to the inputs.
#' @export
#'
#' @examples temp <- seq(0,10)
#' x1 <- sample(temp, 3)
#' x2 <- sample(temp, 3)
#' addvecs(x1,x2)
#' addvecs(x1,x2,x1)
#'
addvecs <- function(...){
  # adds all vectors elementwise

  temp <- vector(length = length(list(...)[[1]]))
  for(i in list(...)){
    temp <- temp + i
  }
  return(temp)
}

#' Multiplying Vectors Elementwise
#'
#' multvecs is used to multiply a list of vectors elementwise.
#'
#' @param ... a list of vectors. All inputs must be of equal length.
#'
#' @return multvecs returns a vector of equal length to the inputs.
#' @export
#'
#' @examples temp <- seq(0,10)
#' x1 <- sample(temp, 3)
#' x2 <- sample(temp, 3)
#' multvecs(x1,x2)
#' multvecs(x1,x2,x1)
#'
multvecs <- function(...){
  # multiplies all input vectors elementwise

  temp <- c(rep(1,length(list(...)[[1]])))
  for(i in list(...)){
    temp <- temp * i
  }
  return(temp)
}

#' Dot Product
#'
#' @param ... two vectors of equal length.
#'
#' @return dotprod returns the dot product of two vectors.
#' @export
#'
#' @examples temp <- seq(0,10)
#' x1 <- sample(temp, 3)
#' x2 <- sample(temp, 3)
#' dotprod(x1,x2)
dotprod <- function(...){
  # computes the dot product

  # can only dot product two vectors
  if(length(list(...)) != 2){
    stop('can only compute the dot product of two vectors')
  }

  temp <- c(rep(1,length(list(...)[[1]])))
  for(i in list(...)){
    temp <- temp * i
  }
  return(sum(temp))
}

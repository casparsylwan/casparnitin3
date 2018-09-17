#' GCD
#'
#' @description Finds the greatest common divisor!
#' @param x1 First
#' @param x2 Second
#'
#' @return
#' @export
#'
euclidean <- function(x1,x2){

  is.scalar <- function(femto) is.numeric(femto) && length(femto) == 1L
  x3 <- abs(x1)
  x4 <- abs(x2)
  if(is.scalar(x1)==FALSE || is.scalar(x2)==FALSE){
    stop("The input is not correct")}

  if(x3>x4){
    smallern<-x4
  } else {
    smallern<-x3
  }
  for(i in 1:smallern){
    if((x3%%i==0)&&(x4%%i==0)){
      euclid=i
    }}
  return(euclid)
}

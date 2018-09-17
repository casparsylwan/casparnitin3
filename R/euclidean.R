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
  if(is.scalar(x1)==FALSE || is.scalar(x2)==FALSE){
    stop("The input is not correct")}

  if(x1>x2){
    smallern<-x2
  } else {
    smallern<-x1
  }
  for(i in 1:smallern){
    if((x1%%i==0)&&(x2%%i==0)){
      euclid=i
    }}
  return(euclid)
}

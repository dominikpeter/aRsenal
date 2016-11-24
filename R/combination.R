
#' Combinations
#'
#' returns number of combinations
#'
#' @param k vector or NULL
#'
#' @return none
#'
#' @examples
#' perm(4, 2)
#'
#'
#' @export
perm <- function(n, k){
  factorial(n)/factorial(n-k)

}


#' Description
#'
#' match returns a vector of non matches
#'
#' @param x vector or NULL: the values to be matched. Long vectors are supported.
#' @param table vector or NULL: the values to be matched against. Long vectors are not supported.
#'
#' @return None
#'
#' @examples
#' "Hans" %notin% c("Fritz","Rolf", "Reto")
#'
#' @export
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

require(data.table)

#' nest
#'
#' nest
#'
#' @param x stat
#'
#' @return nested data.table
#'
#' @examples
#'
#'
#'
#' @export
nest <- function(x, ...) UseMethod("nest")


#' nest
#'
#' nest
#'
#' @param x stat
#'
#' @return nested data.table
#'
#' @examples
#'
#'
#'
#' @export
nest.data.table <- function(x, .group) {
  .group = substitute(.group)
  x[, list(data = list(.SD)), keyby = .group]
}


#' nest
#'
#' nest
#'
#' @param x stat
#'
#' @return nested data.table
#'
#' @examples
#'
#'
#'
#' @export
nest.data.frame <- function(x, ...) {
  tidyr::nest(x, ...)

}


#' nest
#'
#' nest
#'
#' @param x stat
#'
#' @return nested data.table
#'
#' @examples
#'
#'
#'
#' @export
unnest.data.frame <- function(x, ...) {
  tidyr::unnest(x, ...)

}






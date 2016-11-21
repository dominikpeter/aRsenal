
#' Difference Range
#'
#' returns the difference between min and max value
#'
#' @param x vector or NULL
#'
#' @return none
#'
#' @examples
#' x <- rnorm(100)
#' diff.range(x)
#'
#' @export
diff.range <- function(x) diff(range(x))


#' Variance with option for population variance
#'
#' Variance with option of calculation the population variance
#'
#' @param x vector or NULL
#'
#' @return none
#'
#' @examples
#' x <- rnorm(100)
#' var2(x, sample = FALSE)
#'
#' @export
var2 <- function(x, sample = TRUE){
  if (sample)
    return(var(x))

  var(x)*(length(x)-1)/length(x)
}



#' Standard Deviation with option for population variance
#'
#' Standard Deviation with option of calculation the population variance
#'
#' @param x vector or NULL
#'
#' @return none
#'
#' @examples
#' x <- rnorm(100)
#' sd2(x, sample = FALSE)
#'
#' @export
sd2 <- function(x, sample = TRUE){
  if (sample)
    return(sd(x))

  sqrt(var(x)*(length(x)-1)/length(x))
}


#' get outlier
#'
#' get outlier
#'
#' @param x numeric vector
#' @param y numeric vector or NULL
#'
#' @return Numeric Index
#'
#' @examples
#'get_outlier(mtcars$mpg, mtcars$hp)
#'mtcars[get_outlier(mtcars$mpg, mtcars$hp), ]
#'
#' @export
get_outlier <- function(x, y = NULL) {
  stopifnot(is.vector(x) && (is.vector(y) || is.null(y)))

  X <- cbind(x, y)

  idx <- apply(X, MARGIN = 2, get_idx_outlier)

  if (is.list(idx))
    idx <- unlist(idx)

  unique(idx)
}



get_idx_outlier <- function(x) {
  IQR <- 1.5*IQR(x)
  upper <- quantile(x, .75, names = FALSE) + IQR
  lower <- quantile(x, .25, names = FALSE) - IQR
  which(x > upper | x < lower)
}



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
  if (missing(x) ||
      !is.numeric(x) ||
      (!is.numeric(y) && !is.null(y)))
    stop("You must provide a x value and it has to be numeric")

  X <- as.data.frame(cbind(x = x, y = y))

  if (ncol(X) == 1) {
    # Use IQR
    IQR <- 1.5*IQR(x)
    upper <- quantile(x, .75, names = FALSE) + IQR
    lower <- quantile(x, .25, names = FALSE) - IQR
    idx <- which(x > upper | x < lower)
  } else {
    mod <- lm(X$y ~ X$x, data = X)
    cooksd <- unname(cooks.distance(mod))
    upper <- 4*mean(cooksd, na.rm=TRUE)
    idx <- which(cooksd > upper)
  }
  idx
}



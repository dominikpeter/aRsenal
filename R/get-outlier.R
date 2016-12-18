
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
get_outlier <- function(x, y = NULL, formula = c("auto", "lm", "loess")) {
  if (missing(x) ||
      !is.numeric(x) ||
      (!is.numeric(y) && !is.null(y)))
    stop("You must provide a x value and it has to be numeric")

  formula <- match.arg(formula)

  X <- as.data.frame(cbind(x = x, y = y))

  if (ncol(X) == 1) {
    if (formula != "auto") {
      cat("No y Variable was provided: Using IQR*1.5 on univariat Vector")
    }
    # Use IQR
    idx <- IQR_outliter(X$x, times = 1.5)
  } else {

    call_model <- switch(formula,
                         lm = lm(y ~ x, data = X),
                         loess = loess(y ~ x, data = X),
                         gam = mgcv::gam(y ~ x, data = X)
    )

    mod <- call_model(X$y ~ X$x, data = X)
    res <- mod$residuals
    idx <- IQR_outlier(res, times = 1.5)
  }
  idx
}


IQR_outlier <- function(x, times = 1.5) {
  IQR <- times*IQR(x)
  upper <- quantile(x, .75, names = FALSE) + IQR
  lower <- quantile(x, .25, names = FALSE) - IQR
  idx <- which(x > upper | x < lower)
}


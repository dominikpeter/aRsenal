
#' calculate sample size
#'
#'
#'
#' @param x vector
#' @param Margin.of.Error numeric
#' @param conf.level
#'
#' @return numeric
#'
#' @examples
#'
#'
#'
#' @export
sample_size <- function(x, Margin.of.Error, conf.level = 0.95) {
  conf <- 1-((1-conf.level)/2)
  zstar <- qnorm(conf)

  zstar^2 * sigma^2 / Margin.of.Error^2

}

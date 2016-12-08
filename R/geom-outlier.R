require(ggplot2)
#' get outlir
#'
#' get outlier
#'
#' @param x vector or NULL
#'
#' @return none
#'
#' @examples
#'
#'
#'
#' @export
get_outlier <- function(x) {
  stopifnot(is.vector(x))

  upper <- quantile(x, .75, names = FALSE) + (IQR(x)*1.5)
  lower <- quantile(x, .75, names = FALSE) - (IQR(x)*1.5)
  which(x > upper | x < lower)
}

#' get outlir
#'
#' get outlier
#'
#' @param x vector or NULL
#'
#' @return none
#'
#' @examples
#'
#'
#'
#' @export
StatOutlier <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[get_outlier(data$y), , drop = FALSE]
                     },

                     required_aes = c("y")
)


#' get outlir
#'
#' get outlier
#'
#' @param x vector or NULL
#'
#' @return none
#'
#' @examples
#'
#'
#'
#' @export
stat_outlier <- function(mapping = NULL, data = NULL, geom = "text",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatOutlier, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}






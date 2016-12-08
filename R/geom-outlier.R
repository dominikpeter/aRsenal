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
get_outlier <- function(x, y = NULL) {
  stopifnot(is.vector(x) && (is.vector(y) || is.null(y)))

  get_idx_outlier <- function(x) {
    coef <- IQR(x, na.rm = TRUE)*1.5
    upper <- quantile(x, .75, names = FALSE) + coef
    lower <- quantile(x, .25, names = FALSE) - coef
    which(x > upper | x < lower)
  }
  X <- cbind(x, y)

  idx <- unlist(apply(X, MARGIN = 2, get_idx_outlier))
  unique(idx)
}


?apply
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
StatOutlier <- ggproto("StatOutlier", Stat,
                     compute_group = function(data, scales) {
                       data[get_outlier(data$x, data$y), , drop = FALSE]
                     },

                     required_aes = c("x", "y")
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
stat_outlier <- function(mapping = NULL, data = NULL, geom = "point",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatOutlier, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
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
geom_name_outlier <- function(mapping = NULL, data = NULL,
                       position = position_stack(1.08), na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatOutlier, geom = GeomText, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
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
geom_mark_outlier <- function(mapping = NULL, data = NULL,
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  layer(
    stat = StatOutlier, geom = GeomPoint, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}




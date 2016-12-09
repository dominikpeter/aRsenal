require(ggplot2)



StatOutlier <- ggproto("StatOutlier", Stat,
                     compute_group = function(data, scales) {
                       data[get_outlier(data$x, data$y), , drop = FALSE]
                     },

                     required_aes = c("x", "y")
)


#' stat outlier
#'
#' stat outlier
#'
#' @param x stat
#'
#' @return ggplot stat
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

#' name outlier
#'
#' name outlier in ggplot plot
#'
#' @param x aes
#'
#' @return ggplot geom
#'
#' @examples
#' mtcars %>%
#' ggplot(aes(x = factor(cyl), y = hp, label = rownames(.))) +
#'  geom_boxplot() +
#'  geom_name_outlier(size = 4)
#'
#'
#' @export
geom_name_outlier <- function(mapping = NULL, data = NULL,
                       position = position_stack(1.07), na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatOutlier, geom = GeomText, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' mark outlier
#'
#' mark outlier in ggplot plot
#'
#' @param x aes
#'
#' @return ggplot geom
#'
#' @examples
#'mtcars %>%
#'  ggplot(aes(x = mpg, y = hp)) +
#'  geom_point() +
#'  geom_smooth(method = "lm", se = F, color = "black", linetype = "dashed") +
#'  geom_mark_outlier(shape = 1, size = 4, color = "red")
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




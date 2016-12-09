
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

#' geom name outlier
#'
#' name outlier in ggplot plot
#'
#' @param x aes
#'
#' @return ggplot geom
#'
#' @examples
#'mtcars %>%
#'  ggplot(aes(x = qsec, y = hp)) +
#'  geom_point(color = "#112233", size = 3) +
#'  geom_smooth(method = "lm", se = F, color = "#112233", linetype = "dotted") +
#'  geom_name_outlier(aes(label = rownames(.)), vjust = -1.2, size = 3) +
#'  xlim(14, 23) +
#'  ylim(0, 340) +
#'  theme(panel.background = element_rect(color = "#F0F1F5"))
#'
#'mtcars %>%
#'ggplot(aes(x = qsec, y = hp)) +
#'  geom_point(color = "#112233", size = 3) +
#'  geom_smooth(method = "lm", se = F, color = "#112233", linetype = 3) +
#'  geom_mark_outlier(size = 4, alpha = 0.6, color = "#FC575E") +
#'  geom_name_outlier(aes(label = rownames(.)), vjust = -1.2, size = 3) +
#'  xlim(14, 23) +
#'  ylim(0, 340) +
#'  theme(panel.background = element_rect(color = "#F0F1F5"))
#'
#'
#' @export
geom_name_outlier <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatOutlier, geom = GeomText, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' geom mark outlier
#'
#' mark outlier in ggplot plot
#'
#' @param x aes
#'
#' @return ggplot geom
#'
#' @examples
#'mtcars %>%
#'  ggplot(aes(x = qsec, y = hp)) +
#'  geom_point(color = "#112233", size = 3) +
#'  geom_smooth(method = "lm", se = F, color = "#112233", linetype = "dotted") +
#'  geom_mark_outlier(size = 4, alpha = 0.5, color = "#FC575E") +
#'  xlim(14, 23) +
#'  ylim(0, 340) +
#'  theme(panel.background = element_rect(color = "#F0F1F5"))
#'
#'#'mtcars %>%
#'ggplot(aes(x = qsec, y = hp)) +
#'  geom_point(color = "#112233", size = 3) +
#'  geom_smooth(method = "lm", se = F, color = "#112233", linetype = 3) +
#'  geom_mark_outlier(size = 4, alpha = 0.6, color = "#FC575E") +
#'  geom_name_outlier(aes(label = rownames(.)), vjust = -1.2, size = 3) +
#'  xlim(14, 23) +
#'  ylim(0, 340) +
#'  theme(panel.background = element_rect(color = "#F0F1F5"))
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




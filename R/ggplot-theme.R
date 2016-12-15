

#' theme flat
#'
#'
#' @return Numeric Index
#'
#' @examples
#'get_outlier(mtcars$mpg, mtcars$hp)
#'mtcars[get_outlier(mtcars$mpg, mtcars$hp), ]
#'
#' @export
theme_flat <- theme_grey() %+replace% theme(panel.background = element_rect(color = "#F0F1F5"))


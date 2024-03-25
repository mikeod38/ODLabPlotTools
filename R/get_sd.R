#' get_sd
#'
#' @export
#' @examples ggplot() + stat_summary(geom = "errobar", fun.data = get_sd)


get_sd <- function(x) {
  sd_value <- sd(x, na.rm = TRUE)
  return(c(y = mean(x) + sd_value, ymin = mean(x) - sd_value, ymax = mean(x) + sd_value))
}

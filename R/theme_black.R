#' theme_black
#'
#' @export
#' @examples ggplot() + theme_black()

theme_black = function(base_size = 12, base_family = "") {

  # Update geom defaults to white
  update_geom_defaults("point", list(colour = "white"))
  update_geom_defaults("line", list(colour = "white"))
  update_geom_defaults("errorbar", list(colour = "white"))

  theme_grey(base_size = base_size, base_family = base_family) %+replace%

    theme(
      # Specify axis options
      axis.line.x = element_line(color = "white", size  =  0.2),
      axis.line.y = element_line(color = "white", size  =  0.2),
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.ticks = element_line(color = "white", size  =  0.2),
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),
      legend.key = element_rect(color = "white",  fill = "black"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size*0.8, color = "white"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      #panel.border = element_blank(),
      #panel.background = element_blank(),
      panel.background = element_rect(fill = "black", color  =  NA),
      #panel.border = element_rect(fill = NA, color = "white"),
      panel.border = element_rect(colour = "black", fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      # Specify facetting options
      #strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.background = element_blank(),
      strip.text.x = element_text(size = base_size*0.8, color = "white"),
      strip.text.y = element_text(size = base_size*0.8, color = "white", angle = -90),
      #Specify plot options
      plot.background = element_rect(color = "black", fill= "black", size =5),
      plot.title = element_text(size = base_size*1.2, color = "white")
      #plot.spacing = unit(rep(1, 4), "lines")
    )

  # Return to original defaults after theme is applied
  # on.exit({
  #   update_geom_defaults("point", list(colour = "black"))
  #   update_geom_defaults("line", list(colour = "black"))
  #   update_geom_defaults("errorbar", list(colour = "black"))
  # })
}

#' A [ggplot2] theme for Whit
#'
#' Provides a [ggplot2] custom formatted theme.
#'
#' Note: adapted from [urbnthemes]
#'
#' @import extrafont
#' @import ggrepel
#' @md
#' @param scale "continuous" creates a vertical legend to the right of the map. "discrete" creates a horizontal legend above the map.
#' @param base_family,base_size base font family and size
#' @param base_line_size,base_rect_size base line and rectangle sizes
#' @export

theme_map <- function(
  scale = "continuous",
  base_size = 8.5,
  base_family = "Sans",
  base_line_size = 0.5,
  base_rect_size = 0.5
) {
  gg <- theme_print(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  )

  gg <- gg +
    ggplot2::theme(
      line = ggplot2::element_line(
        colour = "black",
        size = base_line_size,
        linetype = 1L,
        lineend = "butt"
      ),
      rect = ggplot2::element_rect(
        fill = "white",
        colour = "black",
        size = base_rect_size,
        linetype = 1L
      ),
      text = ggplot2::element_text(
        family = base_family,
        face = "plain",
        colour = "black",
        size = 16,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.9,
        margin = ggplot2::margin(),
        debug = FALSE
      ),

      # Plot Attributes

      plot.tag = ggplot2::element_text(
        size = base_size * 1.5,
        hjust = 0L,
        vjust = 0L,
        face = "bold",
        margin = ggplot2::margin(b = 10L)
      ),
      plot.tag.position = "topleft",
      plot.title = ggplot2::element_text(
        size = base_size * 20 / 8.5,
        hjust = 0L,
        vjust = 0L,
        face = "bold",
        margin = ggplot2::margin(b = 10L)
      ),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(
        size = 18,
        hjust = 0,
        face = "bold",
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggtext::element_textbox(
        size = 17,
        colour = "gray10",
        width = unit(.999, "npc"),
        hjust = -.4,
        halign = 0,
        lineheight = 1.3
      ),
      plot.caption.position = "plot",
      plot.background = NULL,

      plot.margin = ggplot2::margin(20, 40, 20, 20),

      # axis attributes

      axis.text = ggplot2::element_text(
        size = 16,
        margin = ggplot2::margin(r = 10)
      ),
      axis.text.x = ggplot2::element_text(
        vjust = 1,
        margin = ggplot2::margin(t = 4L),
        size = 16
      ),
      axis.text.y = ggplot2::element_text(hjust = 1),
      axis.text.x.top = NULL,
      axis.text.y.right = NULL,
      axis.ticks.length.x = NULL,
      axis.ticks.length.x.top = NULL,
      axis.ticks.length.x.bottom = NULL,
      axis.ticks.length.y = NULL,
      axis.ticks.length.y.left = NULL,
      axis.ticks.length.y.right = NULL,

      axis.title = ggplot2::element_text(
        size = base_size * 18 / base_size,
        face = "bold"
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8L)),
      axis.title.y = ggplot2::element_text(
        angle = 90L,
        margin = ggplot2::margin(r = 4L)
      ),
      axis.title.x.top = NULL,
      axis.title.y.right = NULL,

      axis.ticks = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_line(colour = "#BDBDBD"),

      axis.line = ggplot2::element_line(),
      axis.line.x = ggplot2::element_line(
        size = 0.5,
        linetype = "solid",
        colour = "#BDBDBD"
      ),
      axis.line.y = ggplot2::element_line(
        size = 0.5,
        linetype = "solid",
        colour = "#BDBDBD"
      ),

      # legend attributes

      legend.background = ggplot2::element_blank(),

      legend.spacing = ggplot2::unit(20L, "pt"),
      legend.spacing.x = ggplot2::unit(4L, "pt"),
      legend.spacing.y = NULL,

      legend.key = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(10L, "pt"),
      legend.key.height = NULL,
      legend.key.width = NULL,

      legend.text = ggplot2::element_text(size = base_size * 1.5, vjust = 0.5),
      legend.text.align = NULL,
      legend.title = ggplot2::element_blank(),
      legend.title.align = NULL,

      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.margin = ggplot2::margin(t = 6L, r = 0L, b = 6L, l = 0L, "pt"),

      legend.box = "horizontal",
      legend.box.margin = NULL,
      legend.box.background = NULL,
      legend.box.spacing = NULL,

      # panel attributes

      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.ontop = FALSE,

      panel.spacing = ggplot2::unit(6L, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,

      panel.grid = NULL,
      panel.grid.major = ggplot2::element_line(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#dedddd"),
      panel.grid.minor = ggplot2::element_line(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),

      # strip attributes (Faceting)

      strip.background = ggplot2::element_rect(
        fill = "#dedddd",
        colour = NA,
        size = 10L
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = base_size * 9.5 / 8.5,
        margin = ggplot2::margin(t = 0L, r = 0L, b = 0L, l = 0L)
      ),

      strip.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 4.5, b = 4.5)
      ),
      strip.text.y = ggplot2::element_text(
        angle = -90L,
        margin = ggplot2::margin(l = 4.5, r = 4.5)
      ),

      strip.placement = "inside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,

      strip.switch.pad.grid = ggplot2::unit(0.1, "cm"),
      strip.switch.pad.wrap = ggplot2::unit(0.1, "cm"),

      # create a complete format
      complete = TRUE
    )

  if (scale == "continuous") {
    gg <- gg +
      ggplot2::theme(
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = ggplot2::element_text(size = base_size)
      )
  }
  # return gg
  gg
}

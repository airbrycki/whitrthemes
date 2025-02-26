#' whit_title
#'
#' Add a title to a plot created with \code{whit_plot()}.
#'
#' @param string character string for a title
#' @param size font size for the title
#' @param width a number of characters to allow before a character return
#'
#' @return a grob formatted for a source in a ggplot
#'
#' @export
#'
whit_title <- function(string, size = 17.5, width = 75) {

  if (width == FALSE) {
    grid::textGrob(string,
             x = unit(0.01, "npc"),
             y = unit(1, "npc"),
             hjust = 0,
             vjust = 1.3,
             gp = grid::gpar(fontsize = size,
                       fontfamily = "Sans",
                       lineheight = 1,
                       fontface = "bold"))
  } else {
    grid::textGrob(stringr::str_wrap(string, width = width),
             x = unit(0.01, "npc"),
             y = unit(1, "npc"),
             hjust = 0,
             vjust = 1.3,
             gp = grid::gpar(fontsize = size,
                       fontfamily = "Sans",
                       lineheight = 1,
                       fontface = "bold"))
  }
}

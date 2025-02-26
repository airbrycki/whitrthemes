#' whit_y_title
#'
#' Add a horizontal y axis title to a plot created with \code{urbn_plot()}.
#'
#' @param string character string for a y-axis title
#' @param size font size for the y-axis title
#'
#' @return a grob formatted for a y-axis title in a ggplot
#'
#' @export
#'
whit_y_title <- function(string, size = 16) {

  grid::textGrob(string,
                 x = unit(0.01, "npc"),
                 y = unit(1, "npc"),
                 hjust = -0.03,
                 vjust = 1.8,
                 gp = grid::gpar(fontsize = size,
                           fontfamily = "Sans"))
}

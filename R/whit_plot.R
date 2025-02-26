#' urbn_plot
#'
#' Combine elements from \code{whit_title},
#' \code{whit_y_title}, \code{get_legend}, \code{remove_legend},
#' \code{whit_notes}, \code{whit_source}, and \code{ggplot2} into
#' one formatted plot.
#'
#' @param ... urbn plot objects or grobs
#' @param heights relative heights of each object in the final plot
#'
#' @return one plot made from many grobs
#'
#' @export
#'
whit_plot <- function(..., heights = 1) {
  gridExtra::grid.arrange(...,
                          heights = heights)
}

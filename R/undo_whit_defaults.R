#' Whit's [ggplot2] theme
#'
#' Resets all the [ggplot2] theme to its default settings
#'
#'
#' @import extrafont
#' @md
#' @export

undo_whit_defaults <- function() {

  # set default theme to theme_grey() ----------------------------------

  ggplot2::theme_set(ggplot2::theme_grey())

  # add Helvetica font to text and label geoms ---------------------------

  ggplot2::update_geom_defaults("text", list(family = "Helvetica"))
  ggplot2::update_geom_defaults("label", list(family = "Helvetica"))

  # set default colours for monochromatic geoms -----------------------------

  ggplot2::update_geom_defaults("bar", list(fill = "#595959"))
  ggplot2::update_geom_defaults("col", list(fill = "#595959"))
  ggplot2::update_geom_defaults("point", list(colour = "black"))
  ggplot2::update_geom_defaults("line", list(colour = "#595959"))
  ggplot2::update_geom_defaults("boxplot", list(fill = "#595959"))
  ggplot2::update_geom_defaults("density", list(fill = "#595959"))
  ggplot2::update_geom_defaults("violin", list(fill = "#595959"))

  # set default colours for monochromatic stats -----------------------------

  ggplot2::update_stat_defaults("count", list(fill = "#595959"))
  ggplot2::update_stat_defaults("boxplot", list(fill = "#595959"))
  ggplot2::update_stat_defaults("density", list(fill = "#595959"))
  ggplot2::update_stat_defaults("ydensity", list(fill = "#595959"))

}

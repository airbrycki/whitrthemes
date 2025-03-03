#' Save ggplot2 plots in standard sizes
#'
#' This is a function to save standardized images.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param size Size of plot. Must be one of "small" (3.25 x 2 inches),
#' "medium" (6.5 x 4 inches), or "large" (9 x 6.5 inches).
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320),
#' "print" (300), or "screen" (72). Applies only to raster output types.
#'
#' @export
whit_save <- function(filename,
                      plot = ggplot2::last_plot(),
                      size = "medium",
                      dpi = 300,
                      height = NULL) {

  stopifnot(is.character(filename))

  if (!size %in% c("small", "medium", "large", "xlarge")) {

    stop("Error: size must be one of 'small', 'medium', 'large', or 'xlarge'")

  }

  if (is.null(height)){
    sizes <- list(
      small = c(width = 3.12, height = 2),
      medium = c(width = 6.25, height = 4),
      large = c(width = 8.75, height = 6.5),
      xlarge = c(width = 12.25, height = 6.5)
    )
  } else {
    sizes <- list(
      small = c(width = 3.12, height = height),
      medium = c(width = 6.25, height = height),
      large = c(width = 8.75, height = height),
      xlarge = c(width = 12.25, height = height)
    )
  }

  selected_size <- sizes[[size]]

  ggsave(
    filename = filename,
    plot = plot,
    width = selected_size["width"],
    height = selected_size["height"]
  )

}



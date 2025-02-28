#' whit_source
#'
#' Add a sources section to the bottom of a plot created with \code{whit_plot()}.
#'
#' @param text character string for a source
#' @param size font size for the source
#' @param width a number of characters to allow before a character return
#' @param plural If `TRUE`, will change "Sources:" to "Sources:"
#'
#' @return a grob formatted for a source in a ggplot
#'
#' @export
#'
whit_source <- function(text, size = 12, width = 120, plural = FALSE) {

  # should "Sources:" be "Sources:" in the source
  if (!plural) {

    section_title <- "Source: "

  } else {

    section_title <- "Sources: "

  }

  # get the shorter first line
  wrapped_lines <- stringr::str_wrap(paste0(section_title, text), width = width)

  line1 <- paste0(stringr::str_split(wrapped_lines, "\n", n = 2)[[1]][1], "\n")

  multiline <- length(stringr::str_split(wrapped_lines, "\n", n = 2)[[1]]) > 1

  if (multiline) {

    lines <- stringr::str_replace_all(stringr::str_split(wrapped_lines,
                                                         "\n", n = 2)[[1]][2], "\n", " ")

    lines <- stringr::str_wrap(lines, width = width)

  }

  grob1 <- grid::textGrob(
    line1,
    name = "source1",
    x = unit(0.02, "npc"),
    y = unit(1, "npc"),
    hjust = 0,
    vjust = 1.2,
    gp = grid::gpar(fontsize = size, fontfamily = "Arial", lineheight = 1)
  )


  if (multiline) {

    grob2 <- grid::textGrob(
      lines,
      x = unit(0.02, "npc"),
      y = unit(1, "npc") - .75 * grid::grobHeight("source1"),
      hjust = 0,
      vjust = 1,
      gp = grid::gpar(fontsize = size, fontfamily = "Arial", lineheight = 1)
    )

    grid::grobTree(grob1, grob2)

  } else {

    grid::grobTree(grob1)

  }
}

#' whit_note
#'
#' Add a notes section to the bottom of a plot created with \code{whit_plot()}.
#'
#' @param text character string for a note
#' @param size font size for the note
#' @param width a number of characters to allow before a character return
#' @param plural If `TRUE`, will change "Note:" to "Notes:"
#'
#' @return a grob formatted for a source in a ggplot
#'
#' @export
#'
whit_note <- function(text, size = 12, width = 120, plural = FALSE) {

  # should "Note:" be "Notes:" in the note
  if (!plural) {

    section_title <- "Note: "

  } else {

    section_title <- "Notes: "

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
    name = "note1",
    x = unit(0.02, "npc"),
    y = unit(1, "npc"),
    hjust = 0,
    vjust = 1.4,
    gp = grid::gpar(fontsize = size, fontfamily = "Sans", lineheight = 1)
  )


  if (multiline) {

    grob2 <- grid::textGrob(
      lines,
      x = unit(0.02, "npc"),
      y = unit(1, "npc") - 0.92 * grid::grobHeight("note1"),
      hjust = 0,
      vjust = 1,
      gp = grid::gpar(fontsize = size, fontfamily = "Sans", lineheight = 1)
    )

    grid::grobTree(grob1, grob2)

  } else {

    grid::grobTree(grob1)

  }
}

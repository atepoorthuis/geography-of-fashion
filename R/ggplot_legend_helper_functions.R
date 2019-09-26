#' Annotate a ggplot legend with two labels on either side
#'
#' Adapted from: https://stackoverflow.com/a/37235413
#'
#' @param plot A ggplot object
#' @param label1 The text for the first (left) label
#' @param label2 The text for the second (right) label
#'
#' @return A ggplot object with the modified legend
#' @export
annotate_legend <- function(plot, label1, label2) {
  g <- ggplotGrob(plot)
  leg <- g$grobs[[which(g$layout$name == "guide-box")]]

  # Construct the label grob
  text1 <- textGrob(
    x = 0.975,
    label1,
    gp = gpar(fontsize = 9, fontfamily = "Helvetica Neue Thin Italic"),
    hjust = 1
  )
  text2 <- textGrob(
    x = 0.025,
    label2,
    gp = gpar(fontsize = 9, fontfamily = "Helvetica Neue Thin Italic"),
    hjust = 0
  )
  height <- unit(1, "grobheight", text1) + unit(5, "points")
  labelGrob <- gTree("labelGrob", children = gList(text1, text2))

  # Add the label grob to a new row added to the legend
  pos <- subset(leg$layout, grepl("guides", name), t:r)
  leg <- gtable_add_rows(leg, height, pos = pos$t + 1)
  leg <- gtable_add_grob(leg, labelGrob, t = pos$t + 2, l = pos$l)

  # Return the modified legend to the original plot
  g$grobs[[which(g$layout$name == "guide-box")]] <- leg
  return(g)
}


#' Create a shared leged (with annotations) for multiple plots
#'
#' Adapted from: https://stackoverflow.com/a/38420690 and https://stackoverflow.com/a/37235413
#'
#' @param ... two or more ggplot objects
#' @param ncol number of columns
#' @param nrow number of rows
#' @param position position of the legend
#'
#' @return A ggplot object with the combined plots
#' @export
#'
grid_arrange_shared_legend <- function(...,
                                       ncol = 1,
                                       nrow = length(list(...)),
                                       position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

  text1 <- textGrob(
    x = 0.975,
    "more attention",
    gp = gpar(fontsize = 9, fontfamily = "Helvetica Neue Thin Italic"),
    hjust = 1
  )
  text2 <- textGrob(
    x = 0.025,
    "less attention",
    gp = gpar(fontsize = 9, fontfamily = "Helvetica Neue Thin Italic"),
    hjust = 0
  )

  height <- unit(1, "grobheight", text1) + unit(5, "points")
  labelGrob = gTree("labelGrob", children = gList(text1, text2))

  # Add the label grob to a new row added to the legend
  pos <- subset(legend$layout, grepl("guides", name), t:r)
  legend <- gtable_add_rows(legend, height, pos = pos$t + 1)
  legend <- gtable_add_grob(legend, labelGrob, t = pos$t + 2, l = pos$l)
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x)
    x + theme(legend.position = "none")) # t, r, b, l
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(
    position,
    "bottom" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)
    ),
    "right" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 2,
      widths = unit.c(unit(1, "npc") - lwidth, lwidth)
    )
  )

  # return gtable invisibly (but I don't remember why..)
  invisible(combined)
  return(combined)
}

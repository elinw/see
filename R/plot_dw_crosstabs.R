plot.datawizard_crosstab <- function(x) {
  proportion_type <- attr(x, "proportions")
  if (is.null(proportion_type)) {
    x_long <- datawizard::data_to_long(
      x,
      rows_to = "row_var",
      select = names(t_base)[-1]
    )
  } else {
    x_long <- attr(x, "prop_table") |>
      datawizard::data_to_long(rows_to = "row_var")
  }
  p <- ggplot(x_long)
  plotlist <- list()
  if (is.null(proportion_type)) {
    x_long
    plotlist[[1]] <- aes(x = row_var, y = name, fill = value)
    plotlist[[2]] <- geom_tile()
    plotlist[[3]] <- scale_fill_gradient(low = "yellow", high = "green")
  } else if (proportion_type == "row") {
    plotlist[[1]] <- aes(x = row_var, y = value, fill = name)
    plotlist[[2]] <- geom_col()
    plotlist[[3]] <- coord_flip()
  } else if (proportion_type == "column") {
    plotlist[[1]] <- aes(x = name, y = value, fill = row_var)
    plotlist[[2]] <- geom_col()
  } else if (proportion_type == "full") {
    plotlist[[1]] <- aes(x = row_var, y = name, fill = value)
    plotlist[[2]] <- geom_tile()
    plotlist[[3]] <- scale_fill_gradient(low = "yellow", high = "green")
  }

  p <- p + plotlist
  p
}

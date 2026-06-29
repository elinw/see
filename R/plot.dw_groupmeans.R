#' Plot method for grouped means
#'
#' The `plot()` method for the `datawizard::means_by_group()` function
#'
#' @param x  An object returned `datawizard::means_by_group.data()`.
#' @param y  Not currently used
#' @param ... Additional options. Acceptable values include `title`
#' with a string to use as a title, `ci` for if confidence intervals
#' (if present in the object should be graphed), and `caption` for if
#' a caption summarizing ANOVA results should be included.
#'
#' @value
#' Produces a faceted plot when there is more than one means-table in the
#' list. If there is a single item a standard plot
#' is returned.
#'
#' @examples
#' group_means_object <-  datawizard::means_by_group(iris$Sepal.Width, iris$Species)
#' plot(group_means_object, title = "group means", ci = FALSE, caption = FALSE)
#'
#' group_means_object <- datawizard::means_by_group(
#'   iris,
#'   c("Sepal.Width", "Petal.Width"),
#'   "Species"
#' )
#'
#' plot(group_means_object, title = "group means")
#'
#' group_means_object <- datawizard::means_by_group(
#'   iris$Sepal.Width, iris$Species)
#' )
#' plot(group_means_object, title = "group means")
#'
#' @export
plot.dw_groupmeans <- function(
  x,
  ...
) {
  .data <- NULL

  dotargs <- .unpack_dots(x, list(...))

  title <- dotargs[["title"]]
  ci <- dotargs[["ci"]]
  caption <- dotargs[["caption"]]
  if (isTRUE(caption)) {
    caption_text <- .build_caption(x)
  } else {
    caption_text = ""
  }
  trimmed <- datawizard::data_filter(x, Category != "Total")

  if ("CI_low" %in% names(trimmed)) {
    lower_lim <- .9 * min(trimmed$CI_low)
    upper_lim <- 1.1 * max(trimmed$CI_high)
  }

  p <- ggplot2::ggplot(trimmed)
  plotlist <- list()
  plotlist[[1]] <- ggplot2::aes(x = .data$Category, y = .data$Mean)

  plotlist[[2]] <- ggplot2::geom_point()
  plotlist[[3]] <- ggplot2::labs(title = title, caption = caption_text)

  # There is an option not to return ci in data_group_means()
  if ("CI_low" %in% names(x) && isTRUE(ci)) {
    plotlist[[4]] <- ggplot2::geom_linerange(ggplot2::aes(
      ymin = .data$CI_low,
      ymax = .data$CI_high
    ))

    plotlist[[5]] <- ggplot2::scale_y_continuous(
      limits = c(lower_lim, upper_lim)
    )
  }

  p + plotlist
}


#' @export
plot.dw_groupmeans_list <- function(
  x,
  y,
  ...
) {
  if (length(x) == 0L || !length(x)) {
    insight::format_error("x is an empty object")
  }

  dotargs <- .unpack_dots(x, list(...))

  title <- dotargs[["title"]]
  ci <- dotargs[["ci"]]
  caption <- dotargs[["caption"]]

  if (length(x) == 1L) {
    p <- plot(
      x[[1]],
      title = title,
      ci = ci,
      caption = caption,
      ...
    )
    return(p)
  }

  x_attributes <- lapply(x, attributes)
  x_var_names <- lapply(x_attributes, `[[`, "var_mean_label")
  x_captions <- mapply(.build_caption, x, x_var_names)
  x_caption <- paste(x_captions, collapse = "")
  names(x) <- x_var_names

  x <- lapply(seq_along(x_var_names), function(i) {
    x[[i]]$origin_df <- x_var_names[[i]]
    x[[i]]
  })

  x_long <- do.call(rbind, x)

  trimmed <- datawizard::data_filter(x_long, Category != "Total")

  if ("CI_low" %in% names(trimmed)) {
    lower_lim <- .9 * min(trimmed$CI_low)
    upper_lim <- 1.1 * max(trimmed$CI_high)
  }

  p <- ggplot2::ggplot(trimmed)
  plotlist <- list()
  plotlist[[1]] <- ggplot2::aes(x = .data$Category, y = .data$Mean)

  plotlist[[2]] <- ggplot2::geom_point()
  plotlist[[3]] <- ggplot2::facet_wrap(~origin_df)

  if (isTRUE(caption)) {
    plotlist[[4]] <- ggplot2::labs(title = title, caption = x_caption)
  } else {
    plotlist[[4]] <- ggplot2::labs(title = title)
  }
  if ("CI_low" %in% names(trimmed) & isTRUE(ci)) {
    plotlist[[5]] <- ggplot2::geom_linerange(ggplot2::aes(
      ymin = .data$CI_low,
      ymax = .data$CI_high
    ))
    plotlist[[6]] <- ggplot2::scale_y_continuous(
      limits = c(lower_lim, upper_lim)
    )
  }

  p <- p + plotlist
  print(p)
}


.build_caption <- function(x, label = NULL) {
  caption <- paste0(
    "\n",
    ifelse(is.null(label), "", paste0(label, ": ")),
    "Anova: R2=",
    insight::format_value(attributes(x)$r2, digits = 3),
    "; adj.R2=",
    insight::format_value(attributes(x)$adj.r2, digits = 3),
    "; F=",
    insight::format_value(attributes(x)$fstat, digits = 3),
    "; ",
    insight::format_p(attributes(x)$p.value, whitespace = FALSE)
  )

  caption
}

.unpack_dots <- function(x, dotargs) {
  #dotargs <- list(...)
  if (length(dotargs) == 0L) {
    dotargs <- list(title = "", caption = TRUE, ci = TRUE)
    dotargsnames <- names(dotargs)
  } else {
    dotargsnames <- names(dotargs)
    if (!"title" %in% dotargsnames) {
      dotargs[["title"]] <- ""
    }
    if (!"caption" %in% dotargsnames) {
      dotargs[["caption"]] <- TRUE
    }
    if (!"ci" %in% dotargsnames) {
      dotargs[["ci"]] <- TRUE
    }
  }

  dotargs
}

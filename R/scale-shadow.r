
ScaleDiscreteIdentity <- getFromNamespace("ScaleDiscreteIdentity", "ggplot2")

binned_scale <- getFromNamespace("binned_scale", "ggplot2")
discrete_scale <- getFromNamespace("discrete_scale", "ggplot2")
datetime_scale <- getFromNamespace("datetime_scale", "ggplot2")
manual_scale <- getFromNamespace("manual_scale", "ggplot2")
mid_rescaler <- getFromNamespace("mid_rescaler", "ggplot2")
continuous_scale <- getFromNamespace("continuous_scale", "ggplot2")
waiver <- getFromNamespace("waiver", "ggplot2")
muted <- getFromNamespace("muted", "scales")
binned_pal <- getFromNamespace("binned_pal", "ggplot2")

#' @rdname scale_colour_hue
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=as.factor(gear)))
#' p + geom_shadowpoint() + scale_shadowcolour_hue()
#'
#' @export
scale_shadowcolour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                        direction = 1, na.value = "grey50", aesthetics = "shadowcolour") {
  discrete_scale(aesthetics, "hue", scales::hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...)
}

#' @rdname scale_colour_hue
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=as.factor(gear)))
#' p + geom_shadowpoint() + scale_shadowcolour_discrete()
#'
#' @export
scale_shadowcolour_discrete <- scale_shadowcolour_hue

#' @rdname scale_brewer
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=as.factor(gear)))
#' p + geom_shadowpoint() + scale_shadowcolour_brewer()
#'
#' @export
scale_shadowcolour_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "shadowcolour") {
  discrete_scale(aesthetics, "brewer", scales::brewer_pal(type, palette, direction), ...)
}

#' @rdname scale_brewer
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=gear))
#' p + geom_shadowpoint() + scale_shadowcolour_distiller() + guides(shadowcolor='none')
#'
#' @export
scale_shadowcolour_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "shadowcolour") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  continuous_scale(aesthetics, "distiller",
                   scales::gradient_n_pal(scales::brewer_pal(type, palette, direction)(7), values, space), na.value = na.value, guide = guide, ...)
  # NB: 6-7 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
  # For diverging scales, you need an odd number to make sure the mid-point is in the center
}

#' @rdname scale_brewer
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=as.factor(gear)))
#' p + geom_shadowpoint() + scale_shadowcolour_brewer()
#'
#'
scale_shadowcolour_fermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "shadowcolour") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  binned_scale(aesthetics, "fermenter", binned_pal(scales::brewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
}

#' @rdname scale_identity
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor='red'))
#' p + geom_shadowpoint() + scale_shadowcolour_identity()
#'
#'
scale_shadowcolour_identity <- function(..., guide = "none", aesthetics = "shadowcolour") {
  sc <- discrete_scale(aesthetics, "identity", scales::identity_pal(), ..., guide = guide,
                       super = ScaleDiscreteIdentity)

  sc
}

#' @rdname scale_continuous
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=gear))
#' p + geom_shadowpoint() + scale_shadowcolour_continuous() + guides(shadowcolour='none')
#'
#'
scale_shadowcolour_continuous <- function(...,
                                    type = getOption("ggplot2.continuous.colour", default = "gradient")) {
  if (is.function(type)) {
    type(...)
  } else if (identical(type, "gradient")) {
    scale_shadowcolour_gradient(...)
  } else if (identical(type, "viridis")) {
    scale_shadowcolour_viridis_c(...)
  } else {
    abort("Unknown scale type")
  }
}

#' @rdname scale_continuous
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=gear))
#' p + geom_shadowpoint() + scale_shadowcolour_binned() + guides(shadowcolour='none')
#'
scale_shadowcolour_binned <- function(...,
                                type = getOption("ggplot2.binned.colour", default = getOption("ggplot2.continuous.colour", default = "gradient"))) {
  if (is.function(type)) {
    type(...)
  } else if (identical(type, "gradient")) {
    scale_shadowcolour_steps(...)
  } else if (identical(type, "viridis")) {
    scale_shadowcolour_viridis_b(...)
  } else {
    abort("Unknown scale type")
  }
}

#' @rdname scale_colour_steps
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=gear))
#' p + geom_shadowpoint() + scale_shadowcolour_steps() + guides(shadowcolour='none')
#'
scale_shadowcolour_steps <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                               na.value = "grey50", guide = "coloursteps", aesthetics = "shadowcolour") {
  binned_scale(aesthetics, "steps", scales::seq_gradient_pal(low, high, space),
               na.value = na.value, guide = guide, ...)
}

#' @rdname scale_colour_steps
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=gear))
#' p + geom_shadowpoint() + scale_shadowcolour_steps2() + guides(shadowcolour='none')
#'
scale_shadowcolour_steps2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                midpoint = 0, space = "Lab", na.value = "grey50", guide = "coloursteps",
                                aesthetics = "shadowcolour") {
  binned_scale(aesthetics, "steps2", scales::div_gradient_pal(low, mid, high, space),
               na.value = na.value, guide = guide, rescaler = mid_rescaler(mid = midpoint), ...)
}

#' @rdname scale_colour_steps
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=gear))
#' p <- p + geom_shadowpoint() + scale_shadowcolour_stepsn(colours=c('red', 'yellow'))
#' p + guides(shadowcolour='none')
#'
scale_shadowcolour_stepsn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                guide = "coloursteps", aesthetics = "shadowcolour", colors) {
  colours <- if (missing(colours)) colors else colours
  binned_scale(aesthetics, "stepsn",
               scales::gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}

#' @rdname scale_gradient
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(economics, aes(date, unemploy, shadowcolor=pce))
#' p + geom_shadowline() + scale_shadowcolour_gradient() + guides(shadowcolour='none')
#'
scale_shadowcolour_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                  na.value = "grey50", guide = "colourbar", aesthetics = "shadowcolour") {
  continuous_scale(aesthetics, "gradient", scales::seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

#' @rdname scale_gradient
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(economics, aes(date, unemploy, shadowcolor=pce))
#' p + geom_shadowline() + scale_shadowcolour_gradient2() + guides(shadowcolour='none')
#'
scale_shadowcolour_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                   midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar",
                                   aesthetics = "shadowcolour") {
  continuous_scale(aesthetics, "gradient2",
                   scales::div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
                   rescaler = mid_rescaler(mid = midpoint))
}

#' @rdname scale_gradient
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(economics, aes(date, unemploy, shadowcolor=pce))
#' p <- p + geom_shadowline() + scale_shadowcolour_gradientn(colours=c('red', 'green'))
#' p + guides(shadowcolour='none')
#'
scale_shadowcolour_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                   guide = "colourbar", aesthetics = "shadowcolour", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale(aesthetics, "gradientn",
                   scales::gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}

#' @rdname scale_gradient
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(economics, aes(uempmed, unemploy, shadowcolor=as.POSIXct(date)))
#' p + geom_shadowpath() + scale_shadowcolour_datetime() + guides(shadowcolour='none')
#'
scale_shadowcolour_datetime <- function(...,
                                  low = "#132B43",
                                  high = "#56B1F7",
                                  space = "Lab",
                                  na.value = "grey50",
                                  guide = "colourbar") {
  datetime_scale(
    "shadowcolour",
    "time",
    palette = scales::seq_gradient_pal(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @rdname scale_gradient
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(economics, aes(uempmed, unemploy, shadowcolor=date))
#' p + geom_shadowpath() + scale_shadowcolour_date() + guides(shadowcolour='none')
#'
scale_shadowcolour_date <- function(...,
                              low = "#132B43",
                              high = "#56B1F7",
                              space = "Lab",
                              na.value = "grey50",
                              guide = "colourbar") {
  datetime_scale(
    "shadowcolour",
    "date",
    palette = scales::seq_gradient_pal(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @rdname scale_grey
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolour=as.factor(gear)))
#' p + geom_glowpoint() + scale_shadowcolour_grey() + guides(shadowcolour='none')
#'
#'
scale_shadowcolour_grey <- function(..., start = 0.2, end = 0.8, na.value = "red", aesthetics = "shadowcolour") {
  discrete_scale(aesthetics, "grey", scales::grey_pal(start, end),
                 na.value = na.value, ...)
}

#' @rdname scale_viridis
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolour=as.factor(gear)))
#' p + geom_glowpoint() + scale_shadowcolour_viridis_d() + guides(shadowcolour='none')
#'
scale_shadowcolour_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", aesthetics = "shadowcolour") {
  discrete_scale(
    aesthetics,
    "viridis_d",
    scales::viridis_pal(alpha, begin, end, direction, option),
    ...
  )

}

#' @rdname scale_viridis
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolour=gear))
#' p + geom_glowpoint() + scale_shadowcolour_viridis_c() + guides(shadowcolour='none')
#'
scale_shadowcolour_viridis_c <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = "colourbar", aesthetics = "shadowcolour") {
  continuous_scale(
    aesthetics,
    "viridis_c",
    scales::gradient_n_pal(
      scales::viridis_pal(alpha, begin, end, direction, option)(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @rdname scale_viridis
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolour=gear))
#' p + geom_glowpoint() + scale_shadowcolour_viridis_b() + guides(shadowcolour='none')
#'
scale_shadowcolour_viridis_b <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = "coloursteps", aesthetics = "shadowcolour") {
  binned_scale(
    aesthetics,
    "viridis_b",
    scales::gradient_n_pal(
      scales::viridis_pal(alpha, begin, end, direction, option)(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @rdname scale_viridis
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolour=as.factor(gear)))
#' p + geom_glowpoint() + scale_shadowcolour_ordinal() + guides(shadowcolour='none')
#'
scale_shadowcolour_ordinal <- scale_shadowcolour_viridis_d

#' @rdname scale_manual
#' @export
#'
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg, shadowcolour=as.factor(gear)))
#' p <- p + geom_glowpoint() + guides(shadowcolour='none')
#' p + scale_shadowcolour_manual(values=c('red', 'blue', 'green'))
#'
scale_shadowcolour_manual <- function(..., values, aesthetics = "shadowcolour", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}


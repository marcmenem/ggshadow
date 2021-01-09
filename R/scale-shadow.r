#' Evenly spaced colours for discrete data
#'
#' This is the default colour scale for categorical variables. It maps each
#' level to an evenly spaced hue on the colour wheel. It does not generate
#' colour-blind safe palettes.
#'
#' @param na.value Colour to use for missing values
#' @inheritDotParams discrete_scale -aesthetics
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @inheritParams scales::hue_pal
#' @rdname scale_hue
#' @export
#' @family colour scales
#' @examples
#' \donttest{
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) + geom_point(aes(colour = clarity)))
#'
#' # Change scale label
#' d + scale_shadowcolour_hue()
#' d + scale_shadowcolour_hue("clarity")
#' d + scale_shadowcolour_hue(expression(clarity[beta]))
#'
#' # Adjust luminosity and chroma
#' d + scale_shadowcolour_hue(l = 40, c = 30)
#' d + scale_shadowcolour_hue(l = 70, c = 30)
#' d + scale_shadowcolour_hue(l = 70, c = 150)
#' d + scale_shadowcolour_hue(l = 80, c = 150)
#'
#' # Change range of hues used
#' d + scale_shadowcolour_hue(h = c(0, 90))
#' d + scale_shadowcolour_hue(h = c(90, 180))
#' d + scale_shadowcolour_hue(h = c(180, 270))
#' d + scale_shadowcolour_hue(h = c(270, 360))
#'
#' # Vary opacity
#' # (only works with pdf, quartz and cairo devices)
#' d <- ggplot(dsamp, aes(carat, price, colour = clarity))
#' d + geom_point(alpha = 0.9)
#' d + geom_point(alpha = 0.5)
#' d + geom_point(alpha = 0.2)
#'
#' # Colour of missing values is controlled with na.value:
#' miss <- factor(sample(c(NA, 1:5), nrow(mtcars), replace = TRUE))
#' ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(colour = miss))
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = miss)) +
#'   scale_shadowcolour_hue(na.value = "black")
#' }
scale_shadowcolour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                        direction = 1, na.value = "grey50", aesthetics = "shadowcolour") {
  discrete_scale(aesthetics, "hue", hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...)
}

scale_shadowcolour_discrete <- scale_shadowcolour_hue


scale_shadowcolour_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "shadowcolour") {
  discrete_scale(aesthetics, "brewer", brewer_pal(type, palette, direction), ...)
}

scale_shadowcolour_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "shadowcolour") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  continuous_scale(aesthetics, "distiller",
                   gradient_n_pal(brewer_pal(type, palette, direction)(7), values, space), na.value = na.value, guide = guide, ...)
  # NB: 6-7 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
  # For diverging scales, you need an odd number to make sure the mid-point is in the center
}

scale_shadowcolour_fermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "shadowcolour") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  binned_scale(aesthetics, "fermenter", binned_pal(brewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
}


scale_shadowcolour_identity <- function(..., guide = "none", aesthetics = "shadowcolour") {
  sc <- discrete_scale(aesthetics, "identity", identity_pal(), ..., guide = guide,
                       super = ScaleDiscreteIdentity)

  sc
}

scale_shadowcolour_continuous <- function(...,
                                    type = getOption("ggplot2.continuous.colour", default = "gradient")) {
  if (is.function(type)) {
    type(...)
  } else if (identical(type, "gradient")) {
    scale_colour_gradient(...)
  } else if (identical(type, "viridis")) {
    scale_colour_viridis_c(...)
  } else {
    abort("Unknown scale type")
  }
}

scale_shadowcolour_binned <- function(...,
                                type = getOption("ggplot2.binned.colour", default = getOption("ggplot2.continuous.colour", default = "gradient"))) {
  if (is.function(type)) {
    type(...)
  } else if (identical(type, "gradient")) {
    scale_colour_steps(...)
  } else if (identical(type, "viridis")) {
    scale_colour_viridis_b(...)
  } else {
    abort("Unknown scale type")
  }
}



scale_shadowcolour_steps <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                               na.value = "grey50", guide = "coloursteps", aesthetics = "shadowcolour") {
  binned_scale(aesthetics, "steps", seq_gradient_pal(low, high, space),
               na.value = na.value, guide = guide, ...)
}


scale_shadowcolour_steps2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                midpoint = 0, space = "Lab", na.value = "grey50", guide = "coloursteps",
                                aesthetics = "shadowcolour") {
  binned_scale(aesthetics, "steps2", div_gradient_pal(low, mid, high, space),
               na.value = na.value, guide = guide, rescaler = mid_rescaler(mid = midpoint), ...)
}


scale_shadowcolour_stepsn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                guide = "coloursteps", aesthetics = "shadowcolour", colors) {
  colours <- if (missing(colours)) colors else colours
  binned_scale(aesthetics, "stepsn",
               gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}


scale_shadowcolour_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                  na.value = "grey50", guide = "colourbar", aesthetics = "shadowcolour") {
  continuous_scale(aesthetics, "gradient", seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

scale_shadowcolour_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                   midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar",
                                   aesthetics = "shadowcolour") {
  continuous_scale(aesthetics, "gradient2",
                   div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
                   rescaler = mid_rescaler(mid = midpoint))
}

scale_shadowcolour_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                   guide = "colourbar", aesthetics = "shadowcolour", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale(aesthetics, "gradientn",
                   gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}


scale_shadowcolour_datetime <- function(...,
                                  low = "#132B43",
                                  high = "#56B1F7",
                                  space = "Lab",
                                  na.value = "grey50",
                                  guide = "colourbar") {
  datetime_scale(
    "shadowcolour",
    "time",
    palette = seq_gradient_pal(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}


scale_shadowcolour_date <- function(...,
                              low = "#132B43",
                              high = "#56B1F7",
                              space = "Lab",
                              na.value = "grey50",
                              guide = "colourbar") {
  datetime_scale(
    "shadowcolour",
    "date",
    palette = seq_gradient_pal(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}

scale_shadowcolour_grey <- function(..., start = 0.2, end = 0.8, na.value = "red", aesthetics = "shadowcolour") {
  discrete_scale(aesthetics, "grey", grey_pal(start, end),
                 na.value = na.value, ...)
}


scale_shadowcolour_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", aesthetics = "shadowcolour") {
  discrete_scale(
    aesthetics,
    "viridis_d",
    viridis_pal(alpha, begin, end, direction, option),
    ...
  )

}

scale_shadowcolour_viridis_c <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = "colourbar", aesthetics = "shadowcolour") {
  continuous_scale(
    aesthetics,
    "viridis_c",
    gradient_n_pal(
      viridis_pal(alpha, begin, end, direction, option)(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}


scale_shadowcolour_viridis_b <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = "coloursteps", aesthetics = "shadowcolour") {
  binned_scale(
    aesthetics,
    "viridis_b",
    gradient_n_pal(
      viridis_pal(alpha, begin, end, direction, option)(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}


scale_shadowcolour_ordinal <- scale_shadowcolour_viridis_d

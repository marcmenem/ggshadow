#' Points
#'
#' The point geom is used to create scatterplots. [geom_glowpoint()] is
#' designed as a drop in replacement for [geom_point()] with an added glow beneath
#' the point to make a busy plot more aesthetically appealing or to make points
#' stand out from the rest of the plot.
#'
#' @inheritParams ggshadow-params
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#'
#' @section Aesthetics:
#' Adds 3 new aesthetics to [geom_point()]:
#' * \code{shadowcolour} defaults to the same color as the point, controls the color of the glow
#' * \code{shadowsize} defaults to \code{size}, controls the sie of the shadow.
#' * \code{shadowalpha} defaults to \code{0.06 * alpha} or \code{0.06}, controls the alpha of the glow
#'
#' @return a layer to add to a plot.
#'
#' @export
#' @examples
#' library( ggplot2 )
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_shadowpoint()
#'
geom_glowpoint <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGlowPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggshadow-ggproto
#' @importFrom glue glue
#' @importFrom glue glue
#' @importFrom rlang warn
#' @importFrom ggplot2 Geom
#' @format NULL
#' @usage NULL
#' @export
GeomGlowPoint <- ggproto("GeomGlowPoint", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = 19, colour = "black", size = 0.3, fill = NA,
    alpha = NA, stroke = 0.5, shadowcolour=NA, shadowsize=NA, shadowalpha = NA
  ),

  handle_na = function(data, params) {
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    # print( data %>% as.tbl )
    # cat('Handle NA\n')

    complete <- stats::complete.cases(data[c("x", "y", "size", "colour")])
    kept <- complete
    data <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      warn(glue("Removed {sum(!kept)} row(s) containing missing values (geom_shadowpoint)."))
    }

    data$shadowcolour[is.na(data$shadowcolour)] <- data$colour
    data$shadowsize[is.na(data$shadowsize)] <- data$size
    data$shadowalpha[is.na(data$shadowalpha)] <- data$alpha * 0.06
    data$shadowalpha[is.na(data$shadowalpha)] <- 0.06

    data
  },

  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }

    coords <- coord$transform(data, panel_params)

    coords.s <- NULL
    for( i in seq(10)){
      coords.i <- coords
      coords.i$colour <- coords$shadowcolour
      coords.i$size <- coords$shadowsize * i
      coords.i$alpha <- coords$shadowalpha

      if (is.null(coords.s)){
        coords.s <- coords.i
      } else {
        coords.s <- rbind(coords.s, coords.i)
      }
    }

    # print( coords.s %>% as.tbl, n=Inf  )
    coords <- rbind( coords.s, coords )[, c( 'x', 'y', 'group', 'shape', 'colour', 'size', 'fill', 'alpha', 'stroke' )]
    # print( coords %>% as.tbl, n=Inf  )

    grid::pointsGrob(
      coords$x, coords$y,
      pch = coords$shape,
      gp = gpar(
        col = alpha(coords$colour, coords$alpha),
        fill = alpha(coords$fill, coords$alpha),
        # Stroke is added around the outside of the point
        fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
        lwd = coords$stroke * .stroke / 2
      )
    )

  }#,

  # draw_key = draw_key_point
)

# translate_shape_string <- getFromNamespace("translate_shape_string", "ggplot2")
# draw_key_point <- getFromNamespace("draw_key_point", "ggplot2")


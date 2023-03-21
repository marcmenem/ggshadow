

#' Connect Observations
#'
#' Plot a shadow beneath the connected lines to make it easier to read a chart with several
#' overlapping observations. `geom_shadowpath()` connects the observations in the order in which they appear
#' in the data. `geom_shadowline()` connects them in order of the variable on the
#' x axis. `geom_shadowstep()` creates a stairstep plot, highlighting exactly
#' when changes occur.
#'
#' The `group` aesthetic determines which cases are
#' connected together. These functions are designed as a straight replacement
#' to the [geom_path()], [geom_line()] and [geom_step()] functions.
#' To set the order of drawing, make the `colour` aesthetic a factor, and set the order
#' from bottom to top.
#'
#' @inheritParams ggshadow-params
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param arrow Arrow specification, as created by [grid::arrow()].
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `linewidth = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @param orientation The orientation of the layer. The default (`NA`)
#' automatically determines the orientation from the aesthetic mapping. In the
#' rare event that this fails it can be given explicitly by setting `orientation`
#' to either `"x"` or `"y"`. See the *Orientation* section for more detail.
#'
#' @seealso
#'  [ggplot2::geom_path()], [ggplot2::geom_line()], [ggplot2::geom_step()]: Filled paths (polygons);
#'
#' @section Missing value handling:
#' `geom_shadowpath()`, `geom_shadowline()`, and `geom_shadowstep()` handle `NA` as follows:
#'
#' *  If an `NA` occurs in the middle of a line, it breaks the line. No warning
#'    is shown, regardless of whether `na.rm` is `TRUE` or `FALSE`.
#' *  If an `NA` occurs at the start or the end of the line and `na.rm` is `FALSE`
#'    (default), the `NA` is removed with a warning.
#' *  If an `NA` occurs at the start or the end of the line and `na.rm` is `TRUE`,
#'    the `NA` is removed silently, without warning.
#'
#' @section Aesthetics:
#' Adds 3 new aesthetics to [geom_path()]:
#' * `shadowcolour` defaults to white, controls the color of the shadow.
#' * `shadowlinewidth` defaults to `2.5 * linewidth`, controls the linewidth of the shadow.
#' * `shadowalpha` defaults to `0.25 * alpha` or `0.9`, controls the alpha of the shadow.
#'
#' @return a layer to add to a plot.
#'
#' @export
#' @examples
#' # geom_shadowline() is suitable for time series
#' library(ggplot2)
#' ggplot(economics_long, aes(date, value01, colour = variable)) + geom_shadowline()
#'
#' ggplot(economics_long, aes(date, value01, colour = value01,
#'                            group = variable, alpha=date, shadowalpha=1)) +
#'           geom_shadowline()
#'
#' @describeIn geom_shadowpath Connects observations in the order in which they appear in the data.
geom_shadowpath <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      lineend = "butt",
                      linejoin = "round",
                      linemitre = 10,
                      arrow = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomShadowPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggshadow-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom grid gpar
#' @importFrom ggplot2 zeroGrob draw_key_path
#' @importFrom vctrs new_data_frame
#' @importFrom cli cli_abort
GeomShadowPath <- ggproto("GeomShadowPath", Geom,
                    required_aes = c("x", "y"),

                    default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA, shadowcolour=NA, shadowlinewidth=NA, shadowalpha = NA),

                    handle_na = function(data, params) {
                      # Drop missing values at the start or end of a line - can't drop in the
                      # middle since you expect those to be shown by a break in the line
                      # print( data %>% as.tbl )
                      # cat('Handle NA\n')

                      complete <- stats::complete.cases(data[c("x", "y", "linewidth", "colour", "linetype")])
                      kept <- stats::ave(complete, data$group, FUN = keep_mid_true)
                      data <- data[kept, ]

                      if (!all(kept) && !params$na.rm) {
                        cli::cli_warn(
                          "{.fn geom_shadowpath}: Removed {sum(!kept)} row{?s} containing missing values."
                        )
                      }

                      data$shadowcolour[is.na(data$shadowcolour)] <- 'white'
                      data$shadowlinewidth <- data$shadowlinewidth %||% data$shadowsize
                      data$shadowlinewidth[is.na(data$shadowlinewidth)] <- (data$linewidth %||% data$size) * 2.5
                      data$shadowalpha[is.na(data$shadowalpha)] <- data$alpha * 0.25
                      data$shadowalpha[is.na(data$shadowalpha)] <- 0.9

                      data
                    },

                    draw_panel = function(data, panel_params, coord, arrow = NULL,
                                          lineend = "butt", linejoin = "round", linemitre = 10,
                                          na.rm = FALSE) {
                      if (!anyDuplicated(data$group)) {
                        cli::cli_inform(
                          c(
                            "!" = "{.fn geom_shadowpath}: Each group consists of
                            only one observation.",
                            "*" = "Do you need to adjust the group aesthetic?"
                          )
                        )
                      }

                      data$shadowlinewidth <- data$shadowlinewidth %||% data$shadowsize

                      # must be sorted on group
                      data <- data[order(data$group), , drop = FALSE]
                      munched <- coord_munch(coord, data, panel_params)

                      # Silently drop lines with less than two points, preserving order
                      rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
                      munched <- munched[rows >= 2, ]
                      if (nrow(munched) < 2) return(ggplot2::zeroGrob())

                      # Work out whether we should use lines or segments
                      attr <- dapply(munched, "group", function(df) {
                        linetype <- unique(df$linetype)
                        vctrs::new_data_frame(
                          solid = identical(linetype, 1) || identical(linetype, "solid"),
                          constant = nrow(unique(df[, c("alpha", "colour","linewidth", "linetype", 'shadowcolour', 'shadowlinewidth', 'shadowalpha')])) == 1
                        )
                      })
                      solid_lines <- all(attr$solid)
                      constant <- all(attr$constant)
                      if (!solid_lines && !constant) {
                        cli::cli_abort(
                          "{.fn geom_shadowpath}: {.arg {c('colour', 'linewidth', 'linetype')}} must be constant over the
                          line when {.arg linetype} is not 'solid'."
                        )
                      }

                      # Work out grouping variables for grobs
                      n <- nrow(munched)
                      group_diff <- munched$group[-1] != munched$group[-n]
                      start <- c(TRUE, group_diff)
                      end <-   c(group_diff, TRUE)

                      if (!constant) {

                        #print( munched %>% as.tbl, n=150 )
                        munched$start <- start
                        munched$end <- end

                        munched.s <- munched
                        munched.s$shadow <- T
                        munched.s$colour <- munched.s$shadowcolour
                        munched.s$linewidth <- munched.s$shadowlinewidth
                        munched.s$alpha <- munched.s$shadowalpha

                        munched$shadow <- F

                        munched <- rbind( munched.s, munched)
                        munched$id <- 2*match(munched$group, unique(munched$group)) - munched$shadow

                        munched <- munched[order(munched$group), c('colour', 'linewidth', 'y', 'x', 'linetype','alpha', 'id', 'start', 'end')]

                        aph <- alpha( munched$colour[munched$start], munched$alpha[munched$start] )

                        # print( munched %>% as.tbl, n=50 )

                        grid::segmentsGrob(
                          munched$x[!munched$end],
                          munched$y[!munched$end],
                          munched$x[!munched$start],
                          munched$y[!munched$start],
                          default.units = "native", arrow = arrow,
                          gp = gpar(
                            col = alpha(munched$colour, munched$alpha)[!munched$end],
                            fill = alpha(munched$colour, munched$alpha)[!munched$end],
                            lwd = munched$linewidth[!munched$end] * .pt,
                            lty = munched$linetype[!munched$end],
                            lineend = lineend,
                            linejoin = linejoin,
                            linemitre = linemitre
                          )
                        )
                      } else {

                        # print( munched %>% as.tbl, n=150 )
                        munched$start <- start

                        munched.s <- munched
                        munched.s$shadow <- T
                        munched.s$colour <- munched.s$shadowcolour
                        munched.s$linewidth <- munched.s$shadowlinewidth %||% munched.s$shadowsize
                        munched.s$alpha <- munched.s$shadowalpha

                        munched$shadow <- F

                        munched <- rbind( munched.s, munched)
                        munched$id <- 2*match(munched$group, unique(munched$group)) - munched$shadow

                        munched <- munched[order(munched$group), c('colour', 'linewidth', 'y', 'x', 'linetype','alpha', 'id', 'start')]

                        aph <- alpha( munched$colour[munched$start], munched$alpha[munched$start] )

                        grid::polylineGrob(
                          munched$x, munched$y, id = munched$id,
                          default.units = "native", arrow = arrow,
                          gp = gpar(
                            col = aph,
                            fill = aph,
                            lwd = munched$linewidth[munched$start] * .pt,
                            lty = munched$linetype[munched$start],
                            lineend = lineend,
                            linejoin = linejoin,
                            linemitre = linemitre
                          )
                        )

                      }
                    },

                    draw_key = ggplot2::draw_key_path,

                    non_missing_aes = "size",
                    rename_size = TRUE
)


#' @describeIn geom_shadowpath Connects observations in order of the variable on the x axis.
#' @export
geom_shadowline <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, orientation = NA,
                      show.legend = NA, inherit.aes = TRUE, ...) {
  # cat('shadow line\n')

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomShadowLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggshadow-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-shadowpath.r
GeomShadowLine <- ggproto("GeomShadowLine", GeomShadowPath,
                    setup_params = function(data, params) {
                      params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
                      params
                    },

                    extra_params = c("na.rm", "orientation"),

                    setup_data = function(data, params) {
                      data$flipped_aes <- params$flipped_aes
                      data <- flip_data(data, params$flipped_aes)
                      data <- data[order(data$PANEL, data$group, data$x), ]
                      flip_data(data, params$flipped_aes)
                    }
)

#' @describeIn geom_shadowpath Creates a stairstep plot, highlighting exactly when changes occur.
#' @param direction direction of stairs: 'vh' for vertical then horizontal,
#'   'hv' for horizontal then vertical, or 'mid' for step half-way between
#'   adjacent x-values.
#' @export
geom_shadowstep <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", direction = "hv",
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomShadowStep,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      direction = direction,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggshadow-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-shadowpath.r
GeomShadowStep <- ggproto("GeomShadowStep", GeomShadowPath,
                    draw_panel = function(data, panel_params, coord, direction = "hv") {
                      data <- dapply(data, "group", stairstep, direction = direction)
                      GeomShadowPath$draw_panel(data, panel_params, coord)
                    }
)

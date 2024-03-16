

#' Connect Observations
#'
#' Plot a glow beneath the connected lines to make it easier to read a chart with several
#' overlapping observations. `geom_glowpath()` connects the observations in the order in which they appear
#' in the data. `geom_glowline()` connects them in order of the variable on the
#' x axis. `geom_glowstep()` creates a stairstep plot, highlighting exactly
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
#'
#' @seealso
#'  [ggplot2::geom_path()], [ggplot2::geom_line()], [ggplot2::geom_step()]:
#'  Filled paths (polygons);
#'
#' @section Missing value handling:
#' `geom_glowpath()`, `geom_glowline()`, and `geom_glowstep()` handle `NA` as
#' follows:
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
#' * `shadowcolour` defaults to path color, controls the color of the shadow.
#' * `shadowlinewidth` defaults to `linewidth`, controls the linewidth of the shadow.
#' * `shadowalpha` defaults to `0.06 * alpha` or `0.06`, controls the alpha of the glow.
#'
#' @return a `ggplot2` layer to add to a plot.
#'
#' @export
#' @examples
#' # geom_glowline() is suitable for time series
#' library(ggplot2)
#' ggplot(economics_long, aes(date, value01, colour = variable)) + geom_glowline()
#'
#'
#' @describeIn geom_glowpath Connects observations in the order in which they appear in the data.
geom_glowpath <- function(mapping = NULL, data = NULL,
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
    geom = GeomGlowPath,
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
#' @importFrom cli cli_warn cli_inform
#' @importFrom grid gList gpar segmentsGrob polylineGrob polygonGrob
#' @importFrom stats complete.cases ave
#' @importFrom scales alpha
#' @importFrom ggplot2 draw_key_path coord_munch
#' @importFrom vctrs new_data_frame
GeomGlowPath <- ggproto("GeomGlowPath", Geom,
                    required_aes = c("x", "y"),

                    default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA,
                                      shadowcolour=NA, shadowlinewidth=NA, shadowalpha = NA,
                                      fill=NA),

                    setup_data = function(data, params) {
                      data$flipped_aes <- params$flipped_aes
                      data <- flip_data(data, params$flipped_aes)
                      data <- transform(data[order(data$PANEL, data$group), ], ymin = 0, ymax = y)

                      # cat( crayon::yellow('setup_data (GlowPath)\n') )
                      # print( data %>% as_tibble )

                      flip_data(data, params$flipped_aes)
                    },

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
                          "{.fn geom_glowpath}: Removed {sum(!kept)} row{?s} containing missing values."
                        )
                      }

                      data$shadowcolour[is.na(data$shadowcolour)] <- data$colour
                      data$linewidth <- data$linewidth %||% data$size
                      data$shadowlinewidth[is.na(data$shadowlinewidth)] <- data$linewidth
                      data$shadowalpha[is.na(data$shadowalpha)] <- data$alpha * 0.06
                      data$shadowalpha[is.na(data$shadowalpha)] <- 0.06

                      # cat( crayon::yellow('handle_na (GlowPath)\n') )
                      # print( data %>% as_tibble )

                      data
                    },

                    draw_panel = function(data, panel_params, coord, arrow = NULL,
                                          lineend = "butt", linejoin = "round", linemitre = 10,
                                          na.rm = FALSE) {
                      if (!anyDuplicated(data$group)) {
                        cli::cli_inform(
                          c(
                            "!" = "{.fn geom_glowpath}: Each group consists of
                            only one observation.",
                            "*" = "Do you need to adjust the group aesthetic?"
                          )
                        )
                      }

                      # cat( crayon::yellow('draw_panel data (GlowPath)\n') )
                      # print( data %>% as_tibble )

                      data$shadowlinewidth <- data$shadowlinewidth %||% data$shadowsize

                      # must be sorted on group
                      data <- data[order(data$group), , drop = FALSE]
                      munched <- ggplot2::coord_munch(coord, data, panel_params)

                      # cat( crayon::yellow('draw_panel munched (GlowPath)\n') )
                      # print( munched %>% as_tibble )

                      # Silently drop lines with less than two points, preserving order
                      rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
                      munched <- munched[rows >= 2, ]
                      if (nrow(munched) < 2) return(ggplot2::zeroGrob())

                      # Work out whether we should use lines or segments
                      attr <- dapply(munched, "group", function(df) {
                        linetype <- unique(df$linetype)
                        vctrs::new_data_frame(
                          solid = identical(linetype, 1) || identical(linetype, "solid"),
                          constant = nrow(unique(df[, c("alpha", "colour","linewidth", "linetype", 'shadowcolour', 'shadowlinewidth', 'shadowalpha', 'fill')])) == 1
                        )
                      })
                      solid_lines <- all(attr$solid)
                      constant <- all(attr$constant)
                      if (!solid_lines && !constant) {
                        cli::cli_abort(
                          "{.fn geom_glowpath}: {.arg {c('colour', 'linewidth', 'linetype')}} must be constant over the
                          line when {.arg linetype} is not 'solid'."
                        )
                      }

                      # Work out grouping variables for grobs
                      n <- nrow(munched)
                      group_diff <- munched$group[-1] != munched$group[-n]
                      start <- c(TRUE, group_diff)
                      end <-   c(group_diff, TRUE)

                      if (!constant) {
                        cli::cli_inform(
                          c(
                            "i" = "Varying color, alpha, linewidth, linetype or shadow values are
                            not supported for {.fn GeomGlowPath}.",
                            ">" = "Defaulting to {.fn GeomShadowLine}"
                          )
                        )
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

                        aph <- scales::alpha( munched$colour[munched$start], munched$alpha[munched$start] )

                        # print( munched %>% as.tbl, n=50 )

                        grid::segmentsGrob(
                          munched$x[!munched$end],
                          munched$y[!munched$end],
                          munched$x[!munched$start],
                          munched$y[!munched$start],
                          default.units = "native", arrow = arrow,
                          gp = grid::gpar(
                            col = scales::alpha(munched$colour, munched$alpha)[!munched$end],
                            fill = scales::alpha(munched$colour, munched$alpha)[!munched$end],
                            lwd = munched$linewidth[!munched$end] * .pt,
                            lty = munched$linetype[!munched$end],
                            lineend = lineend,
                            linejoin = linejoin,
                            linemitre = linemitre
                          )
                        )
                      } else {
                        # print( munched %>% as.tbl, n=15 )
                        munched$start <- start
                        munched$layernum <- 0
                        munched.i <- munched

                        for( i in seq(10)){
                          munched.s <- munched.i
                          munched.s$layernum <- i
                          munched.s$colour <- munched.s$shadowcolour
                          munched.s$linewidth <- (munched.s$shadowlinewidth %||% munched.s$shadowsize) * i
                          munched.s$alpha <- munched.s$shadowalpha

                          munched <- rbind( munched.s, munched)
                        }
                        munched$id <- 11*match(munched$group, unique(munched$group)) - munched$layernum

                        # print( munched %>% as_tibble, n=150 )
                        munched <- munched[order(munched$group), c('colour', 'linewidth', 'y', 'x', 'linetype','alpha', 'id', 'start')]

                        aph <- scales::alpha( munched$colour[munched$start], munched$alpha[munched$start] )

                        g_lines <- grid::polylineGrob(
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


                        if (!is.na(munched.i$fill[1])){
                          grl <- unique(munched.i$group)

                          polys <- grid::gList(g_lines)
                          for( gr in grl){
                            munched.g <- subset(munched.i, group == gr)
                            fillcolour <- munched.g$fill[1]
                            fillalpha  <- munched.g$shadowalpha[1] * 8

                            # cat(crayon::red('Group', gr), fillcolour, fillalpha, '\n')
                            # print( munched.g %>% as_tibble) #, n=Inf )

                            minx <- min( munched.g$x )
                            maxx <- max( munched.g$x )
                            miny <- munched.g$ymin[1]

                            g_poly <- grid::polygonGrob(
                              c(minx, munched.g$x, maxx),
                              c(miny, munched.g$y, miny),
                              default.units = "native",
                              gp = grid::gpar(
                                fill = scales::alpha(fillcolour, fillalpha),
                                col = NA
                              )
                            )

                            polys <- grid::gList( polys, g_poly )
                          }

                          # str( polys )

                          ggname("geom_glowline", grid::grobTree(polys))
                        } else {
                          # cat(crayon::red('No fill colour\n'))
                          ggname("geom_glowline", g_lines)
                        }

                      }
                    },

                    draw_key = ggplot2::draw_key_path,

                    non_missing_aes = "size",
                    rename_size = TRUE
)



#' @describeIn geom_glowpath Connects observations in order of the variable on the x axis.
#' @export
geom_glowline <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, orientation = NA,
                      show.legend = NA, inherit.aes = TRUE, ...) {
  # cat('glow line\n')

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGlowLine,
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
#' @include geom-glowpath.r
GeomGlowLine <- ggproto("GeomGlowLine", GeomGlowPath,
                    setup_params = function(data, params) {
                      params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
                      params
                    },

                    extra_params = c("na.rm", "orientation"),

                    setup_data = function(data, params) {
                      data$flipped_aes <- params$flipped_aes
                      data <- flip_data(data, params$flipped_aes)

                      #data <- data[order(data$PANEL, data$group, data$x), ]
                      data <- transform(data[order(data$PANEL, data$group, data$x), ], ymin = 0, ymax = y)

                      # cat( crayon::yellow('setup_data (GlowLine)\n') )
                      # print( data %>% as_tibble )

                      flip_data(data, params$flipped_aes)
                    }
)

#' @describeIn geom_glowpath Creates a stairstep plot, highlighting exactly when changes occur.
#' @param direction direction of stairs: 'vh' for vertical then horizontal,
#'   'hv' for horizontal then vertical, or 'mid' for step half-way between
#'   adjacent x-values.
#' @export
geom_glowstep <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", direction = "hv",
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGlowStep,
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
#' @include geom-glowpath.r
GeomGlowStep <- ggproto("GeomGlowStep", GeomGlowPath,
                    draw_panel = function(data, panel_params, coord, direction = "hv") {
                      data <- dapply(data, "group", stairstep, direction = direction)
                      GeomGlowPath$draw_panel(data, panel_params, coord)
                    }
)

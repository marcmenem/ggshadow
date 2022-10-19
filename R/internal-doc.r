
#' ggproto classes for ggshadow
#'
#' Please refer to ggplot2 documentation for more information on the ggproto class system
#'
#' @keywords internal
#' @name ggshadow-ggproto
NULL



#' Default params for geoms
#'
#' @param mapping Set of aesthetic mappings created by [aes()] or
#'   [aes_()]. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#'
#' @return a layer object to add to a plot.
#'
#' @keywords internal
#' @name ggshadow-params
NULL



#' Evenly spaced colours for discrete data
#'
#' This is the default colour scale for categorical variables. It maps each
#' level to an evenly spaced hue on the colour wheel. It does not generate
#' colour-blind safe palettes.
#'
#' @param na.value Colour to use for missing values
#' @inheritDotParams ggplot2::discrete_scale -aesthetics
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @inheritParams scales::hue_pal
#' @family colour scales
#'
#' @return a scale object to add to a plot.
#'
#' @name scale_colour_hue
NULL



#' Sequential, diverging and qualitative colour scales from colorbrewer.org
#'
#' @description
#' The `brewer` scales provides sequential, diverging and qualitative
#' colour schemes from ColorBrewer. These are particularly well suited to
#' display discrete values on a map. See \url{https://colorbrewer2.org} for
#' more information.
#'
#' @note
#' The `distiller` scales extend brewer to continuous scales by smoothly
#' interpolating 7 colours from any palette to a continuous scale. The `fermenter`
#' scales provide binned versions of the brewer scales.
#'
#' @details
#' The `brewer` scales were carefully designed and tested on discrete data.
#' They were not designed to be extended to continuous data, but results often
#' look good. Your mileage may vary.
#'
#' @section Palettes:
#' The following palettes are available for use with these scales:
#' \describe{
#'   \item{Diverging}{BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral}
#'   \item{Qualitative}{Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3}
#'   \item{Sequential}{Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges,
#'      OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd}
#' }
#' Modify the palette through the `palette` argument.
#'
#' @inheritParams scales::brewer_pal
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams ggplot2::scale_colour_gradient
#' @inheritParams scales::gradient_n_pal
#' @param palette If a string, will use that named palette. If a number, will index into
#'   the list of palettes of appropriate `type`. The list of available palettes can found
#'   in the Palettes section.
#' @param ... Other arguments passed on to [discrete_scale()], [continuous_scale()],
#'   or [binned_scale()], for `brewer`, `distiller`, and `fermenter` variants
#'   respectively, to control name, limits, breaks, labels and so forth.
#' @family colour scales
#'
#' @return a scale object to add to a plot.
#'
#' @name scale_brewer
NULL




#' Create your own discrete scale
#'
#' These functions allow you to specify your own set of mappings from levels in the
#' data to aesthetic values.
#'
#' The functions `scale_colour_manual()`, `scale_fill_manual()`, `scale_size_manual()`,
#' etc. work on the aesthetics specified in the scale name: `colour`, `fill`, `size`,
#' etc. However, the functions `scale_colour_manual()` and `scale_fill_manual()` also
#' have an optional `aesthetics` argument that can be used to define both `colour` and
#' `fill` aesthetic mappings via a single function call (see examples). The function
#' `scale_discrete_manual()` is a generic scale that can work with any aesthetic or set
#' of aesthetics provided via the `aesthetics` argument.
#'
#' @inheritParams ggplot2::scale_x_discrete
#' @inheritDotParams ggplot2::discrete_scale -expand -position -aesthetics
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @param values a set of aesthetic values to map data values to. The values
#'   will be matched in order (usually alphabetical) with the limits of the
#'   scale, or with `breaks` if provided. If this is a named vector, then the
#'   values will be matched based on the names instead. Data values that don't
#'   match will be given `na.value`.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks (the scale limits)
#'   - A character vector of breaks
#'   - A function that takes the limits as input and returns breaks
#'     as output
#' @section Color Blindness:
#' Many color palettes derived from RGB combinations (like the "rainbow" color
#' palette) are not suitable to support all viewers, especially those with
#' color vision deficiencies. Using `viridis` type, which is perceptually
#' uniform in both colour and black-and-white display is an easy option to
#' ensure good perceptive properties of your visulizations.
#' The colorspace package offers functionalities
#' - to generate color palettes with good perceptive properties,
#' - to analyse a given color palette, like emulating color blindness,
#' - and to modify a given color palette for better perceptivity.
#'
#' For more information on color vision deficiencies and suitable color choices
#' see the [paper on the colorspace package](https://arxiv.org/abs/1903.06490)
#' and references therein.
#' @name scale_manual
#'
#' @return a scale object to add to a plot.
#'
#' @aliases NULL
NULL


#' Continuous and binned colour scales
#'
#' Colour scales for continuous data default to the values of the
#' `ggplot2.continuous.colour` and `ggplot2.continuous.fill` options. These
#' [options()] default to `"gradient"` (i.e., [scale_colour_gradient()] and
#' [scale_fill_gradient()])
#'
#' @param ... Additional parameters passed on to the scale type
#' @param type One of the following:
#'   * "gradient" (the default)
#'   * "viridis"
#'   * A function that returns a continuous colour scale.
#' @seealso [scale_colour_gradient()], [scale_colour_viridis_c()],
#'   [scale_colour_steps()], [scale_colour_viridis_b()], [scale_fill_gradient()],
#'   [scale_fill_viridis_c()], [scale_fill_steps()], and [scale_fill_viridis_b()]
#' @section Color Blindness:
#' Many color palettes derived from RGB combinations (like the "rainbow" color
#' palette) are not suitable to support all viewers, especially those with
#' color vision deficiencies. Using `viridis` type, which is perceptually
#' uniform in both colour and black-and-white display is an easy option to
#' ensure good perceptive properties of your visulizations.
#' The colorspace package offers functionalities
#' - to generate color palettes with good perceptive properties,
#' - to analyse a given color palette, like emulating color blindness,
#' - and to modify a given color palette for better perceptivity.
#'
#' For more information on color vision deficiencies and suitable color choices
#' see the [paper on the colorspace package](https://arxiv.org/abs/1903.06490)
#' and references therein.
#'
#' @return a scale object to add to a plot.
#'
#' @name scale_continuous
NULL



#' Gradient colour scales
#'
#' `scale_*_gradient` creates a two colour gradient (low-high),
#' `scale_*_gradient2` creates a diverging colour gradient (low-mid-high),
#' `scale_*_gradientn` creates a n-colour gradient.
#'
#' Default colours are generated with \pkg{munsell} and
#' `mnsl(c("2.5PB 2/4", "2.5PB 7/10"))`. Generally, for continuous
#' colour scales you want to keep hue constant, but vary chroma and
#' luminance. The \pkg{munsell} package makes this easy to do using the
#' Munsell colour system.
#'
#' @inheritParams scales::seq_gradient_pal
#' @inheritParams ggplot2::scale_colour_gradient
#' @param low,high Colours for low and high ends of the gradient.
#' @param guide Type of legend. Use `"colourbar"` for continuous
#'   colour bar, or `"legend"` for discrete colour legend.
#' @inheritDotParams ggplot2::continuous_scale -na.value -guide -aesthetics
#' @seealso [scales::seq_gradient_pal()] for details on underlying
#'   palette
#' @family colour scales
#'
#' @return a scale object to add to a plot.
#'
#' @name scale_gradient
NULL



#' Binned gradient colour scales
#'
#' `scale_*_steps` creates a two colour binned gradient (low-high),
#' `scale_*_steps2` creates a diverging binned colour gradient (low-mid-high),
#' and `scale_*_stepsn` creates a n-colour binned gradient. These scales are
#' binned variants of the [gradient scale][scale_colour_gradient] family and
#' works in the same way.
#'
#' Default colours are generated with \pkg{munsell} and
#' `mnsl(c("2.5PB 2/4", "2.5PB 7/10"))`. Generally, for continuous
#' colour scales you want to keep hue constant, but vary chroma and
#' luminance. The \pkg{munsell} package makes this easy to do using the
#' Munsell colour system.
#'
#' @inheritParams ggplot2::scale_colour_gradient
#' @inheritDotParams ggplot2::binned_scale -aesthetics -scale_name -palette -na.value -guide -rescaler
#'
#' @seealso [scales::seq_gradient_pal()] for details on underlying
#'   palette
#' @family colour scales
#'
#' @return a scale object to add to a plot.
#'
#' @name scale_colour_steps
NULL



#' Use values without scaling
#'
#' Use this set of scales when your data has already been scaled, i.e. it
#' already represents aesthetic values that ggplot2 can handle directly.
#' These scales will not produce a legend unless you also supply the `breaks`,
#' `labels`, and type of `guide` you want.
#'
#' The functions `scale_colour_identity()`, `scale_fill_identity()`, `scale_size_identity()`,
#' etc. work on the aesthetics specified in the scale name: `colour`, `fill`, `size`,
#' etc. However, the functions `scale_colour_identity()` and `scale_fill_identity()` also
#' have an optional `aesthetics` argument that can be used to define both `colour` and
#' `fill` aesthetic mappings via a single function call. The functions
#' `scale_discrete_identity()` and `scale_continuous_identity()` are generic scales that
#' can work with any aesthetic or set of aesthetics provided via the `aesthetics`
#' argument.
#'
#' @param ... Other arguments passed on to [discrete_scale()] or
#'   [continuous_scale()]
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @param guide Guide to use for this scale. Defaults to `"none"`.
#' @name scale_identity
#'
#' @return a scale object to add to a plot.
#'
#' @aliases NULL
NULL



#' Viridis colour scales from viridisLite
#'
#' The `viridis` scales provide colour maps that are perceptually uniform in both
#' colour and black-and-white. They are also designed to be perceived by viewers
#' with common forms of colour blindness. See also
#' <https://bids.github.io/colormap/>.
#'
#' @inheritParams scales::viridis_pal
#' @inheritParams scales::gradient_n_pal
#' @inheritParams ggplot2::continuous_scale
#' @param ... Other arguments passed on to [discrete_scale()],
#' [continuous_scale()], or [binned_scale] to control name, limits, breaks,
#'   labels and so forth.
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @family colour scales
#'
#' @return a scale object to add to a plot.
#'
#' @name scale_viridis
NULL



#' Sequential grey colour scales
#'
#' Based on [gray.colors()]. This is black and white equivalent
#' of [scale_colour_gradient()].
#'
#' @inheritParams scales::grey_pal
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritDotParams ggplot2::discrete_scale
#' @family colour scales
#'
#' @return a scale object to add to a plot.
#'
#' @name scale_grey
NULL

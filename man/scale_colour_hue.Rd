% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal-doc.r, R/scale-shadow.r
\name{scale_colour_hue}
\alias{scale_colour_hue}
\alias{scale_shadowcolour_hue}
\alias{scale_shadowcolour_discrete}
\title{Evenly spaced colours for discrete data}
\usage{
scale_shadowcolour_hue(
  ...,
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  na.value = "grey50",
  aesthetics = "shadowcolour"
)

scale_shadowcolour_discrete(
  ...,
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
  h.start = 0,
  direction = 1,
  na.value = "grey50",
  aesthetics = "shadowcolour"
)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[ggplot2:discrete_scale]{ggplot2::discrete_scale}}
  \describe{
    \item{\code{scale_name}}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} The name of the scale
that should be used for error messages associated with this scale.}
    \item{\code{palette}}{A palette function that when called with a single integer
argument (the number of levels in the scale) returns the values that
they should take (e.g., \code{\link[scales:pal_hue]{scales::pal_hue()}}).}
    \item{\code{name}}{The name of the scale. Used as the axis or legend title. If
\code{waiver()}, the default, the name of the scale is taken from the first
mapping used for that aesthetic. If \code{NULL}, the legend title will be
omitted.}
    \item{\code{breaks}}{One of:
\itemize{
\item \code{NULL} for no breaks
\item \code{waiver()} for the default breaks (the scale limits)
\item A character vector of breaks
\item A function that takes the limits as input and returns breaks
as output. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}
    \item{\code{labels}}{One of:
\itemize{
\item \code{NULL} for no labels
\item \code{waiver()} for the default labels computed by the
transformation object
\item A character vector giving labels (must be same length as \code{breaks})
\item An expression vector (must be the same length as breaks). See ?plotmath for details.
\item A function that takes the breaks as input and returns labels
as output. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}
    \item{\code{limits}}{One of:
\itemize{
\item \code{NULL} to use the default scale values
\item A character vector that defines possible values of the scale and their
order
\item A function that accepts the existing (automatic) values and returns
new ones. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}
    \item{\code{expand}}{For position scales, a vector of range expansion constants used to add some
padding around the data to ensure that they are placed some distance
away from the axes. Use the convenience function \code{\link[ggplot2:expansion]{expansion()}}
to generate the values for the \code{expand} argument. The defaults are to
expand the scale by 5\% on each side for continuous variables, and by
0.6 units on each side for discrete variables.}
    \item{\code{na.translate}}{Unlike continuous scales, discrete scales can easily show
missing values, and do so by default. If you want to remove missing values
from a discrete scale, specify \code{na.translate = FALSE}.}
    \item{\code{drop}}{Should unused factor levels be omitted from the scale?
The default, \code{TRUE}, uses the levels that appear in the data;
\code{FALSE} uses all the levels in the factor.}
    \item{\code{guide}}{A function used to create a guide or its name. See
\code{\link[ggplot2:guides]{guides()}} for more information.}
    \item{\code{position}}{For position scales, The position of the axis.
\code{left} or \code{right} for y axes, \code{top} or \code{bottom} for x axes.}
    \item{\code{super}}{The super class to use for the constructed scale}
  }}

\item{h}{range of hues to use, in [0, 360]}

\item{c}{chroma (intensity of colour), maximum value varies depending on
combination of hue and luminance.}

\item{l}{luminance (lightness), in [0, 100]}

\item{h.start}{hue to start at}

\item{direction}{direction to travel around the colour wheel,
1 = clockwise, -1 = counter-clockwise}

\item{na.value}{Colour to use for missing values}

\item{aesthetics}{Character string or vector of character strings listing the
name(s) of the aesthetic(s) that this scale works with. This can be useful, for
example, to apply colour settings to the \code{colour} and \code{fill} aesthetics at the
same time, via \code{aesthetics = c("colour", "fill")}.}
}
\value{
a scale object to add to a plot.
}
\description{
This is the default colour scale for categorical variables. It maps each
level to an evenly spaced hue on the colour wheel. It does not generate
colour-blind safe palettes.
}
\examples{
library( ggplot2 )
p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=as.factor(gear)))
p + geom_shadowpoint() + scale_shadowcolour_hue()

library( ggplot2 )
p <- ggplot(mtcars, aes(wt, mpg, shadowcolor=as.factor(gear)))
p + geom_shadowpoint() + scale_shadowcolour_discrete()

}
\seealso{
Other colour scales: 
\code{\link{scale_brewer}},
\code{\link{scale_colour_steps}},
\code{\link{scale_gradient}},
\code{\link{scale_grey}},
\code{\link{scale_viridis}}
}
\concept{colour scales}

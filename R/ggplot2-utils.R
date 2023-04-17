#' `ggplot2-utils.R` includes the non-exported functions from ggplot2 used by
#' ggshadow functions. References to the source files for functions are included
#' in the script below.
#'
#' Updated 2023-03-12.

# ggplot2  is licensed:  MIT + file LICENSE
# and is by:
#   Hadley Wickham [aut] (<https://orcid.org/0000-0003-4757-117X>),
#   Winston Chang [aut] (<https://orcid.org/0000-0002-1576-2126>),
#   Lionel Henry [aut],
#   Thomas Lin Pedersen [aut, cre] (<https://orcid.org/0000-0002-5147-4711>),
#   Kohske Takahashi [aut],
#   Claus Wilke [aut] (<https://orcid.org/0000-0002-7470-9261>),
#   Kara Woo [aut] (<https://orcid.org/0000-0002-5125-4188>),
#   Hiroaki Yutani [aut] (<https://orcid.org/0000-0002-3385-7233>),
#   Dewey Dunnington [aut] (<https://orcid.org/0000-0002-9415-4582>),
#   RStudio [cph, fnd]
#
# MIT License
#
# Copyright (c) 2020 ggplot2 authors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# from geom-path.R ----

# ---
# repo: tidyverse/ggplot2
# file: geom-path.R
# last-updated: 2023-03-07
# license: https://opensource.org/licenses/mit/
# ---

#' @noRd
#' @keywords ggplot2 internal
keep_mid_true <- function(x) {
  first <- match(TRUE, x) - 1
  if (is.na(first)) {
    return(rep(FALSE, length(x)))
  }
  last <- length(x) - match(TRUE, rev(x)) + 1
  c(rep(FALSE, first), rep(TRUE, last - first), rep(
    FALSE,
    length(x) - last
  ))
}

#' Calculate stairsteps for `geom_step()`
#' Used by `GeomStep()`
#'
#' @noRd
#' @keywords ggplot2 internal
#' @importFrom rlang arg_match0
#' @importFrom cli cli_abort
stairstep <- function(data, direction = "hv") {
  direction <- rlang::arg_match0(direction, c("hv", "vh", "mid"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)
  if (n <= 1) {
    return(data[0, , drop = FALSE])
  }
  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2 * n]
    ys <- c(1, rep(2:n, each = 2))
  } else if (direction == "hv") {
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
  } else if (direction == "mid") {
    xs <- rep(1:(n - 1), each = 2)
    ys <- rep(1:n, each = 2)
  } else {
    cli::cli_abort(
      c("{.arg direction} is invalid.",
        i = "Use either {.val vh}, {.val hv}, or {.va mid}"
      )
    )
  }
  if (direction == "mid") {
    gaps <- data$x[-1] - data$x[-n]
    mid_x <- data$x[-n] + gaps / 2
    x <- c(data$x[1], mid_x[xs], data$x[n])
    y <- c(data$y[ys])
    data_attr <- data[c(1, xs, n), setdiff(
      names(data),
      c("x", "y")
    )]
  } else {
    x <- data$x[xs]
    y <- data$y[ys]
    data_attr <- data[xs, setdiff(names(data), c("x", "y"))]
  }
  data_frame0(x = x, y = y, data_attr)
}

# from geom-point.R ----

# ---
# repo: tidyverse/ggplot2
# file: geom-point.R
# last-updated: 2023-03-07
# license: https://opensource.org/licenses/mit/
# ---

#' @noRd
#' @keywords ggplot2 internal
translate_shape_string <- function(shape_string) {
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }
  pch_table <- c(
    `square open` = 0, `circle open` = 1, `triangle open` = 2,
    plus = 3, cross = 4, `diamond open` = 5, `triangle down open` = 6,
    `square cross` = 7, asterisk = 8, `diamond plus` = 9,
    `circle plus` = 10, star = 11, `square plus` = 12, `circle cross` = 13,
    `square triangle` = 14, `triangle square` = 14, square = 15,
    `circle small` = 16, triangle = 17, diamond = 18, circle = 19,
    bullet = 20, `circle filled` = 21, `square filled` = 22,
    `diamond filled` = 23, `triangle filled` = 24, `triangle down filled` = 25
  )
  shape_match <- charmatch(shape_string, names(pch_table))
  invalid_strings <- is.na(shape_match)
  nonunique_strings <- shape_match == 0
  if (any(invalid_strings)) {
    bad_string <- unique0(shape_string[invalid_strings])
    cli::cli_abort("Shape aesthetic contains invalid value{?s}: {.val {bad_string}}")
  }
  if (any(nonunique_strings)) {
    bad_string <- unique0(shape_string[nonunique_strings])
    cli::cli_abort(c("shape names must be given unambiguously",
                     i = "Fix {.val {bad_string}}"
    ))
  }
  unname(pch_table[shape_match])
}

# from scale-gradient.R ----

# ---
# repo: tidyverse/ggplot2
# file: scale-gradient.R
# last-updated: 2023-03-07
# license: https://opensource.org/licenses/mit/
# ---

#' @noRd
#' @keywords ggplot2 internal
#' @importFrom scales rescale_mid
mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

# from scale-manual.R ----

# ---
# repo: tidyverse/ggplot2
# file: scale-manual.R
# last-updated: 2023-03-07
# license: https://opensource.org/licenses/mit/
# ---

#' @noRd
#' @keywords ggplot2 internal
#' @importFrom ggplot2 waiver discrete_scale
#' @importFrom rlang is_missing
manual_scale <- function(aesthetic, values = NULL, breaks = waiver(), ...,
                         limits = NULL) {
  if (rlang::is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }
  if (is.null(limits) && !is.null(names(values))) {
    limits <- function(x) {
      intersect(x, names(values)) %||%
        character()
    }
  }
  if (is.vector(values) && is.null(names(values)) && !is.waive(breaks) &&
      !is.null(breaks) && !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    } else {
      names(values) <- breaks[1:length(values)]
    }
  }
  pal <- function(n) {
    if (n > length(values)) {
      cli::cli_abort("Insufficient values in manual scale.
                     {n} needed but only {length(values)} provided.")
    }
    values
  }
  ggplot2::discrete_scale(aesthetic, "manual", pal,
                          breaks = breaks,
                          limits = limits, ...
  )
}

# from utilities.R  ----

# ---
# repo: tidyverse/ggplot2
# file: utilities.R
# last-updated: 2023-03-07
# license: https://opensource.org/licenses/mit/
# ---

#' @keywords ggplot2 internal
is.waive <- function(x) {
  inherits(x, "waiver")
}

#' @keywords ggplot2 internal
binned_pal <- function(palette) {
  function(x) {
    palette(length(x))
  }
}

#' @keywords ggplot2 internal
#' @importFrom vctrs vec_unique
unique0 <- function(x, ...) {
  if (is.null(x)) x else vctrs::vec_unique(x, ...)
}

#' @noRd
#' @keywords ggplot2 internal
#' @importFrom vctrs vec_rbind
#' @importFrom rlang current_env caller_env
vec_rbind0 <- function(...,
                       .error_call = current_env(),
                       .call = caller_env()) {
  with_ordered_restart(
    vctrs::vec_rbind(..., .error_call = .error_call),
    .call
  )
}

# Wrapping vctrs data_frame constructor with no name repair
#' @noRd
#' @keywords ggplot2 internal
#' @importFrom vctrs data_frame
data_frame0 <- function(...) vctrs::data_frame(..., .name_repair = "minimal")

#' @noRd
#' @keywords ggplot2 internal
split_with_index <- function(x, f, n = max(f)) {
  if (n == 1) {
    return(list(x))
  }
  f <- as.integer(f)
  attributes(f) <- list(
    levels = as.character(seq_len(n)),
    class = "factor"
  )
  unname(split(x, f))
}

#' @noRd
#' @keywords ggplot2 internal
#' @importFrom rlang zap format_error_call is_null
#' @importFrom vctrs vec_ptype2 vec_cast
with_ordered_restart <- function(expr, .call) {
  withCallingHandlers(expr, vctrs_error_incompatible_type = function(cnd) {
    x <- cnd[["x"]]
    y <- cnd[["y"]]
    class_x <- class(x)[1]
    class_y <- class(y)[1]
    restart <- FALSE
    if (is.ordered(x) || is.ordered(y)) {
      restart <- TRUE
      if (is.ordered(x)) {
        x <- factor(as.character(x), levels = levels(x))
      }
      if (is.ordered(y)) {
        y <- factor(as.character(y), levels = levels(y))
      }
    } else if (is.character(x) || is.character(y)) {
      restart <- TRUE
      if (is.character(x)) {
        y <- as.character(y)
      } else {
        x <- as.character(x)
      }
    } else if (is.factor(x) || is.factor(y)) {
      restart <- TRUE
      lev <- c()
      if (is.factor(x)) {
        lev <- c(lev, levels(x))
      }
      if (is.factor(y)) {
        lev <- c(lev, levels(y))
      }
      x <- factor(as.character(x), levels = unique(lev))
      y <- factor(as.character(y), levels = unique(lev))
    }
    if (!restart) {
      return(rlang::zap())
    }
    msg <- paste0(
      "Combining variables of class <", class_x,
      "> and <", class_y, ">"
    )
    desc <- paste0(
      "Please ensure your variables are compatible before plotting (location: ",
      rlang::format_error_call(.call), ")"
    )
    deprecate_soft0("3.4.0", I(msg), details = desc)
    x_arg <- cnd[["x_arg"]]
    y_arg <- cnd[["y_arg"]]
    call <- cnd[["call"]]
    if (inherits(cnd, "vctrs_error_ptype2")) {
      out <- vctrs::vec_ptype2(x, y,
                               x_arg = x_arg, y_arg = y_arg,
                               call = call
      )
      restart <- "vctrs_restart_ptype2"
    } else if (inherits(cnd, "vctrs_error_cast")) {
      out <- vctrs::vec_cast(x, y,
                             x_arg = x_arg, to_arg = y_arg,
                             call = call
      )
      restart <- "vctrs_restart_cast"
    } else {
      return(rlang::zap())
    }
    try_restart <- function(restart, ...) {
      if (!rlang::is_null(findRestart(restart))) {
        invokeRestart(restart, ...)
      }
    }
    try_restart(restart, out)
  })
}

#' @noRd
#' @keywords ggplot2 internal
#' @importFrom rlang caller_env
#' @importFrom lifecycle deprecate_soft
deprecate_soft0 <- function(..., user_env = NULL) {
  user_env <- user_env %||% getOption("ggplot2_plot_env") %||% caller_env(2)
  lifecycle::deprecate_soft(..., user_env = user_env)
}


# From utilities-grid.R ----

# ---
# repo: tidyverse/ggplot2
# file: utilities-grid.R
# last-updated: 2023-03-07
# license: https://opensource.org/licenses/mit/
# ---

#' @noRd
#' @keywords ggplot2 internal
#' @importFrom grid grobName
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

# from performance.R  ----

# ---
# repo: tidyverse/ggplot2
# file: performance.R
# last-updated: 2022-07-05
# license: https://opensource.org/licenses/mit/
# ---

#' @noRd
#' @keywords ggplot2 internal
split_matrix <- function(x, col_names = colnames(x)) {
  force(col_names)
  x <- lapply(seq_len(ncol(x)), function(i) x[, i])
  if (!is.null(col_names)) {
    names(x) <- col_names
  }
  x
}

# More performant modifyList without recursion
#' @keywords ggplot2 internal
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

#' @keywords ggplot2 internal
df_rows <- function(x, i) {
  cols <- lapply(x, `[`, i = i)
  data_frame0(!!!cols, .size = length(i))
}

# from compat-plyr.R ----

# ---
# repo: tidyverse/ggplot2
# file: compat-plyr.R
# last-updated: 2023-03-01
# license: https://opensource.org/licenses/mit/
# ---

#' @noRd
#' @keywords ggplot2 internal
#' @importFrom stats setNames
dapply <- function(df, by, fun, ..., drop = TRUE) {
  grouping_cols <- .subset(df, by)
  fallback_order <- unique0(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res)) {
      return(res)
    }
    if (length(res) == 0) {
      return(data_frame0())
    }
    vars <- lapply(stats::setNames(by, by), function(col) {
      .subset2(
        x,
        col
      )[1]
    })
    if (is.matrix(res)) {
      res <- split_matrix(res)
    }
    if (is.null(names(res))) {
      names(res) <- paste0("V", seq_along(res))
    }
    if (all(by %in% names(res))) {
      return(data_frame0(!!!unclass(res)))
    }
    res <- modify_list(unclass(vars), unclass(res))
    res <- res[intersect(
      c(fallback_order, names(res)),
      names(res)
    )]
    data_frame0(!!!res)
  }
  if (all(vapply(grouping_cols, single_value, logical(1)))) {
    return(apply_fun(df))
  }
  ids <- id(grouping_cols, drop = drop)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  result <- lapply(seq_along(group_rows), function(i) {
    cur_data <- df_rows(df, group_rows[[i]])
    apply_fun(cur_data)
  })
  vec_rbind0(!!!result)
}

#' @noRd
#' @keywords ggplot2 internal
id_var <- function(x, drop = FALSE) {
  if (length(x) == 0) {
    id <- integer()
    n <- 0L
  } else if (!is.null(attr(x, "n")) && !drop) {
    return(x)
  } else if (is.factor(x) && !drop) {
    x <- addNA(x, ifany = TRUE)
    id <- as.integer(x)
    n <- length(levels(x))
  } else {
    levels <- sort(unique0(x), na.last = TRUE)
    id <- match(x, levels)
    n <- max(id)
  }
  attr(id, "n") <- n
  id
}


#' @noRd
#' @importFrom rlang inject
id <- function(.variables, drop = FALSE) {
  nrows <- NULL
  if (is.data.frame(.variables)) {
    nrows <- nrow(.variables)
    .variables <- unclass(.variables)
  }
  lengths <- vapply(.variables, length, integer(1))
  .variables <- .variables[lengths != 0]
  if (length(.variables) == 0) {
    n <- nrows %||% 0L
    id <- seq_len(n)
    attr(id, "n") <- n
    return(id)
  }
  if (length(.variables) == 1) {
    return(id_var(.variables[[1]], drop = drop))
  }
  ids <- rev(lapply(.variables, id_var, drop = drop))
  p <- length(ids)
  ndistinct <- vapply(ids, attr, "n",
                      FUN.VALUE = numeric(1),
                      USE.NAMES = FALSE
  )
  n <- prod(ndistinct)
  if (n > 2^31) {
    char_id <- inject(paste(!!!ids, sep = "\r"))
    res <- match(char_id, unique0(char_id))
  } else {
    combs <- c(1, cumprod(ndistinct[-p]))
    mat <- inject(cbind(!!!ids))
    res <- c((mat - 1L) %*% combs + 1L)
  }
  if (drop) {
    id_var(res, drop = TRUE)
  } else {
    res <- as.integer(res)
    attr(res, "n") <- n
    res
  }
}

#' @noRd
#' @keywords ggplot2 internal
single_value <- function(x, ...) {
  UseMethod("single_value")
}

#' @keywords ggplot2 internal
#' @export
single_value.default <- function(x, ...) {
  # This is set by id() used in creating the grouping var
  identical(attr(x, "n"), 1L)
}

#' @keywords ggplot2 internal
#' @export
single_value.factor <- function(x, ...) {
  # Panels are encoded as factor numbers and can never be missing (NA)
  identical(levels(x), "1")
}

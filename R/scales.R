#' Icon Mapping
#'
#' Conveniently maps data values to icons for use in [magicIcons()]. These
#' functions mirror [leaflet::colorFactor()], [leaflet::colorBin()] and
#' [leaflet::colorQuantile()]: given the same `domain`, `levels`, `bins`,
#' `pretty`, `n`, `probs`, `reverse` and `right` arguments, an icon function
#' and a colour function will split the data in exactly the same way. If
#' `icons` and `palette` have the same length (one per level/bin), each level
#' therefore receives a unique and consistent icon-colour pair. Icons are, by
#' their nature, discrete, so there is no analogue to [leaflet::colorNumeric()].
#' For [iconBin()] and [iconQuantile()], it makes sense to pick "ordered" icons
#' such as the "hourglass" or "temperature" icon families in Font Awesome.
#'
#' @section Matching icons to levels: Levels are ordered in the same way as
#'   [leaflet::colorFactor()]: factor levels if `domain` is a factor, the order
#'   of appearance if `ordered = TRUE`, and otherwise sorted alphabetically. For
#'   [iconBin()] and [iconQuantile()], bins are ordered from lowest to highest.
#'   The first icon is assigned to the first level, the second to the second,
#'   and so on.
#'
#'   Colour palettes can interpolate between colours when there are more levels
#'   than colours; icons cannot. If the number of icons differs from the number
#'   of levels/bins, each level is given the icon at the equivalent position
#'   along the list of icons (so some icons may be repeated or skipped) and a
#'   warning is issued.
#'
#' @param icons The icons that values will be mapped to, one per level or bin.
#'   Icons may be repeated. Note that the `library` (e.g., `"fontawesome"`) is
#'   defined in [magicIcons()].
#' @param domain The possible values that can be mapped. For [iconFactor()]
#'   this should be categorical data (character or factor). For [iconBin()] and
#'   [iconQuantile()] this should be numeric. If `NULL`, levels/bins are
#'   calculated from the data each time the function is called, as in
#'   `{leaflet}`.
#' @param levels An alternate way of specifying levels; if specified, `domain`
#'   is ignored.
#' @param ordered If `TRUE` and `domain` needs to be coerced to a factor, treat
#'   it as already in the correct order.
#' @param na.icon The icon to return for missing or out-of-scope values.
#' @param reverse If `TRUE`, the order of `icons` is reversed.
#' @param bins Either a numeric vector of two or more unique cut points or a
#'   single number (greater than or equal to 2) giving the number of intervals
#'   into which the domain values are to be cut. Defaults to `length(icons)`.
#' @param pretty Whether to use the function [pretty()] to generate the bins
#'   when `bins` is a single number. Note that when `pretty = TRUE`, the number
#'   of bins may not equal `bins`, and may therefore not equal the number of
#'   `icons`; a warning is given if so. Use `pretty = FALSE` to guarantee one
#'   bin per icon.
#' @param right Should the intervals be closed on the right (and open on the
#'   left)? Passed to [cut()]. Note that, to match [leaflet::colorQuantile()],
#'   `right` is ignored by [iconQuantile()] when `domain` is not `NULL`.
#' @param n Number of equal-size quantiles desired. For more precise control,
#'   use the `probs` argument instead. Defaults to `length(icons)`.
#' @param probs A more precise alternative to `n`. Passed directly to
#'   [stats::quantile()].
#'
#' @return A function that takes a single parameter, `x`; when called with a
#'   vector of factors/characters (for [iconFactor()]) or numbers (for
#'   [iconBin()] and [iconQuantile()]), icon names are returned.
#' @export
#'
#' @examples
#' types <- c("Urban Traffic", "Urban Industrial", "Urban Background")
#'
#' # levels are sorted, as in leaflet::colorFactor()
#' iconPal <- iconFactor(c("house", "industry", "car"), types)
#' colorPal <- leaflet::colorFactor(c("#28A197", "#12436D", "#801650"), types)
#' data.frame(type = types, icon = iconPal(types), color = colorPal(types))
#'
#' # numeric data
#' binPal <- iconBin(
#'   c("temperature-low", "temperature-half", "temperature-high"),
#'   domain = 0:30
#' )
#' binPal(c(2, 15, 28))
#'
#' @rdname icon-scales
#' @order 1
iconFactor <- function(
  icons,
  domain,
  levels = NULL,
  ordered = FALSE,
  na.icon = "question",
  reverse = FALSE
) {
  if (missing(domain) && !is.null(levels)) {
    domain <- NULL
  }
  if (!is.null(levels) && anyDuplicated(levels)) {
    warning("Duplicate levels detected")
    levels <- unique(levels)
  }

  lvls <- get_levels(domain, NULL, levels, ordered)
  check_icon_count(icons, length(lvls))
  force(icons)

  args <- list(
    icons = icons,
    levels = lvls,
    ordered = ordered,
    na.icon = na.icon,
    reverse = reverse
  )

  with_icon_attr("factor", args, function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(rep.int(na.icon, length(x)))
    }

    lvls <- get_levels(domain, x, lvls, ordered)

    orig_na <- is.na(x)
    ints <- match(as.character(x), lvls)
    if (any(is.na(ints) != orig_na)) {
      warning(
        "Some values were outside the icon scale and will be treated as NA"
      )
    }

    out <- icons[icon_index(ints, length(lvls), length(icons), reverse)]
    out[is.na(out)] <- na.icon
    out
  })
}

#' @rdname icon-scales
#' @order 2
#' @export
iconBin <- function(
  icons,
  domain,
  bins = length(icons),
  pretty = TRUE,
  na.icon = "question",
  reverse = FALSE,
  right = FALSE
) {
  if (missing(domain) && length(bins) > 1) {
    domain <- NULL
  }
  autobin <- is.null(domain) && length(bins) == 1
  if (!is.null(domain)) {
    bins <- get_bins(domain, NULL, bins, pretty)
  }
  n_bins <- if (length(bins) == 1) bins else length(bins) - 1

  icon_func <- iconFactor(
    icons,
    domain = if (!autobin) seq_len(n_bins),
    na.icon = na.icon,
    reverse = reverse
  )

  args <- list(
    icons = icons,
    bins = bins,
    pretty = pretty,
    na.icon = na.icon,
    reverse = reverse,
    right = right
  )

  with_icon_attr("bin", args, function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(rep.int(na.icon, length(x)))
    }
    bins_to_use <- get_bins(domain, x, bins, pretty)
    ints <- cut(
      x,
      bins_to_use,
      labels = FALSE,
      include.lowest = TRUE,
      right = right
    )
    warn_out_of_range(x, ints)
    icon_func(ints)
  })
}

#' @rdname icon-scales
#' @order 3
#' @export
iconQuantile <- function(
  icons,
  domain,
  n = length(icons),
  probs = seq(0, 1, length.out = n + 1),
  na.icon = "question",
  reverse = FALSE,
  right = FALSE
) {
  if (!is.null(domain)) {
    bins <- stats::quantile(domain, probs, na.rm = TRUE, names = FALSE)
    # leaflet::colorQuantile() does not pass `right` on to colorBin() when a
    # domain is supplied, so `right = FALSE` is always used here. This is
    # mirrored deliberately so icons and colours stay paired.
    bin_func <- iconBin(
      icons,
      domain = NULL,
      bins = bins,
      na.icon = na.icon,
      reverse = reverse
    )
    args <- list(
      icons = icons,
      probs = probs,
      bins = bins,
      na.icon = na.icon,
      reverse = reverse
    )
    return(with_icon_attr("quantile", args, bin_func))
  }

  icon_func <- iconFactor(
    icons,
    domain = seq_len(length(probs) - 1),
    na.icon = na.icon,
    reverse = reverse
  )

  args <- list(
    icons = icons,
    probs = probs,
    bins = NULL,
    na.icon = na.icon,
    reverse = reverse
  )

  with_icon_attr("quantile", args, function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(rep.int(na.icon, length(x)))
    }
    bins_to_use <- stats::quantile(x, probs, na.rm = TRUE, names = FALSE)
    ints <- cut(
      x,
      bins_to_use,
      labels = FALSE,
      include.lowest = TRUE,
      right = right
    )
    warn_out_of_range(x, ints)
    icon_func(ints)
  })
}

# Helpers -----------------------------------------------------------------
# These mirror the internal helpers of leaflet's colour scales so that levels
# and bins are calculated identically.

#' Attach metadata to an icon scale, as leaflet:::withColorAttr()
#'
#' The `iconType` and `iconArgs` attributes allow [addIconLegend()] to
#' reconstruct the levels/bins of an icon scale, in the same way that
#' [leaflet::addLegend()] uses `colorType` and `colorArgs`.
#' @noRd
with_icon_attr <- function(type, args, fun) {
  structure(fun, iconType = type, iconArgs = args)
}

#' Calculate factor levels, as leaflet:::getLevels()
#' @noRd
get_levels <- function(domain, x, lvls, ordered) {
  if (!is.null(lvls)) {
    return(as.character(lvls))
  }
  if (!is.null(domain)) {
    return(calc_levels(domain, ordered))
  }
  if (!is.null(x)) {
    return(calc_levels(x, ordered))
  }
}

#' Calculate factor levels, as leaflet:::calcLevels()
#' @noRd
calc_levels <- function(x, ordered) {
  if (is.null(x)) {
    NULL
  } else if (is.factor(x)) {
    levels(x)
  } else if (ordered) {
    unique(x)
  } else {
    sort(unique(x))
  }
}

#' Calculate bin breaks, as leaflet:::getBins()
#' @noRd
get_bins <- function(domain, x, bins, pretty) {
  if (is.null(domain) && is.null(x)) {
    stop("Assertion failed: domain and x can't both be NULL")
  }
  if (length(bins) > 1) {
    return(bins)
  }
  if (bins < 2) {
    stop(
      "Invalid bins value of ",
      bins,
      "; bin count must be at least 2"
    )
  }
  values <- if (is.null(domain)) x else domain
  if (pretty) {
    base::pretty(values, n = bins)
  } else {
    rng <- range(values, na.rm = TRUE)
    seq(rng[1], rng[2], length.out = bins + 1)
  }
}

#' Map level indices onto icon indices
#'
#' leaflet rescales level `i` of `n_levels` onto `[0, 1]` and samples a colour
#' ramp at that point. Icons can't be interpolated, so the nearest icon is
#' taken instead. When `n_icons == n_levels` this is an exact one-to-one match.
#' @noRd
icon_index <- function(i, n_levels, n_icons, reverse) {
  scaled <- if (n_levels == 1) {
    ifelse(is.na(i), NA_real_, 0.5)
  } else {
    (i - 1) / (n_levels - 1)
  }
  if (reverse) {
    scaled <- 1 - scaled
  }
  round(scaled * (n_icons - 1)) + 1
}

#' Warn if the number of icons doesn't match the number of levels/bins
#' @noRd
check_icon_count <- function(icons, n_levels) {
  if (n_levels > 0 && length(icons) != n_levels) {
    warning(
      "`icons` has length ",
      length(icons),
      " but there are ",
      n_levels,
      " levels/bins; some icons will be repeated or skipped.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Warn about values that fall outside of the bins
#' @noRd
warn_out_of_range <- function(x, ints) {
  if (any(is.na(x) != is.na(ints))) {
    warning(
      "Some values were outside the icon scale and will be treated as NA"
    )
  }
}

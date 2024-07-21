
#' Icon Mapping
#'
#' Conveniently maps data values to icons for use in `magicIcons()`, analogous
#' to [leaflet::colorFactor()], [leaflet::colorBin()] and
#' [leaflet::colorQuantile()]. Icons are, by their nature, discrete, so there is
#' no analogue to [leaflet::colorNumeric()]. For [leaflet::colorBin()] and
#' [leaflet::colorQuantile()], it would make sense to pick "ordered" icons such
#' as the "hourglass" or "temperature" icon families in Font Awesome.
#'
#' @param icons The icons that values will be mapped to. Note that the `library`
#'   (e.g., `"fontawesome"`) is defined in [magicIcons()].
#' @param domain The possible values that can be mapped. For [iconFactor()] this
#'   should be categorical data. For [iconBin()] and [iconQuantile()]
#' @param na.icon The icon to return for missing or out-of-scope values.
#' @param bins,right Passed to [cut()].
#' @param n Number of equal-size quantiles desired. For more precise control,
#'   use the `probs` argument instead.
#' @param probs A more precise alternative to `probs`. Passed directly to
#'   [quantile()].
#'
#' @return A function that takes a single parameter, `x`; when called with a
#'   vector of factors/characters (for [iconFactor()]) or numbers (for
#'   [iconBin()] and [iconQuantile()]), icon names are returned.
#' @export
#'
#' @rdname icon-scales
#' @order 1
iconFactor <- function(icons,
                       domain,
                       na.icon = "question") {
  # match factors to icons
  icons <- unique(icons)
  domain <- unique(domain)
  dict <- stats::setNames(icons, domain)

  fun.out <-
    function(x) {
      x <- unname(dict[x])
      x[is.na(x)] <- na.icon
      return(x)
    }

  attr(fun.out, "type") <- "factor"
  attr(fun.out, "breaks") <- NA
  attr(fun.out, "na.icon") <- na.icon

  return(fun.out)
}

#' @rdname icon-scales
#' @order 2
#' @export
iconBin <- function(icons,
                    domain,
                    bins = length(icons),
                    na.icon = "question",
                    right = FALSE) {
  # build around colorBin to match outputs
  fun <- leaflet::colorBin(
    "viridis",
    domain = domain,
    bins = bins,
    pretty = FALSE,
    right = right,
    na.color = "white"
  )

  # match colours to icons
  dict <- stats::setNames(icons, unique(fun(domain)))

  # function factory
  fun.out <-
    function(x) {
      # run function
      x_cols <- suppressWarnings(fun(x))
      # use dictionary on output
      x_dict <- unname(dict[x_cols])
      # replace NA with the na.icon
      x_dict[is.na(x_dict)] <- na.icon
      x_dict
    }

  # set attributes
  attr(fun.out, "type") <- "bin"
  attr(fun.out, "breaks") <- attributes(fun)$colorArgs$bins
  attr(fun.out, "na.icon") <- na.icon

  # return function
  return(fun.out)
}

#' @rdname icon-scales
#' @order 3
#' @export
iconQuantile <- function(icons,
                         domain,
                         n = length(icons),
                         probs = seq(0, 1, length.out = n + 1),
                         na.icon = "question",
                         right = FALSE) {
  # build around colorBin to match outputs
  fun <- leaflet::colorQuantile(
    "viridis",
    domain = domain,
    n = n,
    probs = probs,
    right = right,
    na.color = "white"
  )

  # match colours to icons
  dict <- stats::setNames(icons, unique(fun(domain)))

  # function factory
  fun.out <-
    function(x) {
      # run function
      x_cols <- suppressWarnings(fun(x))
      # use dictionary on output
      x_dict <- unname(dict[x_cols])
      # replace NA with the na.icon
      x_dict[is.na(x_dict)] <- na.icon
      x_dict
    }

  # set attributes
  attr(fun.out, "type") <- "quantile"
  attr(fun.out, "breaks") <- attributes(fun)$colorArgs$probs
  attr(fun.out, "na.icon") <- na.icon

  # return function
  return(fun.out)
}

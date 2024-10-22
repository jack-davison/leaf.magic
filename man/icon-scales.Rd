% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scales.R
\name{iconFactor}
\alias{iconFactor}
\alias{iconBin}
\alias{iconQuantile}
\title{Icon Mapping}
\usage{
iconFactor(icons, domain, na.icon = "question")

iconBin(
  icons,
  domain,
  bins = length(icons),
  na.icon = "question",
  right = FALSE
)

iconQuantile(
  icons,
  domain,
  n = length(icons),
  probs = seq(0, 1, length.out = n + 1),
  na.icon = "question",
  right = FALSE
)
}
\arguments{
\item{icons}{The icons that values will be mapped to. Note that the \code{library}
(e.g., \code{"fontawesome"}) is defined in \code{\link[=magicIcons]{magicIcons()}}.}

\item{domain}{The possible values that can be mapped. For \code{\link[=iconFactor]{iconFactor()}} this
should be categorical data. For \code{\link[=iconBin]{iconBin()}} and \code{\link[=iconQuantile]{iconQuantile()}}}

\item{na.icon}{The icon to return for missing or out-of-scope values.}

\item{bins, right}{Passed to \code{\link[=cut]{cut()}}.}

\item{n}{Number of equal-size quantiles desired. For more precise control,
use the \code{probs} argument instead.}

\item{probs}{A more precise alternative to \code{probs}. Passed directly to
\code{\link[=quantile]{quantile()}}.}
}
\value{
A function that takes a single parameter, \code{x}; when called with a
vector of factors/characters (for \code{\link[=iconFactor]{iconFactor()}}) or numbers (for
\code{\link[=iconBin]{iconBin()}} and \code{\link[=iconQuantile]{iconQuantile()}}), icon names are returned.
}
\description{
Conveniently maps data values to icons for use in \code{magicIcons()}, analogous
to \code{\link[leaflet:colorNumeric]{leaflet::colorFactor()}}, \code{\link[leaflet:colorNumeric]{leaflet::colorBin()}} and
\code{\link[leaflet:colorNumeric]{leaflet::colorQuantile()}}. Icons are, by their nature, discrete, so there is
no analogue to \code{\link[leaflet:colorNumeric]{leaflet::colorNumeric()}}. For \code{\link[leaflet:colorNumeric]{leaflet::colorBin()}} and
\code{\link[leaflet:colorNumeric]{leaflet::colorQuantile()}}, it would make sense to pick "ordered" icons such
as the "hourglass" or "temperature" icon families in Font Awesome.
}

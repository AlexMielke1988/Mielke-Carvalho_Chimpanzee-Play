% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_serial.R
\name{remove_serial}
\alias{remove_serial}
\title{Goes through bouts, identifies the specified element, and removes it when it occurs in subsequent combinations. For example, take all the continuous 'Bipedal' and remove them if happen again and again}
\usage{
remove_serial(
  elem.bout,
  elem.time = NULL,
  to.remove = "Bipedal",
  move.first = TRUE
)
}
\arguments{
\item{elem.bout}{list of vectors of elements. Co-occurring elements are marked by the percentage symbol. Function returns same vector, but with co-occurring elements split.}

\item{elem.time}{list of vectors of element times. If set to NULL, the function will create a vector with the element order. Makes sure that when elements vector is split, timings are adapted accordingly}

\item{to.remove}{the element that should be removed if it happens continuously}

\item{move.first}{should the element, when occcurring in co-occurrence, be moved to the beginning because it kind of has to happen first (e.g., 'bipedal/arm-swing/hit' becomes 'bipedal' and 'arm-swing/hit')}
}
\value{
Function returns a vector with the split elements
}
\description{
Goes through bouts, identifies the specified element, and removes it when it occurs in subsequent combinations. For example, take all the continuous 'Bipedal' and remove them if happen again and again
}
\author{
Alex Mielke
}

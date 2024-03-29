% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unlist_vector.R
\name{unlist_vector}
\alias{unlist_vector}
\title{Take sequence within bout and randomly assign sequence for co-occurring elements (marked by percentage symbol)}
\usage{
unlist_vector(bout, method = "sample")
}
\arguments{
\item{bout}{vector of elements. Co-occurring elements are marked by the percentage symbol. Function returns same vector, but with co-occurring elements split.}

\item{method}{either 'sample' (just sample one of the co-occurring) or 'random' (randomizes their order)}
}
\value{
Function returns a vector with the split elements
}
\description{
Takes the vector consisting of elements in their order of occurrence and unlists elements that occur at the same time, randomly.
}
\author{
Alex Mielke
}

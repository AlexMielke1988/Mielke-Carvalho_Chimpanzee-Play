% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/element_bout_matrix.R
\name{elem_bout_matrix}
\alias{elem_bout_matrix}
\title{Create matrix of element transitions, across possible random sequences for co-occurring elements}
\usage{
elem_bout_matrix(elem.bout, it = 1, ran.method = "random")
}
\arguments{
\item{elem.bout}{list of vectors of elements. Co-occurring elements are marked by the percentage symbol.}

\item{it}{how many iterations of the random splitting of co-occurring elements should be used? If elements never co-occur, this can be set to 1. If there are many co-occurrences, this number should be high, but this will make things slower}

\item{ran.method}{for co-occurring elements, how should they be split? Can be 'random' (order is randomised), or 'sample' (randomly choose any)}
}
\value{
Function returns a transition matrix. Because it sums over different co-occurrence splits, transitions numbers are not integers
}
\description{
Create matrix of element transitions, across possible random sequences for co-occurring elements
}
\examples{
###
}
\author{
Alex Mielke
}

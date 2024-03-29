% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/similarity_clusters.R
\name{similiarity_clusters}
\alias{similiarity_clusters}
\title{Finds elements that show similar usage patterns, based on euclidean distance between their probabilities or mutual information with all other elements. Uses umap to reduce number of dimensions (bringing it down to 2) and k-means clustering to identify best number of clusters based on AIC values}
\usage{
similiarity_clusters(
  elem.bout,
  measure = c("mi", "prob", "corr"),
  k = NULL,
  facet = FALSE,
  it = 10,
  level = "bigram",
  ran.method = "random",
  n_epochs = 7000,
  trials = 10
)
}
\arguments{
\item{elem.bout}{list of vectors of elements. Co-occurring elements are marked by the percentage symbol.}

\item{measure}{should similarity be calculated based on co-occurrence in same bout ('prob') or point-wise mutual information ('mi')?}

\item{k}{pre-defined cluster solution to be used; NULL if the best cluster solution (based on AIC) should be used}

\item{facet}{should plots be split by cluster? default is FALSE}

\item{it}{how many iterations of the random splitting of co-occurring elements should be used? If elements never co-occur, this can be set to 1. If there are many co-occurrences, this number should be high, but this will make things slower}

\item{level}{Should the correlation/pmi be done on the level of the 'bout' (do two elements co-occur within a bout) or 'bigram' (they follow each other). Default is 'bigram'}

\item{ran.method}{for co-occurring elements, how should they be split? Can be 'random' (order is randomised), or 'sample' (randomly choose any)}

\item{n_epochs}{how many iterations for the UMAP dimension reduction?}

\item{trials}{number of times the cluster detection algorithm should be performed a robust solution}
}
\value{
Returns a list with three plots:
plot.antecedent: plot that shows similarity between elements in the conditional probabilities of which elements follow them in transitions
plot.descendant: plot that shows similarity between elements in the conditional probabilities of which elements precede them in transitions
plot.combined: plot that shows similarity between elements in the conditional probabilities of which elements precede and follow them in transitions
}
\description{
Finds elements that show similar usage patterns, based on euclidean distance between their probabilities or mutual information with all other elements. Uses umap to reduce number of dimensions (bringing it down to 2) and k-means clustering to identify best number of clusters based on AIC values
}
\examples{
###
}
\author{
Alex Mielke
}

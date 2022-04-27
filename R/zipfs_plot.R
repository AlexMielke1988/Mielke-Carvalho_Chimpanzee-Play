#' Plots expected Zipf's distribution against the actual occurrence of elements in the dataset
#'
#' Expected Zipf's distribution is calculated as (Frequency of most common)/Rank
#'
#' @param element vector with element names
#' @param occurrance vector with sums of occurrance for each element
#'
#' @return Function returns the entropy value in bits for each of the levels
#'
#' @importFrom ggplot2 ggplot geom_point theme_classic geom_line xlab ylab ggtitle aes labs scale_colour_manual theme
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
zipf_plot <- function(element,
                      occurrance,
                      zipf = T,
                      title = "Zipfs Law Visualization") {
  word_count <-
    data.frame(element = element,
               count = occurrance) # Data frame containing words and their frequency
  word_count <- word_count[order(-1 * word_count$count), ]
  alpha <- 1 # Change if needed
  word_count$rank <- 1:nrow(word_count)
  word_count$zipfs_freq <-
    ifelse(word_count$rank == 1,
      word_count$count[1],
      word_count$count[1] / word_count$rank^alpha
    )

  if (zipf) {
    zipfs_plot <- ggplot(word_count, aes(x = rank, y = count)) +
      geom_point(aes(color = "observed")) +
      geom_line(aes(color = "observed")) +
      theme_classic() +
      geom_point(aes(y = zipfs_freq, color = "theoretical")) +
      geom_line(aes(y = zipfs_freq, color = "theoretical")) +
      labs(x = "rank", y = "count", title = title) +
      scale_colour_manual(
        name = "Word count",
        values = c("theoretical" = "red", "observed" = "black")
      ) +
      theme(legend.position = "top")
  }
  if (!zipf) {
    zipfs_plot <- ggplot(word_count, aes(x = rank, y = count)) +
      geom_point(color = "black") +
      geom_line(color = "black") +
      theme_classic() +
      labs(x = "rank", y = "count", title = title)
  }
  return(list(word.count = word_count,
              zipf.plot = zipfs_plot))
}

#' Goes through bouts, identifies the specified element, and removes it when it occurs in subsequent combinations. For example, take all the continuous 'Bipedal' and remove them if happen again and again
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol. Function returns same vector, but with co-occurring elements split.
#' @param elem.time list of vectors of element times. If set to NULL, the function will create a vector with the element order. Makes sure that when elements vector is split, timings are adapted accordingly
#' @param to.remove the element that should be removed if it happens continuously
#' @param move.first should the element, when occcurring in co-occurrence, be moved to the beginning because it kind of has to happen first (e.g., 'bipedal/arm-swing/hit' becomes 'bipedal' and 'arm-swing/hit')
#'
#' @return Function returns a vector with the split elements
#'
#' @author Alex Mielke
#' @export
#'

remove_serial <- function(elem.bout,
                          elem.time = NULL,
                          to.remove = "Bipedal",
                          move.first = TRUE) {
  cleaned.bout <- lapply(1:length(elem.bout), function(z) {
    x <- elem.bout[[z]] # assign elements
    if (!is.null(elem.time)) {
      bout.time <- elem.time[[z]]
    } # assign time if time is given
    if (is.null(elem.time)) {
      bout.time <- 1:length(x)
    } # if no time is given, add a 'time' variable
    if (mean(is.na(x)) == 1) {
      return(list(elements = x, time = bout.time))
    }
    if (length(x) == 1) {
      return(list(elements = x, time = bout.time))
    }

    # make list of all elements in event
    x.list <- lapply(x, function(y) {
      unlist(strsplit(y, split = "%", fixed = T)) %>% unique()
    })
    x.time <- lapply(bout.time, function(y) {
      unlist(y)
    })


    # which contain the removed?
    x.contain <- sapply(x.list, function(y) {
      to.remove %in% unlist(y)
    })

    # which contain NA?
    x.na <- is.na(x.list)

    # if(!even.one){
    #   # remove those that contain but also have one before that contains
      x.remove <- c(FALSE, sapply(2:length(x.contain), function(y) {
        x.contain[y] & x.contain[y - 1]
       }))
    # }
    # if(even.one){
      # remove those that contain but also have one before that contains
      # x.remove <- sapply(1:length(x.contain), function(y) {
      #   x.contain[y] & length(x.list[[y]]) > 1
      # })
    # }
    # if there are none, move on
    if (sum(x.contain) == 0) {
      return(list(elements = x, time = bout.time))
    }

    # if there are some: remove in those were it is present and should be contained
    x.list.without <- lapply(1:length(x.list), function(y) {
      if (!(y %in% which(x.remove))) {
        return(x.list[[y]])
      }
      if (y %in% which(x.remove)) {
        return(x.list[[y]][x.list[[y]] != to.remove])
      }
    })

    # turn missing ones into NA
    x.list.without <- lapply(x.list.without, function(y) {
      if (length(y) == 0 | is.null(y)) {
        y <- NA
      }
      if (length(y) > 0 & !is.null(y)) {
        y
      }
    })

    # remove missing ones that were just added by this analysis
    x.removed <- sapply(1:length(x.list.without), function(y) {
      unique(!(is.na(x.list.without[[y]]) & !is.na(x.na[y])))
    })


    # paste them again
    x.list.out <- sapply(x.list.without, function(y) {
      paste(y, collapse = "%")
    })

    # remove the false NA
    x.list.out <- x.list.out[x.removed]
    bout.time <- bout.time[x.removed]

    if (move.first) {
      # in combinations, take out and move first
      x.list.out <- lapply(1:length(x.list.out), function(y) {
        if (!(grepl(x.list.out[[y]], pattern = "%", fixed = T) &
          grepl(x.list.out[[y]], pattern = to.remove, fixed = T))) {
          return(list(element = x.list.out[[y]], time = bout.time[y]))
        }
        if (grepl(x.list.out[[y]], pattern = "%", fixed = T) &
          grepl(x.list.out[[y]], pattern = to.remove, fixed = T)) {
          x.el <- unique(unlist(strsplit(x.list.out[[y]], split = "%")))
          x.el <- x.el[x.el != to.remove]
          return(list(element = c(to.remove, paste(x.el, collapse = "%")), time = rep(bout.time[y], 2)))
        }
      })
      els <- lapply(x.list.out, function(y) {
        y$element
      })
      b.time <- lapply(x.list.out, function(y) {
        y$time
      })
      x.list.out.el <- do.call(c, els)
      x.list.out.t <- do.call(c, b.time)
    }
    if(!move.first){
      x.list.out.el = x.list.out
      x.list.out.t = bout.time
    }

    return(list(elements = x.list.out.el, time = x.list.out.t))
  })
  els <- lapply(cleaned.bout, function(y) {
    y$element
  })
  b.time <- lapply(cleaned.bout, function(y) {
    y$time
  })
  return(list(elements = els, times = b.time))
}

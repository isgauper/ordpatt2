#' Function that finds the Ordinal Patterns probabilities in a time series for a given embedding dimension
#'
#' @usage OPprob2(TS, emb)
#' @param TS time series of length \eqn{n-m+1}
#' @param emb embedding dimension \eqn{m}
#' @returns a sequence of \eqn{n} patterns
#'
#' @name OPprob2
#'
#' @import tibble
#' @import dplyr
#' @import prodlim
#' @import ggplot2
#' @import ggthemes
#'
#' @export
#'
#' @examples
#' set.seed(1234567890, kind="Mersenne-Twister")
#' x <- rnorm(1000) # white noise
#' y <- mov.av(x, order=11) # smoothed with moving averages
#' OPprob2(x, emb=4)
#' OPprob2(y, emb=4)


utils::globalVariables("OP")



OPprob2 <- function(TS, emb, ...){
  UseMethod("OPprob2")
}

# Default method
#' @export
OPprob2.default <- function(TS, emb, ...){

  # TS must not contain characters
  if (!all(sapply(TS, is.numeric))) {
    stop("All columns in 'TS' must be numeric.")
  }

  # TS must not be a list
  if (is.list(TS)){
    stop("'TS' must not be a list")
  }

  # emb must be greater than 1
  if (emb < 2 ){
    stop("'emb' must be greater than 1")
  }

  # emb must be an integer
  if ( emb %% 1 != 0){
    stop("'emb' must be an integer")
  }


  op <- tibble::tibble(
    OP = factor(OPseq2(TS, emb), levels = 1:factorial(emb))
  )

  fr <- op %>% count(OP, .drop = FALSE)
  probs <- fr$n / sum(fr$n)

  result <- as.numeric(probs)
  class(result) <- c("my_OPprob", "numeric")

  return(result)
}


# print without method and class label
#' @export
print.my_OPprob <- function(x, ...) {

  y <- as.numeric(x)
  print(y)

}


# plot histogram of proportion of patterns
#' @export
plot.my_OPprob <- function(x, ...) {

  # get probabilities
  probs <- as.numeric(x)

  # create data frame
  df <- data.frame(
    index = 1:length(probs),
    y = probs
  )

  # plot histogram
  p <- ggplot2::ggplot(df, aes(x = index, y = y)) +
    geom_col(aes(fill="data"),col="black") +
    xlab("Pattern") +
    ylab("Proportion of patterns") +
    ggthemes::theme_tufte() +
    theme(legend.position = "none")

  print(p)

}

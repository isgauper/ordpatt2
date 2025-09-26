#' Entropy Tests of Ordinal Patterns
#'
#' Calculates Fisher Information or Shannon Entropy from Ordinal Pattern probabilities
#' @param p a sequence of ordinal pattern probabilities
#' @param method string; The type of entropy or information to calculate
#' @keywords Ordinal Patterns
#' @export
#' @examples
#' z <- rnorm(1000)
#' op.wn.z <- OPprob(z, emb=4)
#' entropy(op.wn.z, method="Shannon")


#-------Include more examples?-------#
#-------Improve print results so it doesn't say entropy for Fisher Info--------
#-------Should I rename the function so it's more accurate for Fisher too? (I don't think Fisher is a type of entropy)--------#


#------Notes for testing---------#
# error if not one of the expected methods
# make sure results match examples in original code. Example HTsallis(op.ma.4, beta=1.5)


entropy <- function(p, ...){

  UseMethod("entropy")

}


#' @export
entropy.numeric <- function(p, method = c("Fisher", "Renyi", "Shannon", "Tsallis"), beta = 1.5, ...){

      method <- match.arg(method)

      if(!(length(p) >= 2 & min(p) >= 0 & sum(p) <= (1+.Machine$double.eps))) stop("ERROR: Not a valid probability function")

      H <- switch(
        method,

        # compute the normalized Fisher information measure
        Fisher = {

          p1 <- p[-1]
          p2 <- p[-length(p)]

          # Calculate H
          H <- 4 * sum((sqrt(p1) - sqrt(p2))^2)

        },

        # compute normalized Rényi entropy of order beta
        Renyi = {
          N <- length(p)

          # Calculate H
          H <- log(sum(p^beta)) / ((1-beta) * log(N))

        },


        # compute the normalized Shannon entropy
        Shannon = {

          prob <- p[p > 0]
          N <- length(p)

          # Calculate H
          H <- -sum(prob * log(prob)) / log(N)

          },

        # compute normalized Tsallis entropy of index beta
        Tsallis = {

          N <- length(p)
          H <- sum(p - p^beta) / (1 - N^(1-beta))

        }

          )



structure(H, class="my_entropy", method=method)


}

#---------Should probably change this so it returns "Fisher Information", not Entropy.---------
# Print method

#' @export
print.my_entropy <- function(x, ...) {
  cat("Entropy (", attr(x, "method"), "): ", unclass(x), "\n", sep = "")
}


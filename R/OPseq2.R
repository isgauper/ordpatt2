#' Function that finds the OP sequence of a time series, given the
#' embedding dimension and lag
#'
#' @param TS time series
#' @param emb embedding dimension
#' @param lag time lag (default value: 1)
#'
#' @importFrom stats rnorm
#' @export
#'
#' @examples
#' # Generate a time series and compute its ordinal patterns
#' set.seed(1234567890, kind="Mersenne-Twister")
#' x <- rnorm(1000) # white noise
#' OPseq2(x, emb=4, lag=1)
#'


OPseq2 <- function(TS, emb, lag=1){

  # TS must not contain characters
  if (!all(sapply(TS, is.numeric))) {
    stop("All columns in 'TS' must be numeric.")
  }

  # TS must not be a list
  if (is.list(TS)){
    stop("'TS' must not be a list")
  }

  # emb must be greater than 1 and less than Inf
  if (emb < 2 ){
    stop("'emb' must be greater than 1 and less than Inf")
  }

  # emb must be greater than 1 and less than Inf
  if (emb == Inf ){
    stop("'emb' must be greater than 1 and less than Inf")
  }

  # emb must be an integer
  if ( emb %% 1 != 0){
    stop("'emb' must be an integer")
  }

  # lag must be greater than 0
  if (!(lag > 0) ){
    stop("'lag' must be greater than 0")
  }

  # lag must be an integer
  if ( lag %% 1 != 0){
    stop("'lag' must be an integer")
  }


  # number of OP for lag = 1
  el <- length(TS) - emb + 1

  # OP sequence for lag = 1
  seqOP <- vector()
  for (i in 1:el){
    seqOP[i] <- pi_i2(ind_pos(TS[i:(i + emb - 1)]))
  }

  # OP sequence for the given lag

  return(seqOP[seq(1, el, lag)])
}




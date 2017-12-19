#' Simulate parallel sequencing
#' 
#' Simulates the number of copies of each of a set of sequence variants
#' produced from a parallel sequencer.
#' 
#' @param depth Number of sequences produced (integer).
#' @param probs Abundances or probabilities of input sequence variants (numeric).
#'   Must be greater than 0.
#' 
#' @return A vector of counts of sequence variants. 
#' 
#' @examples 
#' sim_seq(depth = 2e5, probs = c(0.5, 1, 100))
#' 
#' @export
sim_seq <- function(depth, probs){
  library(gtools)
  if(depth < 0){
    stop('argument "depth" must be non-negative.')
  }
  if(min(probs) <= 0){
    stop('argument "probs" must be strictly positive')
  }
  seq_alphas <- probs/min(probs)
  res <- round(depth * rdirichlet(n = 1, alpha = seq_alphas)[1,])
  return(res)
}

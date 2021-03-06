#' Simulate the polymerase chain reaction
#' 
#' Simulates the number of copies of template at each cycles of a PCR. 
#' At each cycle, additional copies are modeled as a draw from a multinomial 
#' distribution. The number of additional molecules created at each step 
#' ('size' parameter) is the product of the number of molecules available in 
#' the previous cycle and the efficiency of the reaction at the current cycle. 
#' The efficiency at each cycle is modeled as a logarithmic decay function of 
#' the number of cycles, along with the cycle at which an inflection is reached 
#' and the slope at that cycle. This approximates the loss of efficiency at 
#' each cycle as reaction components are degraded, exhausted, or used up.
#' 
#' @param template_copies integer vector of counts of target fragments 
#' @param template_effs vector of values in {0,1} 
#' @param ncycles number of cycles
#' @param inflection number of cycles at which maximum decrease in efficiency occurs
#' @param slope rate of decrease at inflection (this is not explicitly true)
#' @param full Logical indicating whether to return copies at each cycle, or 
#'   return only the counts after the final cycle. Defaults to FALSE
#' 
#' @return A matrix of counts of molecules 
#'   per input template (columns) at each cycle (rows)
#' 
#' @examples 
#' do_pcr(2, 0.5, ncycles = 40, inflection = 1, slope = 0.9)
#' 
#' @export
do_pcr <- function(template_copies, template_effs, 
                   ncycles, inflection = 18, slope = 0.5, 
                   stochastic = TRUE, full = FALSE){
  library(gtools)
  cycles   <- 0:ncycles
  cycle_efficiency <- 1/(1+exp(slope*(cycles - inflection)))
  # plot(cycles, cycle_efficiency)
  
  if(length(template_copies) != length(template_effs)){
    stop('Template_copies and template_effs must be the same length.')
  }
  product <- matrix(NA, nrow = ncycles, ncol = length(template_copies))
  for(i in 1:ncycles){
    if(i == 1){
      counts_prev <- template_copies
    } else {
      counts_prev <- product[i-1, ]
    }
    cycle_prod <- sum(counts_prev * cycle_efficiency[i])
    cycle_prob <- (counts_prev * template_effs)/sum(counts_prev * template_effs)
    if(stochastic){
      # rmultinom chokes when n (size) > ~2.148e9
      # new_copies <- rmultinom(n = 1, size = cycle_prod, prob = cycle_prob)
      # new_copies <- rpois(n = length(template_copies), 
                          # lambda = cycle_prob * cycle_prod)
      # use dirichlet * cycle productivity instead
      cycle_lambdas <- cycle_prob * (cycle_prob/min(cycle_prob))
      new_copies <- round(cycle_prod * 
        rdirichlet(n = 1, lambdas = cycle_lambdas)[1,])
    }else{
      new_copies <- round(cycle_prob * cycle_prod)
    }
    product[i,] <- as.numeric(counts_prev) + as.numeric(new_copies)      
  }
  if(full){
    return(product)
  } else {
    return(product[ncycles, ])
  }
  # total_prod <- rowSums(product)
  # plot(total_prod, las = 1)
  # points(cycle_efficiency*max(total_prod), type = 'l', col = hsv(1,0.5,1))
}

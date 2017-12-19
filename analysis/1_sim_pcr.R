################################################################################
# simulate PCR
################################################################################

# Primer efficiency will be modified, though original sample will not, 
# so add primer efficiency now rather than generate fixed efficiencies upfront

library(data.table)

eff_mean <- 0.95
var_lev <- c(1, 10, 100, 1000)

pcr_dat <- list()
set.seed(1)
for(v in 1:length(var_lev)){
  SHAPE1 <- eff_mean * var_lev[v]
  SHAPE2 <- (1 - eff_mean) * var_lev[v]
  template_dat[, eff := primer_eff(
    N = templates, mmv = 'custom', mmv.beta = c(SHAPE1, SHAPE2)), 
    by = sample]

  pcr_reps <- 1 # pointless when stochastic = FALSE...
  set.seed(1) # pointless when stochastic = FALSE; remains just in case.
  temp <- list()
  for(i in 1:pcr_reps){
    temp[[i]] <- template_dat[,list(
      rep.pcr = i, 
      mmv = var_lev[v], 
      species, 
      amplicons = do_pcr(template_copies = templates, 
                         template_effs = eff, 
                         ncycles = 30, inflection = 15, slope = 0.5, stochastic = FALSE)
      ), by = sample]
  }
  pcr_dat[[v]] <- rbindlist(temp)
  rm(temp)
}

pcr_dat <- rbindlist(pcr_dat)
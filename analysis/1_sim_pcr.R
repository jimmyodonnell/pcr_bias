################################################################################
# simulate PCR
################################################################################

library(data.table)

# Primer efficiency will be modified, though original sample will not, 
# so add primer efficiency now rather than generate fixed efficiencies upfront
set.seed(1)
template_dat[, eff := primer_eff(N = templates, mmv = 'custom', mmv.beta = c(9.5,0.5)), by = sample]

pcr_reps <- 1 # pointless when stochastic = FALSE...
set.seed(1) # pointless when stochastic = FALSE; remains just in case.
temp <- list()
for(i in 1:pcr_reps){
  temp[[i]] <- template_dat[,list(
    rep.pcr = i, 
    species, 
    amplicons = do_pcr(template_copies = templates, 
                       template_effs = eff, 
                       ncycles = 30, inflection = 15, slope = 0.5, stochastic = FALSE)
    ), by = sample]
}
pcr_dat <- rbindlist(temp)
rm(temp)

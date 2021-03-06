################################################################################
# simulate PCR
################################################################################

# Primer efficiency will be modified, though original sample will not, 
# so add primer efficiency now rather than generate fixed efficiencies upfront

library(data.table)

eff_mean <- 0.95
beta.coefs <- c(1, 2, 5, 10)

pcr_dat <- list()
set.seed(1)
len.outer <- length(beta.coefs)
len.inner <- length(unique(template_dat$templates.id))
for(v in 1:length(beta.coefs)){

  # variance for primer efficiencies
  SHAPE1 <- eff_mean * beta.coefs[v]
  SHAPE2 <- (1 - eff_mean) * beta.coefs[v]

  pcr_reps <- 1 # pointless when stochastic = FALSE...
  set.seed(1) # pointless when stochastic = FALSE; remains just in case.
  temp <- list()
  for(i in 1:pcr_reps){
    temp[[i]] <- template_dat[,list(
      rep.pcr = i, 
      pcr.id = templates.id + len.inner*(v-1), 
      beta.coef = beta.coefs[v], 
      species, 
      templates, 
      eff = primer_eff(
        N = templates, mmv = 'custom', mmv.beta = c(SHAPE1, SHAPE2))
      ), by = templates.id][,
        amplicons := do_pcr(template_copies = templates, template_effs = eff, 
          ncycles = 30, inflection = 15, slope = 0.5, stochastic = FALSE)
      ][,templates := NULL]
  }
  pcr_dat[[v]] <- rbindlist(temp)
  rm(temp)
}

pcr_dat <- rbindlist(pcr_dat)

sim_sequencing <- TRUE
if(sim_sequencing){
  seq_depth <- 2e5
  pcr_dat[, seq.count := sim_seq(depth = seq_depth, probs = amplicons), by = pcr.id]
}else(
  pcr_dat[, seq.count := round(amplicons)]
)
pcr_dat

################################################################################
# simulate PCR
################################################################################

# Primer efficiency will be modified, though original sample will not, 
# so add primer efficiency now rather than generate fixed efficiencies upfront

library(data.table)

eff_mean <- 0.95
var_lev <- c(1, 2, 5, 10)

pcr_dat <- list()
set.seed(1)
len.outer <- length(var_lev)
len.inner <- length(unique(template_dat$templates.id))
for(v in 1:length(var_lev)){

  # variance for primer efficiencies
  SHAPE1 <- eff_mean * var_lev[v]
  SHAPE2 <- (1 - eff_mean) * var_lev[v]
  template_dat[, eff := primer_eff(
    N = templates, mmv = 'custom', mmv.beta = c(SHAPE1, SHAPE2)), 
    by = templates.id]

  pcr_reps <- 1 # pointless when stochastic = FALSE...
  set.seed(1) # pointless when stochastic = FALSE; remains just in case.
  temp <- list()
  for(i in 1:pcr_reps){
    temp[[i]] <- template_dat[,list(
      rep.pcr = i, 
      pcr.id = templates.id + len.inner*(v-1), 
      mmv = var_lev[v], 
      eff.var = var(eff), 
      species, 
      amplicons = do_pcr(template_copies = templates, template_effs = eff, 
        ncycles = 30, inflection = 15, slope = 0.5, stochastic = FALSE)
      ), by = templates.id]
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

library(ggplot2)
p <- ggplot(pcr_dat, aes(factor(mmv), eff.var)) + 
  geom_violin(
    adjust = 10, # kernel density bandwidth
    scale = 'count', # 'width' or 'count' for width at widest point
    draw_quantiles = c(0.25, 0.5, 0.75)
    ) + 
  xlab('Coefficient of beta parameters ("mmv")') +
  ylab('Primer efficiency variance')
p
EXPORT <- FALSE
if(EXPORT){
  plot_name <- 'efficiency_v_betacoef_vio'
  plot_file <- paste0('../figures/', plot_name, '.pdf')
  ggsave(filename = plot_file, device = pdf)
  rm(p)
}

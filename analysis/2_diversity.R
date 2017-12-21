################################################################################
# compare estimates from two samples

library(vegan)

div.dat <- merge(template_dat, pcr_dat, by = c('templates.id', 'species'))

div.dat[ , seq.prop := seq.count/sum(seq.count), by = pcr.id ]
div.dat[ , tem.prop := templates/sum(templates), by = pcr.id ]

div.full <- div.dat[ , .(
  eff.var = var(eff), 
  dist.bc.raw = vegdist(
    x = matrix(data = c(tem.prop, seq.prop), nrow = 2, byrow = TRUE), 
    method = "bray", binary = FALSE), 
  simp.in = diversity(templates, index = "simpson"), 
  shan.in = diversity(templates, index = "shannon"), 
  rich.in = specnumber(templates), 
  simp.out = diversity(seq.count, index = "simpson"), 
  shan.out = diversity(seq.count, index = "shannon"), 
  rich.out = specnumber(seq.count) 
  ), by = c('pcr.id', 'templates.id', 'even', 'rich')
]

################################################################################
# calculate differences between input and output
div.full[, simp.diff := simp.out - simp.in]
div.full[, shan.diff := shan.out - shan.in]
div.full[, rich.diff := rich.out - rich.in]

div.full[ , div.scen := paste(even, rich, sep = '.')]

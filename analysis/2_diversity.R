################################################################################
# compare estimates from two samples

library(vegan)


# div.in <- samples.env[ , .(simp = diversity(templates, index = "simpson")), by = sample]
div.in <- template_dat[ , .(
  simp.in = diversity(templates, index = "simpson"), 
  shan.in = diversity(templates, index = "shannon"), 
  rich.in = specnumber(templates)
  ), by = templates.id]

################################################################################
# Dissimilarity calculation
# Here is an illustration of how I attempt to calculate dissimilarity within DT
PCRID <- 1
temp <- matrix(
  data = c(
    template_dat[
      templates.id == pcr_dat[pcr.id == PCRID, unique(templates.id)],
      templates], 
    pcr_dat[pcr.id == PCRID, seq.count]), 
  nrow = 2, byrow = TRUE
)
vegdist(temp, method = 'bray', binary = FALSE)
# but, whoa whoa whoa, this may not be the best way of doing this... 
# ... instead, something like this?
merge(
  x =      pcr_dat[,.(templates.id, pcr.id, seq.count)], 
  y = template_dat[,.(templates.id, templates)], by = 'templates.id', all = TRUE)
# and then turn these into matrices by pcr.id?

################################################################################
div.out <- pcr_dat[ , .(
  simp.out = diversity(seq.count, index = "simpson"), 
  shan.out = diversity(seq.count, index = "shannon"), 
  rich.out = specnumber(seq.count),
  dist.bc.raw = vegdist(x = matrix(
    c(template_dat[templates.id, templates], seq.count), 
      nrow = 2, byrow = TRUE), 
    method = 'bray', binary = TRUE
  )
), by = .(templates.id, pcr.id, eff.var)]

div.in
div.out

div.full <- merge(div.in, div.out, by = 'templates.id')

################################################################################
# calculate differences between input and output
div.full[, simp.diff := simp.out - simp.in]
div.full[, shan.diff := shan.out - shan.in]
div.full[, rich.diff := rich.out - rich.in]
div.full <- merge(div.full, unique(template_dat[,.(templates.id, even, rich)]), by = 'templates.id')

div.full[ , div.scen := paste(even, rich, sep = '.')]

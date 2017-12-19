################################################################################
# compare estimates from two samples

library(vegan)


# div.in <- samples.env[ , .(simp = diversity(templates, index = "simpson")), by = sample]
div.in <- template_dat[ , .(
  simp.in = diversity(templates, index = "simpson"), 
  shan.in = diversity(templates, index = "shannon"), 
  rich.in = specnumber(templates)
  ), by = sample]

################################################################################
div.out <- pcr_dat[ , .(
  simp.out = diversity(amplicons, index = "simpson"), 
  shan.out = diversity(amplicons, index = "shannon"), 
  rich.out = specnumber(amplicons)
  ), by = .(sample, rep.pcr)]

div.in
div.out

div.full <- merge(div.in, div.out, by = 'sample')

# calculate differences between input and output
div.full[, simp.diff := simp.in - simp.out]
div.full[, shan.diff := shan.in - shan.out]
div.full <- merge(div.full, unique(template_dat[,.(sample, even, rich)]), by = 'sample')

div.full[ , div.scen := paste(even, rich, sep = '.')]

divplot <- function(mmv, metric){
  if(metric == "simpson"){
  	divcol <- quote(simp.diff)
    XLAB <- "Delta Simpson"
  }
  if(metric == "shannon"){
  	divcol <- quote(shan.diff)
  	XLAB <- "Delta Shannon"
  }
  if(metric == "richness"){
  	divcol <- quote(rich.diff)
  	XLAB <- "Delta Richness"
  }
  maintext <- paste0('Mismatch Variation = ', mmv)
  par(mar = c(4, 7, 3, 1))
  boxbase <- function(...){
  	boxplot(div.full[,eval(divcol)] ~ div.full[,div.scen], 
      # data = div.full, 
      # ylim = range(c(div.full[,simp.diff], div.full[,simp.diff])), 
      horizontal = TRUE, las = 1, ...
    )
  }
  boxbase()
  abline(v = 0, col = hsv(1,0.5,1), lty = 2, lwd = 2)
  boxbase(add = TRUE)
  title(main = maintext, xlab = XLAB)
  # points(div.full[, simp.in], col = 2, lwd = 2)
  
}
divplot(mmv = 'Low', metric = "simpson")

# richness biplot
plot(rich.out ~ rich.in, data = unique(div.full[,.(sample, rep.pcr, rich.in, rich.out)]))
abline(a = 0, b = 1, col = hsv(1,0.5,1))
################################################################################
# Plotting diversity
################################################################################

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

# biplots
# richness
par(mar = c(4,4,1,1))
plot(rich.out ~ rich.in, data = div.full)
abline(a = 0, b = 1, col = hsv(1,0.5,1))


# set plot colors
rich.levs <- levels(as.factor(div.full$rich))
ncolors <- length(rich.levs)

even.levs <- levels(as.factor(div.full[,even]))
even.num  <- as.numeric(as.factor(div.full[,even]))

library(RColorBrewer)
mycolors <- brewer.pal(ncolors, 'Spectral')

library(viridis)
mycolors <- viridis(ncolors, end = 0.9, alpha = 0.6)
mycolors.legend <- viridis(ncolors, end = 0.9)

mycolors <- gghue(ncolors, alpha = 0.6)
mycolors.legend <- gghue(ncolors)

EXPORT <- FALSE
plot_name <- "richness_by_efficiency"

if(!exists("legend_text")){ legend_text <- list()}
legend_text[plot_name] <- {
"Scaled change in richness versus primer efficiency variance. 
Color indicates richness level, while shapes indicate evenness levels. 
See methods text for detailed data descriptions."
}
if(EXPORT){
  pdf_file    <- paste0('../figures/', plot_name, ".pdf")
  legend_file <- paste0('../figures/', plot_name, "_legend.txt")
  writeLines(legend_text[[plot_name]], con = legend_file)
  pdf(file = pdf_file, width = 8, height = 8)
}
par(mar = c(4,4,1,1))
plot(rich.diff/rich.in ~ eff.var, data = div.full, 
  col = mycolors[as.factor(div.full$rich)], 
  pch = even.num, lwd = 1, 
  xlab = "Variance in Primer Efficiency", 
  ylab = expression(paste('Scaled ', Delta, ' richness')), 
  las = 1)
legend('topright', title = 'Richness', legend = rich.levs, 
  col = mycolors.legend, pch = 19, pt.lwd = 2, 
  bty = 'n')
legend('bottomright', title = "Evenness", legend = even.levs, 
  pch = 1:length(even.levs), col = grey(0.5), bty = "n")
if(EXPORT){
  dev.off()
}

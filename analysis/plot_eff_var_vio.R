# Generate violin plot of 
library(ggplot2)
p <- ggplot(
  data = div.full, 
  aes(factor(beta.coef), eff.var)
  ) + 
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
}
rm(p)

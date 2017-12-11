gghue <- function(n, alpha = 1){
  # function for plot colors (sorta like ggplot)
  hues = seq(15, 375, length = n+1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}



subject <- 1:10
levs <- c("Templates", "Amplicons")
dat <- expand.grid(subject, levs)
colnames(dat) <- c("subject", "level")

dat$value <- rep(1:10, 2)

dat.skew  <- dat
set.seed(1)
dat.skew[dat.skew$level == "Amplicons", "value"] <- sample(1:10)

dat.zero <- dat.skew
dat.zero[dat.zero$subject %in% 1:3 & dat.zero$level == "Amplicons", "value"] <- 0

plot_pairlines <- function(data, id.col, ttt.col, val.col, 
                           draw.axis.2 = FALSE, draw.axis.4 = FALSE, ...){
  par(mar = c(4,4,1,1))
  xrange <- c(1,2)
  yrange <- c(0, max(data[,val.col]))
  plot(xrange, yrange, type = "n", axes = FALSE, 
       ann = FALSE,
       las = 1, ...) #
  axis(1, at = 1:2, labels = levels(data[,ttt.col]))
  if(draw.axis.2){
    AT.0 <- unique(c(0, data[,val.col]))
    AT <- 2*(0:(max(AT.0)/2))
    axis(2, 
         at = AT, labels = AT*1e2, 
         col = NA, col.ticks = grey(0.8), # alt: lwd = 0, lwd.ticks = 1, 
         las = 1)
  }
  if(draw.axis.4){
    AT.0 <- unique(c(0, data[,val.col]))
    AT <- 2*(0:(max(AT.0)/2))
    axis(4, at = AT, labels = AT*1e9, 
         col = NA, col.ticks = grey(0.8), # alt: lwd = 0, lwd.ticks = 1, 
         las = 1)
  }
  # title(xlab = "Something", ylab = "Count")
  
  mycols <- gghue(length(subject))
  
  for(i in 1:length(unique(data[,id.col]))){
    thisdat <- data[data[,id.col] == i, c(ttt.col, val.col)]
    lines(thisdat, 
          col = mycols[i], lwd = 3)
    points(thisdat, 
           col = mycols[i], pch = 21, bg = 'white', lwd = 3)
  }
}
pdf(file = "pair_line_plot.pdf", width = 6, height = 3)
par(oma = c(1,1,1,6))
layout(mat = matrix(c(1:3), nrow = 1))
plot_pairlines(dat, id.col = "subject", ttt.col = "level", val.col = "value", draw.axis.2 = TRUE)
mtext("Templates", side = 2, line = 3)
plot_pairlines(dat.skew, id.col = "subject", ttt.col = "level", val.col = "value")
plot_pairlines(dat.zero, id.col = "subject", ttt.col = "level", val.col = "value", draw.axis.4 = TRUE)
mtext("Amplicons", side = 4, line = 5)
# text(par("usr")[2]*1.11, mean(par("usr")[3:4]), "AMPLICONS", srt = -90, xpd = TRUE, pos = 4)
dev.off()
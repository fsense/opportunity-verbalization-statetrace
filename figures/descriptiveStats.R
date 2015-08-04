# Descriptive Statistics Plot for the STA Paper
# f.sense@rug.nl | Sept. '13

# Assumes that `combinedDat` is loaded into the environment (created in supplement.Rmd).

subj <- unique(combinedDat$sub)

# Plotting parameters:
pdf('descriptiveStats.pdf', width = 11, height = 5)

par(mfrow = c( 1, 2), las=1)

for(cond in c('simultaneous', 'sequential')) {
  
  plot(NA, ylim=c(0, 1), xlim=c(1, 7), ylab='hits - false alarms', xlab='set size', main=paste(ifelse(cond == 'sequential', '(B) Sequential', '(A) Simultaneous'), 'Condition'), axes=FALSE)
  axis(1, at=c(1:3, 5:7), labels=rep(c(2,4,8), 2), mgp=c(1, 1, 0))
  axis(2, at=seq(0, 1, .1), labels=seq(0, 1, .1))
  abline(h=seq(0, 1, .1), col='gray', lty=2)
  
  # one line per pp
  for(s in unique(combinedDat$sub)) {
    y1 <- combinedDat$d[combinedDat$sub == s & combinedDat$art == 'articulate' & combinedDat$seq == cond]
    y2 <- combinedDat$d[combinedDat$sub == s & combinedDat$art == 'silent' & combinedDat$seq == cond]
    lines(x=c(1:3), y=y1, col='#00000022')
    lines(x=c(5:7), y=y2, col='#00000022')
    points(x=c(1:3, 5:7), y=c(y1, y2), col='#00000055', pch=19)
  }
  
  # group means as symbols
  gM <- aggregate(d ~ ss + seq + art, combinedDat, mean)
  gM <- gM$d[gM$seq == cond]
  lines(x=c(1:3), y=gM[1:3], col='#000000', lwd=2)
  lines(x=c(5:7), y=gM[4:6], col='#000000', lwd=2)
  points(x=c(1:3, 5:7), y=gM, col=rep(c('#FF0000', '#0000FF'), each=3), pch=rep(c(17, 15), each=3), cex=1.8)
  
  # add a legend
  legend(x=3.75, y=.15, legend=c('articulate', 'silent'), col=c('#FF0000', '#0000FF'), pch=c(17,15), bty='n')
}

dev.off()
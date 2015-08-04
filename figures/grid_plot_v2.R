# Grid plot for the STA paper. Code snippets taken from paper/supplement/supplement.Rmd
# Adjustments made according to the following criteria:
# [DONE] a. Make the statetrace plots all fit together in the 5 by 3 grid, with only small gaps. plot the axes only on the outside.
# [DONE] b. Add large axis labels (probably using mtext) on the outer axes, one on each side (ie, they span all plots)
# [DONE] c. Only add the legend to the lower right-hand (ie last) plot.
# d. Add the Bayes factor to each plot
# [DONE] e. They are currently in order of Bayes factor (notice that my code loads up the Bayes factors first and orders on that). Make sure the ordering remains the same.
# [DONE] f. Don't add participant numbers; we don't need them.
# [DONE] g. You can add the grid lines if you like.
# [DONE] h. Export the figure to PDF.

loadSamples = TRUE
plotRestricted <- TRUE

BF = 1:length(unique(combinedDat$sub))
names(BF) = unique(combinedDat$sub)
bf.ord = order(read.table(file="BFs.txt"))
bf <- read.table(file="BFs.txt")

subs = unique(combinedDat$sub)[bf.ord]

sub.samples = list()
if(loadSamples) load("samples.Rda")

# Set up the PDF:
pdf('grid_plot_v2.pdf', width = 8.3, height = 11.7) # A4 sized
# Plotting parameters:
par(mfrow=c(5,3), mar=rep(.2, 4), las=1, oma=c(4.5, 4.5, 3, .2))
# default mar=c(5.1, 4.1, 4.1, 2.1) for bottom, left, top, and right

# for plotting margins:
left <- rep(c(TRUE, FALSE, FALSE), 5)
below <- rep(c(FALSE, FALSE, FALSE, FALSE, TRUE), each=3)
counter <- 1

# Do each subject analysis separately
for(sub in subs){
  datSub = combinedDat[combinedDat$sub==sub,]
  
  if(loadSamples){
    samples.sim = sub.samples[[sub]][["sim"]]
    samples.seq = sub.samples[[sub]][["seq"]]
  }else if(plotRestricted){
    sufficient = FALSE
    samples.sim = NULL
    while(!sufficient){
      samples.sim = rbind(
        samples.sim,
        restrict.samples.mean(simMu,simStdErr,simLab,M=10000,articRestrict=TRUE))
      if(nrow(samples.sim)>5000) sufficient = TRUE
    }
    
    sufficient = FALSE
    samples.seq = NULL
    while(!sufficient){
      samples.seq = rbind(
        samples.seq,
        restrict.samples.mean(seqMu,seqStdErr,simLab,M=10000,articRestrict=TRUE))
      if(nrow(samples.seq)>5000) sufficient = TRUE
    }
  }
  
  if(plotRestricted){
    my.d = c(colMeans(samples.sim),colMeans(samples.seq))
    my.serr = c(apply(samples.sim,2,sd),apply(samples.seq,2,sd))
  }else{
    my.d = datSub$d
    my.serr = datSub$stdErr
  }
  
  
  # Plot
  plot(NA,main="",xlab="",ylab="",ylim=c(0,1),xlim=c(0,1),asp=TRUE, type='n', axes=FALSE)
  
  # Grid lines:
  abline(h=seq(0, 1, .2), lty=2, col='#00000018')
  abline(v=seq(0, 1, .2), lty=2, col='#00000018')
  
  # axes:
  # For some reason, bty='n' doesn't work...
  if(below[counter]) {
    axis(1, seq(0, 1, .2), outer = TRUE)
  }
  
  if(left[counter]) {
    axis(2, seq(0, 1, .2), outer = TRUE)
  }
  
  lines(my.d[1:3 + 6],my.d[1:3],lty=1)
  lines(my.d[1:3 + 3 + 6],my.d[1:3 + 3],lty=2)
  # Error bars:
  arrows(my.d[1:6 + 6]-my.serr[1:6 + 6],my.d[1:6],my.d[1:6 + 6] + my.serr[1:6 + 6], my.d[1:6],code=3,angle=0,col=rgb(1,0,0,.5))
  arrows(my.d[1:6 + 6],my.d[1:6]-my.serr[1:6],my.d[1:6 + 6], my.d[1:6]+my.serr[1:6],code=3,angle=0,col=rgb(1,0,0,.5))
  # plot the numbers on top:
  points(my.d[1:6 + 6],my.d[1:6],pch=as.character(datSub$ss[1:6], cex=1.2))
  
  # inlcude BF:
  text(.2, .9, paste("BF =", round(bf[sub, ], 1)), cex=1.1)
  
  counter <- counter + 1
}

legend(.55, .2 ,legend=c("Articulate","Silent"),lty=1:2, bty='n')

# Overall title:
mtext("State-Trace Plots for Individual Participants", outer=TRUE, line=1, font=2)

# Overarching axis labels:
par(las=0)
mtext("Accuracy in Simultaneous Condition", outer=TRUE, line=3, font=2, side=2) # left margin
mtext("Accuracy in Sequential Condition", outer=TRUE, line=3, font=2, side=1) # bottom margin

# write to PDF and close the device:
dev.off()
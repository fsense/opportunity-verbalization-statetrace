# Effects Plot for the STA paper
# Richard Morey, June 2015

# Assumes that `combinedDat` is loaded into the environment (created in supplement.Rmd).

eff.est = combinedDat[combinedDat$art=="silent","d"] - combinedDat[combinedDat$art=="articulate","d"]
eff.se = sqrt(combinedDat[combinedDat$art=="silent","stdErr"]^2 + combinedDat[combinedDat$art=="articulate","stdErr"]^2)

effDF = cbind(combinedDat[combinedDat$art=="silent",-c(3,5,6)],eff = eff.est, stdErr = eff.se)

which.first = data[data$trial == 3 & data$session == 1,"sequential"]
names(which.first) = data[data$trial == 3 & data$session == 1,"subjNumber"]

effDF$first = factor(which.first[effDF$sub])

pdf(file = "effects.pdf", width = 10, height = 5, version = "1.4")
par(mfrow = c(1,2), las=1, bty="n")

###############
##### Figure 3A:
plot(effDF[effDF$seq == "sequential","eff"], effDF[effDF$seq == "simultaneous","eff"],
     ylim = c(-.25,.25), xlim = c(-.25,.25), ylab = "Simultaneous silent advantage",
     xlab = "Sequential silent advantage", pch = as.character(effDF[effDF$seq == "sequential","ss"]),
     col = 1)#as.integer(effDF$first))

segments(effDF[effDF$seq == "sequential","eff"] - effDF[effDF$seq == "sequential","stdErr"],
       effDF[effDF$seq == "simultaneous","eff"],
       effDF[effDF$seq == "sequential","eff"] + effDF[effDF$seq == "sequential","stdErr"],
       effDF[effDF$seq == "simultaneous","eff"],
       col="lightgray")

segments(effDF[effDF$seq == "sequential","eff"],
         effDF[effDF$seq == "simultaneous","eff"] - effDF[effDF$seq == "simultaneous","stdErr"],
         effDF[effDF$seq == "sequential","eff"],
         effDF[effDF$seq == "simultaneous","eff"] + effDF[effDF$seq == "simultaneous","stdErr"],
         col="lightgray")

points(effDF[effDF$seq == "sequential","eff"], effDF[effDF$seq == "simultaneous","eff"],
 pch = as.character(effDF[effDF$seq == "sequential","ss"]),
 col = 1)#as.integer(effDF$first))
abline(0,1, lty=2)
text(-.24,.24,adj=0,"Simultaneous advantage (28/45)", col="darkgray")
text(.24,-.24,adj=1,"Sequential advantage (17/45)", col="darkgray")

sum((effDF[effDF$seq == "sequential","eff"] - effDF[effDF$seq == "simultaneous","eff"]) > 0)
nrow(effDF[effDF$seq == "sequential",])

###############
##### Figure 3B:
corr.by.block = with(data,tapply(CResp,list(blocknum,setSize,silent,sequential),mean))
adv.for.silent = corr.by.block[,,"silent",] - corr.by.block[,,"articulate",]
effect.by.pres = adv.for.silent[,,"sequential"] - adv.for.silent[,,"simultaneous"]

matplot(effect.by.pres, ylab="Effect (seq - sim)",
        xlab="Block (2 per session)",ty='b',pch=c("2","4","8"),ylim=c(-.21,.21), col=1)
abline(h=0,col="gray")
# text(10,.2,"Silent adv. bigger for seq. pres.",adj=1, col="black")
# text(10,-.2,"Silent adv. bigger for sim. pres.",adj=1, col="black")
text(10,.2,"Silent adv. bigger for sequential pres.",adj=1, col="darkgray")
text(10,-.2,"Silent adv. bigger for simultaneous pres.",adj=1, col="darkgray")

dev.off()
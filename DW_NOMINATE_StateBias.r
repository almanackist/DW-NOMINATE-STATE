#dw <- read.dta("/Users/alanj/Downloads/HL01111E21_PRES.DTA")
#dws <- read.dta("/Users/alanj/Downloads/SL01111E21_BSSE.DTA")
DWcommon <- read.dta("/HANDSL01111E20_BSSE.DTA")
states <- read.table('/states.data')

pres <- subset(DWcommon, cd==0 & state==99)
house <- subset(DWcommon, cd != 0)
sen <- subset(DWcommon, cd==0 & state != 99)

party <- function(data, p) {
  if (p == "D") {
    return(subset(data, party==100))
  } else if (p == "R") {
    return(subset(data, party==200))
  } else if (p == "I") {
    return(subset(data, party!=100 & party!=200))
  } else {
    return()
  }
}

statebias <- function(stateabb, partymedian=FALSE) {
  statenum <- states[rownames(states)==stateabb,]$id
  tot <- subset(house, state==statenum)

  # Base plot
  plot(-10,0,xlim=c(1,111), ylim=c(-1.1, 1.1), frame.plot=F, xlab="Congress", ylab="DW-NOMINATE dim1", main=agg[rownames(agg)==stateabb,]$name)
  
  # Shade background in proportion to degree of conservative (red) or liberal (blue) tendency
  for (i in seq(1,nrow(pres),1)) {
    if (pres[i,]$dwnom1 < 0) {
      rect(pres[i,]$cong,-1,pres[i+1,]$cong,1,border=NA, col=hsv(.6,abs(pres[i,]$dwnom1/3),1))
    } else {
      rect(pres[i,]$cong,-1,pres[i+1,]$cong,1,border=NA, col=hsv(1,pres[i,]$dwnom1/3,1))
    } 
  }
  
  # Plot all individual House, Senate, and presidential points
  points(jitter(party(DWcommon,"D")$cong, factor=3), party(DWcommon,"D")$dwnom1, col=colors()[402], cex=0.1)
  points(jitter(party(DWcommon,"R")$cong, factor=3), party(DWcommon,"R")$dwnom1, col=colors()[421], cex=0.1)
  points(jitter(party(DWcommon,"I")$cong, factor=3), party(DWcommon,"I")$dwnom1, col=colors()[496], cex=0.1)

  # Plot larger circles for all House members of selected state
  hdem <- subset(house, state==statenum & party==100)
  hrep <- subset(house, state==statenum & party==200)
  hind <- subset(house, state==statenum & party!=100 & party!=200)
  points(jitter(hdem$cong, factor=2), hdem$dwnom1, col="black", bg="dodgerblue", pch=21, cex=0.5)
  points(jitter(hrep$cong, factor=2), hrep$dwnom1, col="black", bg="firebrick2", pch=21, cex=0.5)
  points(jitter(hind$cong, factor=2), hind$dwnom1, col="black", bg="seagreen3", pch=21, cex=0.5)
 
  # Plot squares for all Senate members of selected state
  sdem <- subset(sen, state==statenum & party==100)
  srep <- subset(sen, state==statenum & party==200)
  sind <- subset(sen, state==statenum & party!=100 & party!=200)
  points(sdem$cong, sdem$dwnom1, col="black", bg="dodgerblue", pch=22, cex=1)
  points(srep$cong, srep$dwnom1, col="black", bg="red2", pch=22, cex=1)
  points(sind$cong, sind$dwnom1, col="black", bg="seagreen3", pch=22, cex=1)  

  # Plot line for median value of R/D house members from selected state
  if (partymedian==TRUE){
    agg <- aggregate(tot$dwnom1, by=list(tot$party, tot$cong), FUN=mean)
    meandem <- subset(agg, Group.1==100)
    meanrep <- subset(agg, Group.1==200)
    lines(meandem$Group.2, meandem$x, col="dodgerblue", lwd=3)
    lines(meanrep$Group.2, meanrep$x, col="red2", lwd=3)    
  }

  # Horizontal gridlines
  for (i in seq(-1.5,1.5,0.5)) {
    abline(i, 0, col="lightgray")
  } 
  
}

statebias("MA")
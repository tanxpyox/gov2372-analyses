# Figure_A8.R
# John G. Bullock
# 2011 September 5

# Created from Medicaid_2010_05_recall_cued_vs_not.R.

# Creates Figure A8 in  
# 
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (August): 
#    496-515.

library(grid)
library(lattice)
library(Hmisc)
library(pscl)
load('Experiment_2.RData')
postscript.bg      <- 'transparent'
dotcolor           <- 'black'
dotplotlinecolor   <- 'gray67'
dotplotbordercolor <- 'black'
stripbordercolor   <- 'black'
stripbgcolor       <- 'gray77' # Best compromise between screen and print appearance.
PDFwidth        <- 10
PDFheight       <- 10
panelheight     <- 1.25 # inches
panelwidth      <- 1.50 # inches
between.panels     <- list(y=2, x=.9)
base.cex.size   <- 1.05 # standard is 1.0
striptextsize   <-  .75*base.cex.size # cex
xaxissize       <-  .70*base.cex.size # cex
yaxissize       <- xaxissize*1.05 
xlabsize        <-  .80*base.cex.size # cex
ylabsize        <-  .80*base.cex.size # cex
stripheight     <- 1.3
xlab            <- 'Percentages Correctly Recalling Facts about Policy' # 'Percentages Holding Correct Beliefs about Policy Details and Party Cues' # in Experiment 1
xlist           <- list(  draw=TRUE, alternating=FALSE
                        , labels=c('20%', '35%', '50%', '65%'), at=seq(.2, .65, by=.15)
                        , tck=c(.5,0)
                        , cex=xaxissize
                        , limits=c(.12, .78) # c(.585, .815))
                       )
ylist           <- list(draw=TRUE, alternating=c(1,3), limits=c(.6, 4.2),
                        labels=c('policy direction, no cues', 
                                 'policy direction, cues', 
                                 'aid cutoff, no cues', 'aid cutoff, cues', 
                                 'number of recipients, no cues', 
                                 'number of recipients, cues'),
                        tck=c(0,0), col='black', cex=yaxissize)  

# ALIASES
tr   <- data$article.type
PID  <- data$PID.pre
Dem  <- PID=="Dem"
Rep  <- PID=="Rep"
cued <- data$cue.condition!='no_cue'



###################################
# DATA FOR DOTPLOTS
###################################
# Numbers wrong and right for each question
tmp.N           <- apply(memory, 2, function (x) table(x, cued))
tmp.Dem.N       <- apply(memory[Dem,], 2, function (x) table(x, cued[Dem]))
tmp.Rep.N       <- apply(memory[Rep,], 2, function (x) table(x, cued[Rep]))
rownames(tmp.N) <- c('false.nocue', 'true.nocue', 'false.cue', 'true.cue')
rownames(tmp.Rep.N) <- rownames(tmp.Dem.N) <- rownames(tmp.N)

# New data structure for numbers right and wrong.  Five columns for uncued 
# subjects, then five columns for cued subjects
tmp.N2          <- cbind(tmp.N[1:2,], tmp.N[3:4,]) 
tmp.Dem.N2      <- cbind(tmp.Dem.N[1:2,], tmp.Dem.N[3:4,])
tmp.Rep.N2      <- cbind(tmp.Rep.N[1:2,], tmp.Rep.N[3:4,])
percent.right   <- c(tmp.N2[2,]/(tmp.N2[1,]+tmp.N2[2,]),
                     tmp.Dem.N2[2,]/(tmp.Dem.N2[1,]+tmp.Dem.N2[2,]),
                     tmp.Rep.N2[2,]/(tmp.Rep.N2[1,]+tmp.Rep.N2[2,]))

# Create data frame for plotting                 
memory.plotdata <- data.frame(question=rep(colnames(memory), 6),
                              party=rep(c('all', 'Dem', 'Rep'), each=6),
                              cued=rep(c(FALSE, TRUE), each=ncol(memory)),
                              percent.right=percent.right, 
                              lower=rep(NA, ncol(memory)),
                              upper=rep(NA, ncol(memory)))

tmp.CI.data <- cbind(tmp.N2, tmp.Dem.N2, tmp.Rep.N2)
for (i in 1:nrow(memory.plotdata)) { 
  memory.plotdata[i, c('lower', 'upper')] <- betaHPD(1+tmp.CI.data[2,i], 
                                                     1+tmp.CI.data[1,i], p=.95) 
}


# ORDER THE DATA
memory.plotdata$order <- c(2.6, 3.8, 1.4, 2.2, 3.4, 1) 
ylist$at <- rev(sort(unique(memory.plotdata$order)))

# SET XLIM
# Set xlim so that that there is equal space on either 
# side of the extreme margins.  [2010 05 16]
# tmp1 <- max(memory.plotdata$upper) - floor(max(memory.plotdata$upper))
# tmp2 <- ceiling(min(memory.plotdata$lower)) - min(memory.plotdata$lower)
# tmp <- max(tmp1, tmp2)
# xlim <- c(ceiling(min(memory.plotdata$lower))-tmp, floor(max(memory.plotdata$upper))+tmp) 
# xlist$limits <- xlim
tmp <- table(tr, memory[, 'cutoff'])
tmp[,2]/apply(tmp, 1, sum)
tmp <- table(data$cue.memory.cutoff, memory[, 'cutoff'])



###################################
# STRIP FUNCTION
###################################
horiz.strip <- function(which.panel, ...) {
  striptext <- c(paste('all subjects (N\u2006=\u2006', nrow(data), ')', sep=''),
                 paste('Democrats (N\u2006=\u2006',    sum(Dem),   ')', sep=''),
                 paste('Republicans (N\u2006=\u2006',  sum(Rep),   ')', sep=''))
  grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, 
                                                       fill=stripbgcolor, 
                                                       lwd=.5))                                                    
  ltext(.5, .5, striptext[panel.number()], font=2, cex=striptextsize) 
}



###################################
# PANEL FUNCTION
###################################
myPanel <- function(...) {
  trellis.par.set("clip", list(panel="on", strip="on")) 
  panel.Dotplot(...)
  trellis.par.set("clip", list(panel="off", strip="off")) # for permitting panel.axis(outside=T, ...)
  trellis.par.set("axis.text", list(cex=.65)) # set size of axis text
  panel.axis(side="bottom", outside=T, at=seq(.45, .75, by=.15), 
             label=c('45%', '60%', '75%'), 
             text.cex=xaxissize, rot=c(0, 0), tck=.75)
}



###################################
# PRINT FIGURE TO SCREEN
###################################
trellis.par.set("axis.line",  list(alpha=1, col=dotplotbordercolor, lty=1, lwd=.5))
trellis.par.set("clip",       list(panel="off", strip="off")) #                          for permitting panel.axis(outside=T, ...)
trellis.par.set("dot.symbol", list(alpha=1, cex=.5, col=dotcolor, font=1, pch=16)) # black dot (not grey) to mark the mean
trellis.par.set("dot.line",   list(alpha=1, col=dotplotlinecolor, lty=1, lwd=1))
trellis.par.set("plot.line",  list(alpha=1, col=dotcolor, lty=1, lwd=1))
tmp <- trellis.par.get("layout.heights"); tmp$axis.xlab.padding <- 1.40; trellis.par.set("layout.heights", tmp)
trellis.par.set("par.xlab.text", list(alpha=1, cex=xlabsize, col='black', font=2)) # font=2 gives bold print for x label
tmp <- trellis.par.get("strip.border"); tmp$col[1] <- 'white'; trellis.par.set("strip.border", tmp)
Medicaid201005.dotplot.recall.cuevnot <- Dotplot(order ~ Cbind(percent.right, lower, upper) | party, 
                                                 data=memory.plotdata,
                                                 xlab=xlab, ylab='',
                                                 layout=c(3,1), # x columns, y rows
                                                 between=between.panels,                   
                                                 strip=horiz.strip,
                                                 par.strip.text=list(lines=stripheight),
                                                 scales=list(y=ylist, x=xlist))
print(Medicaid201005.dotplot.recall.cuevnot, 
      panel.width=list(panelwidth, "inches"), 
      panel.height=list(panelheight, "inches"))

# Figure_A3.R
# John G. Bullock
# 2011 September 4

# Created from Medicaid_2008_11_recall_normal_and_guessing_corrected.R.

# Creates Figure A3 in the appendix to  
# 
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (3): 496-515.

load('Experiment_1.RData')
library(grid)
library(lattice)
library(Hmisc)
library(Design)
library(pscl)



################################################################
# SET UP FIGURE PARAMETERS [2009 03 10]
################################################################
postscript.bg      <- 'transparent'
dotcolor           <- 'black'
dotplotlinecolor   <- 'gray67'
dotplotbordercolor <- 'black'
stripbordercolor   <- 'black'
stripbgcolor       <- 'gray77' # prints right on Nora's printer.  Best compromise between screen and print appearance.
panelheight   <- 0.80
panelwidth      <- 2.00 # inches
stripheight     <- 2.075
xlab            <- ''    # 'Percentages Holding Correct Beliefs about Policy Details and Party Cues' # in Experiment 1
base.cex.size   <- 1.00
striptextsize   <-  .80*base.cex.size # cex
xlabsize        <-  .80*base.cex.size # cex
xaxissize       <-  .80*base.cex.size # cex
ylabsize        <-  .80*base.cex.size # cex
xlist           <- list(  draw=TRUE, alternating=c(1,1), labels=c('10%', '', '30%', '', '50%', '', '70%'), at=seq(.1, .7, by=.10), tck=c(.45,0), cex=xaxissize)
ylist.labels    <- c('number of recipients', 'aid cutoff', 'Democratic stance', 'Republican stance', 'policy direction')
ylist.labels <- ylist.labels[-c(3,4)] 
ylist           <- list(  draw=TRUE,  alternating=c(3,3)
                   	    , labels=ylist.labels
                        , at=length(ylist.labels):1
			                  , tck=c(0,0), col='black', cex=ylabsize)  

# ALIASES
tr  <- data$article.type
PID <- data$PID.pre
Dem <- PID=="Dem"
Rep <- PID=="Rep"



############################
# ORGANIZE DATA FOR DOTPLOTS
############################
tmp <- apply(memory, 2, table) # numbers wrong and right for each question
memory.plotdata <- data.frame(question=colnames(memory),
                              percent.right=apply(memory, 2, function(x) {sum(x, na.rm=TRUE)})/nrow(memory),
                              percent.right.lower=rep(NA, ncol(memory)), percent.right.upper=rep(NA, ncol(memory)),
                              percent.right.gc=rep(NA, ncol(memory)),
                              percent.right.lower.gc=rep(NA, ncol(memory)), percent.right.upper.gc=rep(NA, ncol(memory)))
for (i in 1:ncol(tmp)) {
  memory.plotdata[i, c('percent.right.lower', 'percent.right.upper')] <- betaHPD(1+tmp[,i][2], 1+tmp[,i][1]) # 95% CIs for uncorrected data
}
memory.plotdata['cutoff', 'percent.right.gc']            <- memory.plotdata['cutoff',   'percent.right']          - (1/4)*(1 - memory.plotdata['cutoff',   'percent.right'])
memory.plotdata['policy.direction', 'percent.right.gc']  <- memory.plotdata['policy.direction', 'percent.right']  - (1/2)*(1 - memory.plotdata['policy.direction', 'percent.right'])
memory.plotdata['number.recipients', 'percent.right.gc'] <- memory.plotdata['number.recipients', 'percent.right'] - (1/4)*(1 - memory.plotdata['number.recipients', 'percent.right'])
for (i in 1:ncol(tmp)) {
  memory.plotdata[i, c('percent.right.lower.gc', 'percent.right.upper.gc')] <- betaHPD(1+nrow(memory)*memory.plotdata[i, 'percent.right.gc'], 1+nrow(memory)*(1-memory.plotdata[i, 'percent.right.gc'])) # 95% CIs for uncorrected data
}

# now reshape the data for the Dotplot() function
memory.plotdata <- data.frame(rbind(as.matrix(memory.plotdata[, 5:7]), as.matrix(memory.plotdata[, 2:4]))) 
  # Error message about rowname duplication can be ignored.  [2009 06 04]
memory.plotdata <- cbind(rep(paste(c(2,1,3), colnames(memory), sep=''), 2), 
                         memory.plotdata, 
                         c(rep('corrected', ncol(memory)), rep('uncorrected', ncol(memory))))
  # The digits at the start of the question names determine the plotting order within each panel [2008 02 26]
colnames(memory.plotdata) <- c('question', 'mean', 'lower', 'upper', 'panel')


# SET XLIM AND YLIM
xlist$limits <- c(0, .79)
ylist$limits <- c(.6, .5+(nrow(memory.plotdata)/2))



#######################
# STRIP FUNCTION
#######################
# gives names of my choosing to the strips
horiz.strip <- function(which.panel, ...) {
  if (panel.number()==1) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5)) # gp=gpar(fill=trellis.par.get("strip.background")$col[1]))    
    ltext(.5, .5, "percentages correct\n(unadjusted)", font=2, cex=striptextsize) 
  }  
  else if (panel.number()==2) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5))
    ltext(.5, .5, "percentages correct\n(guessing\u00ADcorrected)", font=2, cex=striptextsize)
  }
}  



#######################
# PANEL FUNCTION
#######################
myPanel <- function(...) {
  # gives x-axis to each panel
  trellis.par.set("clip", list(panel="on", strip="on")) 
  panel.Dotplot(...)
}



#######################
# PRINT TO SCREEN
#######################
trellis.par.set("axis.line",  list(alpha=1, col=dotplotbordercolor, lty=1, lwd=.5))
trellis.par.set("clip",       list(panel="off", strip="off")) #                          for permitting panel.axis(outside=T, ...)
trellis.par.set("dot.symbol", list(alpha=1, cex=.5, col=dotcolor, font=1, pch=16)) # black dot (not grey) to mark the mean
trellis.par.set("dot.line",   list(alpha=1, col=dotplotlinecolor, lty=1, lwd=1))
trellis.par.set("plot.line",  list(alpha=1, col=dotcolor, lty=1, lwd=1))
tmp <- trellis.par.get("layout.heights"); tmp$axis.xlab.padding <- 4.8; trellis.par.set("layout.heights", tmp)
trellis.par.set("par.xlab.text", list(alpha=1, cex=xlabsize, col='black', font=1)) # font=2 gives bold print for x label
tmp <- trellis.par.get("strip.border"); tmp$col[1] <- 'white'; trellis.par.set("strip.border", tmp)
Medicaid200811.dotplot.memory <- Dotplot(question ~ Cbind(mean, lower, upper) | panel, 
                                         data=memory.plotdata,
                                         xlab=xlab, ylab='',
                                         panel=myPanel,
                                         layout=c(2,1),             # x columns, y rows
                                         between=list(y=2, x=1.2),                   
                                         index.cond=list(c(2, 1)),  # arrange panels from top to bottom
                                         strip=horiz.strip,
                                         par.strip.text=list(lines=stripheight),
                                         scales=list(y=ylist, x=xlist))
print(Medicaid200811.dotplot.memory, panel.width=list(panelwidth, "inches"), panel.height=list(panelheight, "inches"))

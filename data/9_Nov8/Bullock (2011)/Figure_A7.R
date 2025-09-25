# Figure_A7.R
# John G. Bullock
# 2011 August 28

# Created from Medicaid_2010_05_dotplots.R

# Creates Figure A7 in the appendix to  
# 
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (3): 496-515.

load('Experiment_2.RData')
library(grid)
library(lattice)
library(Hmisc)
library(pscl)

dotcolor           <- 'black'
dotplotlinecolor   <- 'gray67'
dotplotbordercolor <- 'black'
stripbordercolor   <- 'black'
stripbgcolor       <- 'gray77' # '#e6e6e6'
PSwidth         <- 8.5
PSheight        <- 11
panelheight     <- list(0.935, 'inches')  
panelwidth      <- list(1.65, 'inches') 
stripheight     <- list(lines=2, lineheight=10)  # lineheight is space between lines
base.cex.size   <-  1.05 # standard is 1.0
striptextsize   <-  .75*base.cex.size # cex
xlabsize        <-  .80*base.cex.size # cex
ylabsize        <-  .80*base.cex.size # cex
xlist           <- list(draw=FALSE, alternating=1, labels=c(1:6), at=c(1:6), tck=c(.5,0), cex=xlabsize)
ylist <- list(draw=TRUE, alternating=c(3,3),
              labels=c('large change, Dem. legislators oppose', 'large change, no cues', 'small change, Dem. legislators oppose', 'small change, no cues'),
              at=c(4, 2, 3, 1), # at=4:1
              tck=c(0,0), col="black", cex=ylabsize, limits=c(.6, 4.5)) 


         
##################################################
# SET UP DATA FOR DOTPLOTS
##################################################
att.num           <- unclass(att)
data$PID.leanpart <- data$PID.pre 
d2                <- expand.grid(cue=c('no_cue', 'Dem_opp'), pol.size=c('small', 'large'), policy=c('expand', 'reduce'), party=c('Dem', 'Rep'))
d2$partypolicy    <- rep(c("Demlib","Demcon","Replib","Repcon"), each=4)
d2$order          <- rep(c(1, 3, 2, 4), 4)   # rep(1:4, 4)
for (i in 1:nrow(d2)) { 
  d2$lower[i] <- t.test(att.num[cue==d2$cue[i] & pol.size==d2$pol.size[i] & pol==d2$policy[i] & data$PID.leanpart==d2$party[i]])$conf.int[1]
  d2$mean[i]  <- mean(  att.num[cue==d2$cue[i] & pol.size==d2$pol.size[i] & pol==d2$policy[i] & data$PID.leanpart==d2$party[i]], na.rm=TRUE)
  d2$upper[i] <- t.test(att.num[cue==d2$cue[i] & pol.size==d2$pol.size[i] & pol==d2$policy[i] & data$PID.leanpart==d2$party[i]])$conf.int[2]
}



##################################################
# STRIP AND PANEL FUNCTIONS
##################################################
horiz.strip <- function(...) {
  if (panel.number()==1) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5)) # gp=gpar(fill=trellis.par.get("strip.background")$col[1]))    
    striptext <- paste("Dem. subjects (N=", sum(Dem & reduce), ")\nconservative policy", sep='') 
    ltext(.5, .5, striptext, font=2, cex=striptextsize, gp=gpar(lineheight=.4)) 
  }  
  else if (panel.number()==3) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5))
    striptext <- paste("Dem. subjects (N=", sum(Dem & expand), ")\nliberal policy", sep='') 
    ltext(.5, .5, striptext, font=2, cex=striptextsize) 
  }
  else if (panel.number()==2) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5))
    striptext <- paste("GOP subjects (N=", sum(Rep & reduce), ")\nconservative policy", sep='')
    ltext(.5, .5, striptext, font=2, cex=striptextsize) 
  }
  else if (panel.number()==4) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5))
    striptext <- paste("GOP subjects (N=", sum(Rep & expand), ")\nliberal policy", sep='')
    ltext(.5, .5, striptext, font=2, cex=striptextsize)
  }
}  

myPanel <- function(...) {
  trellis.par.set("clip", list(panel="on", strip="on")) 
  panel.Dotplot(...)
  trellis.par.set("clip", list(panel="off", strip="off")) # for permitting panel.axis(outside=T, ...)
  trellis.par.set("axis.text", list(cex=xlabsize))        # set size of axis text
  panel.axis(side="bottom", outside=TRUE, at=1:6, label=c('1', '2','3','4','5', '6'), tck=.5, rot=c(0, 0))
}



##################################################
# PRINT TO SCREEN
##################################################
# Set xlim so that that there is equal space on either side of the extreme margins.  
tmp1 <- max(d2$upper) - floor(max(d2$upper))
tmp2 <- ceiling(min(d2$lower)) - min(d2$lower)
tmp <- max(tmp1, tmp2)+.1
xlim <- c(ceiling(min(d2$lower))-tmp, floor(max(d2$upper))+tmp) 

trellis.par.set("axis.line",  list(alpha=1, col=dotplotbordercolor, lty=1, lwd=.5))
trellis.par.set("clip",       list(panel="off", strip="off"))                                    # for permitting panel.axis(outside=T, ...)
trellis.par.set("dot.symbol", list(alpha=1, cex=.5*base.cex.size, col=dotcolor, font=1, pch=16)) # black dot (not grey) to mark the mean
trellis.par.set("dot.line",   list(alpha=1, col=dotplotlinecolor, lty=1, lwd=1))
trellis.par.set("plot.line",  list(alpha=1, col=dotcolor,         lty=1, lwd=.65))                # CI line
tmp <- trellis.par.get("strip.border"); tmp$col[1] <- 'white'; trellis.par.set("strip.border", tmp)
Medicaid.2010.05.dotplot <- Dotplot(order ~ Cbind(mean, lower, upper) | partypolicy, 
                                    data=d2,
                                    drop.unused.levels=TRUE,
                                    xlab="", 
                                    ylab="",
                                    xlim=xlim,
                                    panel=myPanel,
                                    layout=c(2,2),                    # 1 columns, 3 rows
                                    between=list(y=3.0, x=1.15),                   
                                    index.cond=list(c(1,3,2,4)),      # Arrange panels.  First in the list is in lower left.  Second is in upper left.
                                    strip=horiz.strip,
                                    par.strip.text=stripheight,
                                    scales=list(y=ylist, x=xlist))
print(Medicaid.2010.05.dotplot, panel.width=panelwidth, panel.height=panelheight)

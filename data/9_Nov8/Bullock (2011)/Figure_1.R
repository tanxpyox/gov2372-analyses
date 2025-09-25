# Figure_1.R
# John G. Bullock
# 2011 August 28

# Created from Medicaid_2008_11_dotplots.R

# Creates Figure 1 in  
# 
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (3): 496-515.

meanNA <- function (x) { return(mean(x, na.rm = TRUE)) }
library(car)  # for recode()
library(grid)
library(lattice)
library(Hmisc)
library(Design)
load('Experiment_1.RData')

postscript.bg      <- 'transparent'
dotcolor           <- 'black'
dotplotlinecolor   <- 'gray67'
dotplotbordercolor <- 'black'
stripbordercolor   <- 'black'
stripbgcolor       <- 'gray77' # '#e6e6e6'
panelheight        <-  .60
panelwidth         <- 1.68 # inches
stripheight        <- list(lines=2.25, lineheight=10) # lineheight is space between lines

base.cex.size   <- 1.00
striptextsize   <-  .80*base.cex.size # cex
xlabsize        <-  .80*base.cex.size # cex
ylabsize        <-  .80*base.cex.size # cex
xlist           <- list(draw=FALSE, alternating=c(1,1), labels=c(2:5), at=c(2:5), limits=c(1.5, 5.5), tck=c(.5,0), cex=xlabsize)
ylist           <- list(draw=TRUE, alternating=c(3,3), 
                        labels=c('Dem. legislators oppose', 'no cues', 'Dem. legislators support'), 
                        at=c(3,2,1), tck=c(0,0), col="black", cex=ylabsize, limits=c(.6, 3.5))

                    
# ALIASES
tr         <- data$article.type
tr.con     <- gregexpr('con', tr)>-1
tr.lib     <- !tr.con
tr.nocue   <- tr%in%c("con_nocue", "lib_nocue")
Demsupp    <- gregexpr('Demsupp', tr)>-1
Demopp     <- gregexpr('Demopp', tr)>-1
att        <- data$initial.like
att.num    <- unclass(att)
dis        <- att<='disapprove slightly'
app        <- att>='approve slightly'
PID        <- data$PID.pre 
Dem        <- PID=="Dem"
Rep        <- PID=="Rep"


# DATA FOR DOTPLOTS
d2             <- expand.grid(condition=c('Dems support', 'no cues', 'Dems oppose'), policy=c("liberal", "conservative"), party=c("Dem","Rep"))
d2$partypolicy <- rep(c("Demlib","Demcon","Replib","Repcon"), each=3)
d2$mean    <- c(meanNA(att.num[Dem & tr.lib & Demsupp]), meanNA(att.num[Dem & tr.lib & tr.nocue]), meanNA(att.num[Dem & tr.lib & Demopp]),
                meanNA(att.num[Dem & tr.con & Demsupp]), meanNA(att.num[Dem & tr.con & tr.nocue]), meanNA(att.num[Dem & tr.con & Demopp]),
                meanNA(att.num[Rep & tr.lib & Demsupp]), meanNA(att.num[Rep & tr.lib & tr.nocue]), meanNA(att.num[Rep & tr.lib & Demopp]),
                meanNA(att.num[Rep & tr.con & Demsupp]), meanNA(att.num[Rep & tr.con & tr.nocue]), meanNA(att.num[Rep & tr.con & Demopp]))
CIs <- matrix(c(t.test(att.num[Dem & tr.lib & Demsupp])$conf.int, t.test(att.num[Dem & tr.lib & tr.nocue])$conf.int, t.test(att.num[Dem & tr.lib & Demopp])$conf.int,
                t.test(att.num[Dem & tr.con & Demsupp])$conf.int, t.test(att.num[Dem & tr.con & tr.nocue])$conf.int, t.test(att.num[Dem & tr.con & Demopp])$conf.int,
                t.test(att.num[Rep & tr.lib & Demsupp])$conf.int, t.test(att.num[Rep & tr.lib & tr.nocue])$conf.int, t.test(att.num[Rep & tr.lib & Demopp])$conf.int,
                t.test(att.num[Rep & tr.con & Demsupp])$conf.int, t.test(att.num[Rep & tr.con & tr.nocue])$conf.int, t.test(att.num[Rep & tr.con & Demopp])$conf.int),
              ncol=2, byrow=T)
d2$lower <- CIs[,1]
d2$upper <- CIs[,2]


# STRIP FUNCTION
horiz.strip <- function(...) {
  if (panel.number()==1) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5)) # gp=gpar(fill=trellis.par.get("strip.background")$col[1]))
    striptext <- paste("Dem. subjects (N\u2006=\u2006", sum(Dem & tr.con), ")\nconservative policy", sep='')
    ltext(.5, .5, striptext, font=2, cex=striptextsize, gp=gpar(lineheight=.4))
  }  
  else if (panel.number()==3) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5))
    striptext <- paste("Dem. subjects (N\u2006=\u2006", sum(Dem & tr.lib), ")\nliberal policy", sep='') 
    ltext(.5, .5, striptext, font=2, cex=striptextsize)
  }
  else if (panel.number()==2) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5))
    striptext <- paste("GOP subjects (N\u2006=\u2006", sum(Rep & tr.con), ")\nconservative policy", sep='')
    ltext(.5, .5, striptext, font=2, cex=striptextsize)
  }
  else if (panel.number()==4) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5))
    striptext <- paste("GOP subjects (N\u2006=\u2006", sum(Rep & tr.lib), ")\nliberal policy", sep='')
    ltext(.5, .5, striptext, font=2, cex=striptextsize)
  }
}  


# PANEL FUNCTION
myPanel <- function(...) {
  # gives x-axis to each panel
  trellis.par.set("clip", list(panel="on", strip="on")) 
  panel.Dotplot(...)
  trellis.par.set("clip", list(panel="off", strip="off")) # for permitting panel.axis(outside=TRUE, ...)
  trellis.par.set("axis.text", list(cex=xlabsize)) # set size of axis text
  panel.axis(side="bottom", outside=TRUE, at=2:5, label=c('2','3','4','5'), tck=.5, rot=c(0, 0))
}


# PRINT TO SCREEN
trellis.par.set("axis.line",  list(alpha=1, col=dotplotbordercolor, lty=1, lwd=.5))
trellis.par.set("clip",       list(panel="off", strip="off"))                      # for permitting panel.axis(outside=T, ...)
trellis.par.set("dot.symbol", list(alpha=1, cex=.5, col=dotcolor, font=1, pch=16)) # black dot (not grey) to mark the mean
trellis.par.set("dot.line",   list(alpha=1, col=dotplotlinecolor, lty=1, lwd=1))
trellis.par.set("plot.line",  list(alpha=1, col=dotcolor, lty=1, lwd=1))
tmp <- trellis.par.get("strip.border"); tmp$col[1] <- 'white'; trellis.par.set("strip.border", tmp)
omni1.dotplot <- Dotplot(condition ~ Cbind(mean, lower, upper) | partypolicy, 
                         data=d2,
                         drop.unused.levels=TRUE,
                         xlab="", ylab="",
                         panel=myPanel,
                         layout=c(2,2),     
                         between=list(y=2.7, x=.9),                   
                         index.cond=list(c(1,3,2,4)),  # Arrange panels.  First in the list is in lower left.  Second is in upper left.
                         strip=horiz.strip,
                         par.strip.text=stripheight,
                         scales=list(y=ylist, x=xlist))
print(omni1.dotplot, panel.width=list(panelwidth, "inches"), panel.height=list(panelheight, "inches"))

# Figure_A9.R
# John G. Bullock
# 2011 September 11

# Created from Medicaid_2010_05_Republican_approval_by_age.R.

# Creates Figure A10 in  
# 
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (August): 
#    496-515.

library(grid)
library(lattice)
library(Hmisc)
library(pscl)
load('Experiment_1.RData')
meanNA <- function (x) { return(mean(x, na.rm=TRUE)) }
sumNA  <- function (x) { return(sum(x, na.rm=TRUE)) } 


################################################################################
# SET UP FIGURE PARAMETERS
################################################################################
dotcolor           <- 'black'
dotplotlinecolor   <- 'transparent' # need to do this manually
dotplotlinelwd     <-  .5 
dotplotgridcolor   <- 'gray67'      # for manual plotting of horizontal bg lines
dotplotgridlwd     <- 1             # for manual plotting of horizontal bg lines
dotplotbordercolor <- 'black'
stripbordercolor   <- 'black'
stripbgcolor       <- 'gray77' # '#e6e6e6'
PSwidth            <- 7.25
PSheight           <- 4.05
panelcol.panelrow  <- c(2, 1)
between.panels     <- list(y=2, x=.9)
panelheight        <- list(1.10, 'inches')
panelwidth         <- list(1.78, 'inches')
panelheight.2      <- list(1.38, 'inches')
panelwidth.2       <- list(panelwidth[[1]]*2, 'inches')
base.cex.size      <- 1.00
striptextsize      <-  .80*base.cex.size 
xaxissize           <- .75*base.cex.size 
yaxissize           <- .80*base.cex.size 
xlabsize           <-  .80*base.cex.size 
ylabsize           <-  .80*base.cex.size 
stripheight        <- list(lines=1, lineheight=10) # lineheight = space btw. lines
xlist              <- list(draw=FALSE, alternating=c(1,1), 
                           labels=c(2:5), at=c(2:5), 
                           tck=c(.5,0), cex=xlabsize)
# ylist limits and at set below
ylist              <- list(draw=TRUE, alternating=c(3,3), 
                           labels=c('Dem. legislators oppose', 'no cues', 
                                    'Dem. legislators support'), 
                           tck=c(0,0), col="black", cex=ylabsize) 
ylist.2            <- list(draw=TRUE, alternating=c(3,3), 
                           labels=c('Dem. legislators oppose', 'no cues', 
                                    'Dem. legislators support', 'overall'),                            
                           tck=c(0,0), col='black', cex=ylabsize)
                            
                                              
###############################################################################                       
# ALIASES
###############################################################################
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
age.low <- data$age<=30 & data$age.legit
age.hi  <- data$age> 55 & data$age.legit


###############################################################################                       
# DATA FOR DOTPLOTS
###############################################################################
# There are two plots.  The first, a two-panel plot, shows Republican approval 
# by age of the liberal policy (panel 1) and the conservative policy (panel 2).
# The second, a one-panel plot, shows (liberal policy rating - conservative
# policy rating) by age.  We start by creating the data frame for the first 
# plot.  [2011 03 04]

# BASIC MEANS PLOT [2011 03 04]
Rep.age.data <- expand.grid(condition=c('Dems support', 'no cues', 'Dems oppose'), 
                            policy=c('liberal', 'conservative'), 
                            age=c('low', 'high'))
Rep.age.means <- function (age) {
  c(meanNA(att.num[Rep & tr.lib & Demsupp  & age]), 
    meanNA(att.num[Rep & tr.lib & tr.nocue & age]), 
    meanNA(att.num[Rep & tr.lib & Demopp   & age]),
    meanNA(att.num[Rep & tr.con & Demsupp  & age]), 
    meanNA(att.num[Rep & tr.con & tr.nocue & age]), 
    meanNA(att.num[Rep & tr.con & Demopp   & age]))
}
Rep.age.data$mean <- c(Rep.age.means(age.low), Rep.age.means(age.hi))              
CIs <- matrix(c(t.test(att.num[Rep & tr.lib & Demsupp  & age.low])$conf.int, 
                t.test(att.num[Rep & tr.lib & tr.nocue & age.low])$conf.int, 
                t.test(att.num[Rep & tr.lib & Demopp   & age.low])$conf.int,
                t.test(att.num[Rep & tr.con & Demsupp  & age.low])$conf.int, 
                t.test(att.num[Rep & tr.con & tr.nocue & age.low])$conf.int, 
                t.test(att.num[Rep & tr.con & Demopp   & age.low])$conf.int,
                t.test(att.num[Rep & tr.lib & Demsupp  & age.hi])$conf.int, 
                t.test(att.num[Rep & tr.lib & tr.nocue & age.hi])$conf.int, 
                t.test(att.num[Rep & tr.lib & Demopp   & age.hi])$conf.int,
                t.test(att.num[Rep & tr.con & Demsupp  & age.hi])$conf.int, 
                t.test(att.num[Rep & tr.con & tr.nocue & age.hi])$conf.int, 
                t.test(att.num[Rep & tr.con & Demopp   & age.hi])$conf.int),
              ncol=2, byrow=TRUE)
Rep.age.data$lower <- CIs[,1]
Rep.age.data$upper <- CIs[,2]
Rep.age.data$y <- c(rep(seq(1.3, by=1.3, length=3), 2),
                    rep(seq(1.0, by=1.3, length=3), 2)) 
ylist$limits <- c(.4, max(Rep.age.data$y)+.6)                              
ylist$at     <- Rep.age.data$y[12:10]


# DIFFERENCES PLOT [2011 03 04]
Rep.age.diffs.data <- expand.grid(condition=c('overall', 'Dems support',  
                                              'no cues', 'Dems oppose'), 
                                  age=c('low', 'high'))
Rep.age.diff.means <- function (age) {
  c(meanNA(att.num[Rep & tr.lib & age]) - 
    meanNA(att.num[Rep & tr.con & age]),
    meanNA(att.num[Rep & tr.lib & Demsupp  & age]) - 
    meanNA(att.num[Rep & tr.con & Demsupp  & age]), 
    meanNA(att.num[Rep & tr.lib & tr.nocue & age]) -  
    meanNA(att.num[Rep & tr.con & tr.nocue & age]), 
    meanNA(att.num[Rep & tr.lib & Demopp   & age]) - 
    meanNA(att.num[Rep & tr.con & Demopp   & age]))
}
Rep.age.diffs.data$mean <- c(Rep.age.diff.means(age.low), Rep.age.diff.means(age.hi))              
Rep.age.diff.CIs <- function (age) {
  matrix(c(t.test(att.num[Rep & tr.lib & age], 
                  att.num[Rep & tr.con & age])$conf.int,
           t.test(att.num[Rep & tr.lib & Demsupp  & age], 
                  att.num[Rep & tr.con & Demsupp  & age])$conf.int,
           t.test(att.num[Rep & tr.lib & tr.nocue & age], 
                  att.num[Rep & tr.con & tr.nocue & age])$conf.int,
           t.test(att.num[Rep & tr.lib & Demopp   & age], 
                  att.num[Rep & tr.con & Demopp   & age])$conf.int), 
         ncol=2, byrow=TRUE)            
}
Rep.age.diffs.data$lower <- rbind(Rep.age.diff.CIs(age.low), 
                                  Rep.age.diff.CIs(age.hi))[,1]
Rep.age.diffs.data$upper <- rbind(Rep.age.diff.CIs(age.low), 
                                  Rep.age.diff.CIs(age.hi))[,2]
Rep.age.diffs.data$y <- c(seq(1.3, by=1.3, length=4), seq(1, by=1.3, length=4))
ylist.2$limits <- c(.4, max(Rep.age.diffs.data$y)+.6)                              
ylist.2$at     <- Rep.age.diffs.data$y[8:5]


###############################################################################                       
# STRIP FUNCTIONS
###############################################################################
striptext <- c(paste('liberal policy (N\u2006=\u2006',      
                     sum(Rep & tr.lib & (age.low | age.hi)), ')', sep=''),
               paste('conservative policy (N\u2006=\u2006', 
                     sum(Rep & tr.con & (age.low | age.hi)), ')', sep=''))
horiz.strip <- function(...) {
  grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), 
               gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5))     
  ltext(.5, .5, striptext[panel.number()], font=2, cex=striptextsize)
}  
horiz.strip.2 <- function(...) {
  grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), 
               gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5))     
  ltext(.5, .5, paste('liberal policy \u2212 conservative policy (N\u2006=\u2006', 
                      sum(Rep & (age.low | age.hi)), ')', sep=''), 
        font=2, cex=striptextsize)
}  


###############################################################################                       
# PANEL FUNCTION
###############################################################################                       
myPanel <- function(...) {

  # CREATE BACKGROUND GRID OF GREY LINE FOR EACH PANEL [2011 01 04]
  trellis.par.set("clip", list(panel="on", strip="on")) 
  panel.abline(h=ylist$at, col=dotplotgridcolor, lwd=dotplotgridlwd)
  
  # FINISH THE PANEL
  panel.Dotplot(...)
  trellis.par.set("clip", list(panel="off", strip="off")) 
  trellis.par.set("axis.text", list(cex=xaxissize))
  panel.axis(side="bottom", outside=TRUE, at=2:5, label=c('2','3','4','5'), 
             tck=.5, rot=c(0, 0))
} 
myPanel.2 <- function(...) {

  # CREATE BACKGROUND GRID OF GREY LINE FOR EACH PANEL [2011 01 04]
  trellis.par.set("clip", list(panel="on", strip="on")) 
  panel.abline(h=ylist.2$at, col=dotplotgridcolor, lwd=dotplotgridlwd)
  
  # FINISH THE PANEL
  panel.Dotplot(...)
  trellis.par.set("clip", list(panel="off", strip="off")) 
  trellis.par.set("axis.text", list(cex=xaxissize))
  panel.axis(side="bottom", outside=TRUE, at=seq(-1.5, 1.5, by=.5), 
             label=c('-1.5', '-1', '-.5', '0', '.5', '1', '1.5'), 
             tck=.5, rot=c(0, 0))    
} 


###############################################################################
# PRINT THE FIGURE
###############################################################################
trellis.par.set("axis.line",  
                list(alpha=1, col=dotplotbordercolor, lty=1, lwd=.5))
trellis.par.set("clip",       list(panel="off", strip="off")) 
trellis.par.set("dot.symbol", 
                list(alpha=1, cex=.5, col=dotcolor, font=1, pch=16)) # black dot for mean 
trellis.par.set("dot.line",   
                list(alpha=1, col=dotplotlinecolor, lty=1, lwd=dotplotlinelwd))
trellis.par.set("plot.line",  
                list(alpha=1, col=dotcolor, lty=1, lwd=dotplotgridlwd))
trellis.par.set("superpose.line",   
                list(alpha=1, col=dotcolor, lty=1, lwd=dotplotlinelwd))
tmp <- trellis.par.get("strip.border")
tmp$col[1] <- 'white' 
trellis.par.set("strip.border", tmp)
Med2008.11.Rep.approval.by.age <- Dotplot(y ~ Cbind(mean, lower, upper) | policy, 
                                          group=age,
                                          data=Rep.age.data,
                                          drop.unused.levels=TRUE,
                                          xlab='', ylab='',
                                          panel=myPanel,
                                          layout=panelcol.panelrow,
                                          #index.cond=list(c(2,1)), # order of panels
                                          between=between.panels,                 
                                          strip=horiz.strip,
                                          par.strip.text=stripheight,
                                          scales=list(y=ylist, x=xlist),
                                          pch=c(1,16))
Med2008.11.Rep.approval.diffs <- Dotplot(y ~ Cbind(mean, lower, upper) | TRUE, 
                                         group=age,
                                         data=Rep.age.diffs.data,
                                         drop.unused.levels=TRUE,
                                         xlim=c(-1.7, 1.7), 
                                         xlab='', ylab='',
                                         panel=myPanel.2,
                                         layout=c(1,1),
                                         strip=horiz.strip.2, # it's in myPanel.2
                                         par.strip.text=stripheight,
                                         scales=list(y=ylist.2, x=xlist),
                                         pch=c(1,16))
print(Med2008.11.Rep.approval.by.age, 
      position=c(0, .65, 1, 1),
      panel.width=panelwidth, panel.height=panelheight,
      more=TRUE)
print(Med2008.11.Rep.approval.diffs,
      position=c(.075, 0, .925, .55), 
      panel.height=panelheight.2)



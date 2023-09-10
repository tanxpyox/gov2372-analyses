# Figure_3.R
# John G. Bullock
# 2011 August 28

# Created from Medicaid_2010_05_inhibition_dotplots.R

# Creates Figure 4 in   
# 
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (3): 496-515.

load('Experiment_2.RData')
source('Medicaid_2010_05_average_effects.R') # source code for computing average effects and bootstrapped CIs.  
library(boot)
library(grid)
library(lattice)
library(Hmisc)
library(pscl)

dotcolor           <- 'black'
dotplotlinecolor   <- 'gray67' # better than white [2010 06 21]
dotplotbordercolor <- 'black'
stripbordercolor   <- 'black'
stripbgcolor       <- 'gray77' # prints right on Nora's printer.  Best compromise between screen and print appearance.
PDFwidth        <- 10
PDFheight       <- 10
panelheight     <- 2.60 # inches
panelwidth      <- 2.11 # inches
betweenlist     <- list(y=3.5, x=1)
base.cex.size   <- 1.05 # standard is 1.0
striptextsize   <-  .75*base.cex.size # cex
xaxissize       <-  .75*base.cex.size # cex
yaxissize       <- xaxissize*1.05 
xlabsize        <-  .825*base.cex.size # cex
ylabsize        <-  .825*base.cex.size # cex
xlabcolor       <- 'black'
ylabcolor       <- xlabcolor  
stripheight     <- 2.075
xlabtext        <- c('Mean Number of Thoughts', 'Thought\u00ADAttitude Correlation', 'Mean Policy Facts Recalled', 'Mean Seconds Spent on Article')  
xlist           <- list(draw=TRUE, alternating=FALSE, 
                        at=list(c(2.5, 3, 3.5, 4, 4.5),
                                seq(.46, .70, by=.06),
                                seq(  1,   2, by=.25),
                                seq(150, 250, by=25)),                                     
                        labels=list(c(2.5, 3, 3.5, 4, 4.5),
                                    c('.46', '.52', '.58', '.64', '.70'),
                                    seq(  1,   2, by=.25),
                                    seq(150, 250, by=25)),
                        limits=list(c(2.20, 4.80),
                                    c( .42,  .74),
                                    c( .90, 2.10),
                                    c(140, 260)),  
                        tck=c(.5,0), cex=xaxissize, relation='free', axs='i')
ylist           <- list(draw=TRUE,  alternating=c(1,1),
                        labels=c('all subjects, cues', 'all subjects, no cues', 'large\u00ADchange condition, cues', 'large\u00ADchange condition, no cues',
                                 'small\u00ADchange condition, cues', 'small\u00ADchange condition, no cues', 'Democrats, cues', 'Democrats, no cues', 'Republicans, cues', 'Republicans, no cues'),                                   
                        tck=c(0,0), col='black', cex=yaxissize)  


                    
###################################
# ALIASES
###################################
tr   <- data$article.type
cued <- data$cue.condition!='no_cue'



###################################
# DATA FOR DOTPLOTS
###################################
data.to.plot <- expand.grid(cued=c(TRUE, FALSE), category=c('all', 'large change', 'small change', 'Dem', 'Rep'), panel=4:1)
data.to.plot$upper <- data.to.plot$mean <- data.to.plot$lower <- NA
tmpfunc <- function (var.to.test, subset.var) {  
  tmp.cued   <- t.test(var.to.test[subset.var &  cued])
  tmp.uncued <- t.test(var.to.test[subset.var & !cued])
  rbind(c(as.numeric(tmp.cued$estimate),   tmp.cued$conf.int),
        c(as.numeric(tmp.uncued$estimate), tmp.uncued$conf.int))
}

# AVERAGE RECALL
correct <- apply(memory[, c('cutoff', 'policy.direction', 'number.recipients')], 1, sum)
data.to.plot[data.to.plot$panel==3, c('mean', 'lower', 'upper')] <- rbind(tmpfunc(correct, TRUE), tmpfunc(correct, pol.size.large), tmpfunc(correct, pol.size.small), tmpfunc(correct, Dem), tmpfunc(correct, Rep))

# AVERAGE TIME SPENT ON ARTICLE
tmpfunc.time <- function (var.to.test, subset.var, trimlevel=.005) {  
  var.cued   <- var.to.test[subset.var &  cued]
  var.cued   <- var.cued[var.cued>quantile(var.cued, trimlevel) & var.cued<quantile(var.cued, 1-trimlevel)]
  var.uncued <- var.to.test[subset.var &  !cued]
  var.uncued <- var.uncued[var.uncued>quantile(var.uncued, trimlevel) & var.uncued<quantile(var.uncued, 1-trimlevel)]  
  tmp.cued   <- t.test(var.cued)
  tmp.uncued <- t.test(var.uncued)
  rbind(c(as.numeric(tmp.cued$estimate),   tmp.cued$conf.int),
        c(as.numeric(tmp.uncued$estimate), tmp.uncued$conf.int))
}
data.to.plot[data.to.plot$panel==4, c('mean', 'lower', 'upper')] <- rbind(tmpfunc.time(data$time.on.article, TRUE), tmpfunc.time(data$time.on.article, pol.size.large), tmpfunc.time(data$time.on.article, pol.size.small), tmpfunc.time(data$time.on.article, Dem), tmpfunc.time(data$time.on.article, Rep))

# AVERAGE THOUGHTS IN DIFFERENT CONDITIONS
data.to.plot[data.to.plot$panel==1, c('mean', 'lower', 'upper')] <- rbind(tmpfunc(thoughts.averaged, TRUE), tmpfunc(thoughts.averaged, pol.size.large), tmpfunc(thoughts.averaged, pol.size.small), tmpfunc(thoughts.averaged, Dem), tmpfunc(thoughts.averaged, Rep))

# THOUGHT-ATTITUDE CORRELATIONS
tmpfunc.cor <- function(subset.var=TRUE, iterations=1000) {
  boot.data <- data.frame(att=unclass(att), thoughts.positivity, cued)[subset.var,]
  boot.cor  <- function(data, indices) { 
    data  <- data[indices, ]
    cor.cued   <- cor(data$thoughts.positivity[ data$cued], data$att[ data$cued], use='p')
    cor.uncued <- cor(data$thoughts.positivity[!data$cued], data$att[!data$cued], use='p')
    c(cor.cued, cor.uncued)
  }
  bootresult <- boot(data=boot.data, statistic=boot.cor, R=iterations)
  means  <- bootresult$t0
  lowers <- bootresult$t0 - 1.96*apply(bootresult$t, 2, sd) 
  uppers <- bootresult$t0 + 1.96*apply(bootresult$t, 2, sd)
  cbind(means, lowers, uppers)
}
data.to.plot[data.to.plot$panel==2, c('mean', 'lower', 'upper')] <- rbind(tmpfunc.cor(TRUE), tmpfunc.cor(pol.size.large), tmpfunc.cor(pol.size.small), tmpfunc.cor(Dem), tmpfunc.cor(Rep))

# ORDER THE DATA WITHIN EACH PANEL
data.to.plot$order <- rep(c(14, 13, 11, 10, 8, 7, 5, 4, 2, 1), 2) 
ylist$at <- rev(sort(data.to.plot$order[data.to.plot$panel==1]))

# SET YLIM
ylist$limits <- c(.6, .4+max(data.to.plot$order))



###################################
# STRIP FUNCTION
###################################
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



###################################
# PANEL FUNCTION
###################################
myPanel <- function(...) {
  # gives x-axis to each panel
  trellis.par.set("clip", list(panel="on", strip="on")) 
  panel.Dotplot(...)
  trellis.par.set("clip", list(panel="off", strip="off")) # for permitting panel.axis(outside=T, ...)
  
  # PANEL NUMBERS AND LABELS
  panel.numbers.text <- c(3, 4, 1, 2)
  grid.polygon(x=c(.875, .9975, .9975, .875, .875), y=c(.9, .9, .9975, .9975, .9), gp=gpar(fill=dotplotlinecolor, col=dotplotlinecolor)) #  grey background for panel number  
  grid.text(panel.numbers.text[panel.number()], x=.94, y= .950, just='center', gp=gpar(font=2, cex=1.15*xlabsize, col='black'))
  grid.text(xlabtext[panel.number()]          , x=.50, y=-.170, just='center', gp=gpar(font=2, cex=    xlabsize, col=xlabcolor))
  
}



###################################
# PRINT FIGURE TO SCREEN
###################################
trellis.par.set("axis.line",  list(alpha=1, col=dotplotbordercolor, lty=1, lwd=.5))
trellis.par.set("clip",       list(panel="off", strip="off"))                      # for permitting panel.axis(outside=T, ...)
trellis.par.set("dot.symbol", list(alpha=1, cex=.5, col=dotcolor, font=1, pch=16)) # black dot (not grey) to mark the mean
trellis.par.set("dot.line",   list(alpha=1, col=dotplotlinecolor, lty=1, lwd=1))   # background line
trellis.par.set("plot.line",  list(alpha=1, col=dotcolor, lty=1, lwd=.75))          # CI line
trellis.par.set("par.xlab.text", list(alpha=1, cex=xlabsize, col='black', font=2)) # font=2 gives bold print for x label
tmp <- trellis.par.get("strip.border"); tmp$col[1] <- 'white'; trellis.par.set("strip.border", tmp)
Medicaid201005.dotplots.inhibition <- Dotplot(order ~ Cbind(mean, lower, upper) | panel , data=data.to.plot
                   , xlab='', ylab=''
                   , panel=myPanel
                   , layout=c(2,2)            # x columns, y rows
                   , between=betweenlist                   
                   , strip=FALSE
                   , scales=list(y=ylist, x=xlist)
                   )
print(Medicaid201005.dotplots.inhibition, panel.width=list(panelwidth, "inches"), panel.height=list(panelheight, "inches"))

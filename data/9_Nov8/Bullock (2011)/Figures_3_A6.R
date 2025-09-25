# Figures_3_A6.R
# John G. Bullock
# 2011 August 28

# Created from Medicaid_2010_05_overall_effects_dotplot.R

# Creates Figures 3 and A6 in   
# 
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (3): 496-515.
#
# If EDUC.COMPARE is set to FALSE  on line 28, this code creates Figure 3.  
# If EDUC.COMPARE is set to TRUE   on line 28, this code creates Figure A6.

rm(list=ls())  # necessary if overall.effects() is going to work as intended                           
old.search <- search()
packages.to.detach <- c('Design', 'Hmisc')
for (i in packages.to.detach) { 
  if (any(grepl(i, search()))) { do.call(detach, list(paste('package:', i, sep=''))) } 
}
load('Experiment_2.RData')
source('Medicaid_2010_05_average_effects.R') #  code for computing average effects and bootstrapped CIs
library(grid)
library(lattice)
library(Hmisc)
library(pscl)

EDUC.COMPARE <- FALSE   # if FALSE, panels vary by partisanship
if (EDUC.COMPARE) { 
  AGE.COMPARE <- TRUE 
} else { 
  AGE.COMPARE <- FALSE 
} 

RIGHT.STRIP        <- !EDUC.COMPARE  # include panel strips on right-hand sides of each row?
dotcolor           <- 'black'
dotplotlinecolor   <- 'gray67'
dotplotbordercolor <- 'black'
stripbordercolor   <- 'black'
stripbgcolor       <- 'gray77' 
if (EDUC.COMPARE) { 
  if (AGE.COMPARE) { 
            layout <- c(3,2) } else { 
            layout <- c(3,1) } 
  } else {  layout <- c(3,3) } 
panelnumbercol     <- grey(.2) # color of number of each panel; lower values are darker
PDFwidth           <- 8.5
PDFheight          <- 11.0
panelheight        <- 1.215
if (RIGHT.STRIP) { panelwidth  <- 1.13 } else { 
  if (EDUC.COMPARE) { panelwidth <- 1.275 } else { panelwidth  <- 1.475 } # inches
}
if (RIGHT.STRIP) { stripheight <- 1.075 } else { stripheight <- 2.075 }
if (RIGHT.STRIP) { parstriptext <- list(lines=c(0,0,stripheight)) } else { parstriptext <- list(lines=stripheight) }
striptextsize      <-  .78 # cex
if (EDUC.COMPARE) { xlim <- c(0, 2) } else { xlim <- c(0, 3) }
xlab               <- 'Average Attitude Differences'
xlabsize           <-  .94  # cex
if (EDUC.COMPARE & AGE.COMPARE) { xlab.padding <- 4.85 } else { xlab.padding <- 1.8 }
xaxissize          <-  .8 
ylabsize           <-  .8  # cex
if (RIGHT.STRIP)  { xbetween <- .50 } else { xbetween <-  .25 }
if (RIGHT.STRIP)  { ybetween <- .50 } else { 
  if (EDUC.COMPARE & AGE.COMPARE) { ybetween <- 2.90 } else { ybetween <- 2.00 }
}
if (RIGHT.STRIP)  { xlist.alternating <- c(1, 1, 1) } else { xlist.alternating <- c(1, 1, 1) } 
if (EDUC.COMPARE) { xlist.labels      <- c('.25', '1', '1.75') } else { xlist.labels <- c('.5', '1.5', '2.5') }  
xlist              <- list(draw=TRUE,
                           alternating=xlist.alternating,
                           labels=xlist.labels, tck=c(.5,0), cex=xaxissize, relation='same', axs='r') # , axs='i'  
if (EDUC.COMPARE) { xlist$axs <- 'i'}
xlist$at <- as.numeric(xlist$labels)
if (EDUC.COMPARE & AGE.COMPARE) { xlist$draw=FALSE }
ylist.labels       <- c('Dem. legislators oppose vs. no cues', '1.55 \u00D7 (Dem. legislators oppose vs. no cues)', 'subjects\u2019 party ID: Dem. vs. Republican', 'policy: liberal vs. conservative')  
ylist              <- list(draw=TRUE, alternating=1,
                           labels=ylist.labels,                             
                           at=length(ylist.labels):1, tck=c(0,0), col='black', cex=ylabsize, limits=c(.45, length(ylist.labels)+.65))  



################################
# DATA FOR DOTPLOTS
################################
# Calculations are in Medicaid_2010_05_average_effects.R [2010 05 17]
age.lo <- data$age<=30
age.hi <- data$age>=55
#soph.low <- know.gen.tri=='low'
#soph.hi  <- know.gen.tri=='high' 
educ.low <- data$education<='12th'
if (!(EDUC.COMPARE)) {
  overall.fx.data       <- data.frame(order=rep(4:1, 3), panel=rep(7:9, each=4), 
                                      average.effects())
  overall.fx.large.data <- data.frame(order=rep(4:1, 3), panel=rep(4:6, each=4), 
                                      average.effects(subset.var=pol.size.large))
  overall.fx.small.data <- data.frame(order=rep(4:1, 3), panel=rep(1:3, each=4), 
                                      average.effects(subset.var=pol.size.small))                             
}
if (EDUC.COMPARE) {
  overall.fx.data     <- data.frame(order=rep(4:1, 3), panel=rep(1:3, each=4), average.effects(Dem.subset.var=educ.low, Rep.subset.var=!educ.low, iter=1000))
  if (AGE.COMPARE) { overall.fx.age.data <- data.frame(order=rep(4:1, 3), panel=rep(4:6, each=4), average.effects(Dem.subset.var=age.lo, Rep.subset.var=age.hi, iter=1000)) }
}

# Rename the column headings for the data frames.
tmp        <- ls(pat='overall.*data')
tmpobjects <- lapply(tmp, get) 
tmpobjects <- lapply(tmpobjects, function (x) { colnames(x)[3:5] <- c('mean', 'lower', 'upper'); x} )
for (i in 1:length(tmp)) { assign(tmp[i], tmpobjects[[i]]) }

if (EDUC.COMPARE) { 
  if (AGE.COMPARE) { data.to.plot <- rbind(overall.fx.data, overall.fx.age.data) } else { data.to.plot <- overall.fx.data } 
} else { data.to.plot <- rbind(overall.fx.data, overall.fx.large.data, overall.fx.small.data) }



####################################
# STRIP FUNCTION
####################################
numbers.of.obs <- c(N.small=sum(pol.size.small), Dem.N.small=sum(Dem & pol.size.small), Rep.N.small=sum(Rep & pol.size.small),
                    N.large=sum(pol.size.large), Dem.N.large=sum(Dem & pol.size.large), Rep.N.large=sum(Rep & pol.size.large),
                    N=nrow(data),                Dem.N=sum(Dem),                        Rep.N      =sum(Rep),
                    educ.low.N=sum(educ.low, na.rm=TRUE), educ.hi.N=sum(!educ.low, na.rm=TRUE),
                    age.lo.N=sum(age.lo, na.rm=TRUE),     age.hi.N=sum(age.hi, na.rm=TRUE))
                    #soph.lo.N=sum(soph.low, na.rm=TRUE),  soph.hi.N=sum(soph.hi, na.rm=TRUE))               

if ( RIGHT.STRIP) { 
  panels.with.horizontal.strips <- 7:9
  striptext <- rep(c('all subjects', 'Dem. subjects', 'GOP subjects'), 3) 
}
if (!RIGHT.STRIP) { 
  if (EDUC.COMPARE) { 
    if (AGE.COMPARE) { 
      panels.with.horizontal.strips <- 1:6 
    } else { 
      panels.with.horizontal.strips <- 1:3 
    } 
  } else { 
    panels.with.horizontal.strips <- 1:9 
  }
  if (EDUC.COMPARE) { 
    striptext <- c(paste('all subjects\n(N = ',           numbers.of.obs['N'],          ')', sep=''),
                   paste('low\u00ADed. subjects\n(N = ',  numbers.of.obs['educ.low.N'], ')', sep=''),
                   paste('high\u00ADed. subjects\n(N = ', numbers.of.obs['educ.hi.N'],  ')', sep='')) 
  } 
  if (AGE.COMPARE)  { 
    striptext <- c(  striptext, striptext[1], paste('age 30 or younger\n(N = ', numbers.of.obs['age.lo.N'], ')', sep=''),
                                              paste('age 55 or older\n(N = ', numbers.of.obs['age.hi.N'], ')', sep='')) 
  }
  if (!(EDUC.COMPARE)) {
    striptext <- c(paste('all subjects,\nsmall change (N = ',  numbers.of.obs['N.small'],     ')', sep=''),
                   paste('Dem. subjects,\nsmall change (N = ', numbers.of.obs['Dem.N.small'], ')', sep=''),
                   paste('GOP subjects,\nsmall change (N = ',  numbers.of.obs['Rep.N.small'], ')', sep=''),
                   paste('all subjects,\nlarge change (N = ',  numbers.of.obs['N.large'],     ')', sep=''),
                   paste('Dem. subjects,\nlarge change (N = ', numbers.of.obs['Dem.N.large'], ')', sep=''),
                   paste('GOP subjects,\nlarge change (N = ',  numbers.of.obs['Rep.N.large'], ')', sep=''),
                   paste('all subjects\n(N = ',                numbers.of.obs['N'],           ')', sep=''),
                   paste('Dem. subjects\n(N = ',               numbers.of.obs['Dem.N'],       ')', sep=''),
                   paste('GOP subjects\n(N = ',                numbers.of.obs['Rep.N'],       ')', sep=''))
  }
}

horiz.strip <- function(which.panel, ...) {
  if (panel.number()%in%panels.with.horizontal.strips) {
    grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5)) # gp=gpar(fill=trellis.par.get("strip.background")$col[1]))
    ltext(.5, .5, striptext[panel.number()], cex=striptextsize, font=2, lineheight=1.1)
  }
}
  


################
# PANEL FUNCTION
################
myPanel <- function(which.panel, ...) {
  ymid <- ylist$limits[1] + (.5*(ylist$limits[2]-ylist$limits[1]))
  trellis.par.set("clip", list(panel="on", strip="on")) 
  panel.Dotplot(...)
  trellis.par.set("clip", list(panel="off", strip="off")) # for permitting panel.axis(outside=T, ...)
  trellis.par.set("axis.text", list(cex=.65)) # set size of axis text
  if ((EDUC.COMPARE & AGE.COMPARE)) { panel.axis(side="bottom", outside=TRUE, at=xlist$at, label=xlist$labels, tck=xlist$tck[1], rot=c(0, 0)) }
  panel.number.in.figure <- c(7, 8, 9, 4, 5, 6, 1, 2, 3)
  if (RIGHT.STRIP) {     
    # make the right-hand strips
    if (panel.number()%in%c(3,6,9)) { grid.polygon(x=c(1,1.15,1.15,1,1), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5)) } # gp=gpar(fill=trellis.par.get("strip.background")$col[1]))
    if (panel.number()==3) { panel.text(x=xlim[2]+.24, y=ymid, adj=.5, label='small changes',  font=2, cex=striptextsize, srt=-90)}
    if (panel.number()==6) { panel.text(x=xlim[2]+.24, y=ymid, adj=.5, label='large changes',  font=2, cex=striptextsize, srt=-90)}
    if (panel.number()==9) { panel.text(x=xlim[2]+.24, y=ymid, adj=.5, label='all conditions', font=2, cex=striptextsize, srt=-90)}
  }  
}



###################
# PRINT TO SCREEN
###################
trellis.par.set("axis.line",  list(alpha=1, col=dotplotbordercolor, lty=1, lwd=0.5)) # panel border lines
trellis.par.set("clip",       list(panel="off", strip="off"))                        # for permitting panel.axis(outside=T, ...)
trellis.par.set("dot.symbol", list(alpha=1, cex=.5, col=dotcolor, font=1, pch=16))   # black dot (not grey) to mark the mean
trellis.par.set("dot.line",   list(alpha=1, col=dotplotlinecolor, lty=1, lwd=1))
trellis.par.set("plot.line",  list(alpha=1, col=dotcolor, lty=1, lwd=1))
tmp <- trellis.par.get("layout.heights"); tmp$axis.xlab.padding <- xlab.padding; tmp$lwd=0; trellis.par.set("layout.heights", tmp)
trellis.par.set("par.xlab.text", list(alpha=1, cex=xlabsize, col='black', font=1)) # bold print for x label
tmp <- trellis.par.get("strip.border"); tmp$col[1] <- 'white'; trellis.par.set("strip.border", tmp)
Med201005.overall.dotplot <- Dotplot(order ~ Cbind(mean, lower, upper) | panel,
                                     data=data.to.plot,
                                     xlab=xlab, ylab="",
                                     xlim=xlim,
                                     panel=myPanel,
                                     layout=layout,
                                     between=list(y=ybetween, x=xbetween),                   
                                     strip=horiz.strip,
                                     par.strip.text=parstriptext, # gives stripheight
                                     scales=list(y=ylist, x=xlist))
print(Med201005.overall.dotplot, panel.width=list(panelwidth, "inches"), panel.height=list(panelheight, "inches"))



###################
# CLEAN UP
###################
for (i in packages.to.detach) { 
  if (any(regexpr(i, old.search)>-1)) { do.call(library, list(i)) } 
}

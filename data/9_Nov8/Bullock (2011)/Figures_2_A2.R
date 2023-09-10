# Figures_2_A2.R
# John G. Bullock
# 2011 August 28

# Created from Medicaid_2008_11_dotplots.R

# Creates Figure 2 or Figure A2 in  
# 
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (3): 496-515.

# If EDUC.COMPARE is set to FALSE  on line 37, this code creates Figure 2.  
# If EDUC.COMPARE is set to TRUE   on line 37, this code creates Figure A2.

# This file generates Postscript with postscript(), then calls Acrobat 
# Distiller to convert the Postscript to PDF.  I do this instead of creating a
# PDF file directly (with pdf()) because the figure looks better on-screen 
# when I do it this way.  
#   Acrobat Distiller also embeds fonts in the PDF file, which pdf() does not.  
# You can use embedFonts() instead of Acrobat Distiller to embed fonts, but 
# the figure won't look as good.  In particular, the dots on 'j' characters 
# are off-center.


old.search <- search()
packages.to.detach <- c('Design', 'Hmisc')
for (i in packages.to.detach) { 
  if (any(grepl(i, search()))) { do.call(detach, list(paste('package:', i, sep=''))) } 
}
library(grid)
library(lattice)
library(Hmisc)
library(pscl)
source('Medicaid_2008_11_average_effects.R') # source code for computing average effects and bootstrapped CIs.  Takes nearly ten minutes.


EDUC.COMPARE       <- FALSE  # panels vary by education, not partisanship 
postscript.bg      <- 'transparent'
dotcolor           <- 'black'
dotplotlinecolor   <- 'gray67'
dotplotbordercolor <- 'black'
stripbordercolor   <- 'black'
stripbgcolor       <- 'gray77' # best compromise between screen and print appearance.
panelheight        <- 1.325
panelwidth         <- 1.25 # 3.0 # inches
stripheight        <- 2.075
if (EDUC.COMPARE) { xlim <- c(0, 2.65) } else { xlim <- c(.1, 2.9) }
xlab               <- 'Average Attitude Differences'
base.cex.size <- 1.00
striptextsize <-  .80*base.cex.size # cex
xlabsize      <-  .80*base.cex.size # cex
xaxissize     <-  .80*base.cex.size # cex
ylabsize      <-  .80*base.cex.size # cex
xbetween      <-  .25
xlist         <- list(  draw=T, alternating=1, labels=c('.5', '1.5', '2.5'), at=seq(.5, 2.5, by=1), tck=c(.5,0), cex=xaxissize, relation='free')  
ylist         <- list(  draw=T, alternating=1
                           , labels=c('Dem. legislators support vs. no cues', 'Dem. legislators oppose vs. no cues', 'Dem. legis. support vs. Dem. legis. oppose', 'subjects\u2019 party ID: Dem. vs. Republican', 'policy direction: liberal vs. conservative')                             
                           , at=5:1, tck=c(0,0), col='black', cex=ylabsize, limits=c(.6, 5.5))  


#####################################################################
# DATA FOR DOTPLOTS
#####################################################################
# Some calculations are in Medicaid_2008_11_average_effects.R [2009 06 23]
age.low <- data$age<=30 & data$age.legit
age.hi  <- data$age> 55 & data$age.legit
if (!EDUC.COMPARE) {
  overall.fx.data <- data.frame(  effect=rep(c('5Demsupp', '4Demopp', '3Demsuppopp', '2PID', '1policy_info'), 3)
                                , panel=rep(1:3, each=5)
                                , rbind(  bootresult.Demsupport.summary
                                        , bootresult.Demoppose.summary
                                        , bootresult.Demsuppoppose.summary
                                        , bootresult.PID.summary
                                        , bootresult.polinfo.summary

                                        , bootresult.Demsupport.Demsonly.summary
                                        , bootresult.Demoppose.Demsonly.summary
                                        , bootresult.Demsuppoppose.Demsonly.summary
                                        , c(-100, -100, -100)
                                        , bootresult.polinfo.Demsonly.summary

                                        , bootresult.Demsupport.GOPonly.summary
                                        , bootresult.Demoppose.GOPonly.summary
                                        , bootresult.Demsuppoppose.GOPonly.summary
                                        , c(-100, -100, -100)                                    
                                        , bootresult.polinfo.GOPonly.summary
                                        
                                        , deparse.level=0))
}

if (EDUC.COMPARE) {
    overall.fx.data <- data.frame(  effect=rep(c('5Demsupp', '4Demopp', '3Demsuppopp', '2PID', '1policy_info'), 3)
            , panel=rep(1:3, each=5)
            , rbind(  bootresult.Demsupport.summary
                    , bootresult.Demoppose.summary
                    , bootresult.Demsuppoppose.summary
                    , bootresult.PID.summary
                    , bootresult.polinfo.summary
					
                    , bootresult.Demsupport.lowedonly.summary
                    , bootresult.Demoppose.lowedonly.summary
                    , bootresult.Demsuppoppose.lowedonly.summary
                    , bootresult.PID.summary.lowedonly
                    , bootresult.polinfo.lowedonly.summary
					
                    , bootresult.Demsupport.highedonly.summary
                    , bootresult.Demoppose.highedonly.summary
                    , bootresult.Demsuppoppose.highedonly.summary
                    , bootresult.PID.summary.highedonly                                    
                    , bootresult.polinfo.highedonly.summary
                    
                    , deparse.level=0))
}


names(overall.fx.data)[3:5] <- c('mean', 'lower', 'upper')
if (EDUC.COMPARE) {
	numbers.of.obs <- c(overall=nrow(data), 
                      Dem.N=sum(educ.low, na.rm=TRUE), 
                      GOP.N=sum(!educ.low, na.rm=TRUE))
} else { 
  numbers.of.obs <- c(overall=nrow(data), 
                      Dem.N=sum(Dem, na.rm=TRUE), 
                      GOP.N=sum(Rep, na.rm=TRUE)) 
}

                       
#####################################################################
# STRIP FUNCTION
#####################################################################
horiz.strip <- function(which.panel, ...) {
  grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbgcolor, lwd=.5)) # gp=gpar(fill=trellis.par.get("strip.background")$col[1]))
  if (panel.number()==1)      { ltext(.5, .5, paste('all subjects\n(N = ',  numbers.of.obs['overall'], ')', sep=''), cex=striptextsize, font=2, lineheight=1.1) }  
  else if (panel.number()==2 &  EDUC.COMPARE) { ltext(.5, .5, paste('low\u00ADed. subjects\n(N = ', numbers.of.obs['Dem.N'],   ')', sep=''), cex=striptextsize, font=2, lineheight=1.1) }
  else if (panel.number()==3 &  EDUC.COMPARE) { ltext(.5, .5, paste('high\u00ADed. subjects\n(N = ',  numbers.of.obs['GOP.N'],   ')', sep=''), cex=striptextsize, font=2, lineheight=1.1) }
  else if (panel.number()==2) { ltext(.5, .5, paste('Dem. subjects\n(N = ', numbers.of.obs['Dem.N'],   ')', sep=''), cex=striptextsize, font=2, lineheight=1.1) }
  else if (panel.number()==3) { ltext(.5, .5, paste('GOP subjects\n(N = ',  numbers.of.obs['GOP.N'],   ')', sep=''), cex=striptextsize, font=2, lineheight=1.1) }  
}
  

#####################################################################
# PANEL FUNCTION
#####################################################################
myPanel <- function(...) {
  # gives x-axis to each panel
  trellis.par.set("clip", list(panel="on", strip="on")) 
  panel.Dotplot(...)
  trellis.par.set("clip", list(panel="off", strip="off")) # for permitting panel.axis(outside=T, ...)
  trellis.par.set("axis.text", list(cex=.65)) # set size of axis text
}


#####################################################################
# PRINT TO SCREEN
#####################################################################
trellis.par.set("axis.line",  list(alpha=1, col=dotplotbordercolor, lty=1, lwd=0.5)) # panel border lines
trellis.par.set("clip",       list(panel="off", strip="off"))                        # for permitting panel.axis(outside=T, ...)
trellis.par.set("dot.symbol", list(alpha=1, cex=.5, col=dotcolor, font=1, pch=16))   # black dot (not grey) to mark the mean
trellis.par.set("dot.line",   list(alpha=1, col=dotplotlinecolor, lty=1, lwd=1))
trellis.par.set("plot.line",  list(alpha=1, col=dotcolor, lty=1, lwd=1))
tmp <- trellis.par.get("layout.heights"); tmp$axis.xlab.padding <- .875; tmp$lwd=0; trellis.par.set("layout.heights", tmp)
trellis.par.set("par.xlab.text", list(alpha=1, cex=xlabsize, col='black', font=1)) # bold print for x label
tmp <- trellis.par.get("strip.border"); tmp$col[1] <- 'white'; trellis.par.set("strip.border", tmp)
Med200811.overall.dotplot <- Dotplot(effect ~ Cbind(mean, lower, upper) | panel,
                                     data=overall.fx.data,
                                     xlab=xlab, ylab="",
                                     xlim=xlim,
                                     panel=myPanel,
                                     layout=c(3,1),  # columns, then rows
                                     between=list(y=0, x=xbetween),                   
                                     strip=horiz.strip,
                                     par.strip.text=list(lines=stripheight),
                                     scales=list(y=ylist, x=xlist))         
print(Med200811.overall.dotplot, panel.width=list(panelwidth, "inches"), panel.height=list(panelheight, "inches"))

# Figure_A1.R
# John G. Bullock
# 2011 August 28

# Created from SSI_CPS_NES_comparison.R

# Creates Figure A1 in the appendix to  
# 
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (3): 496-515.


load('Experiment_1.RData')
library(foreign)
library(grid)
library(lattice)
library(Hmisc)
library(Design)
ANES_2008       <- TRUE  # if false, use ANES 2004



################################################################
# SET UP FIGURE PARAMETERS [2009 03 10]
################################################################
postscript.bg   <- 'transparent'
stripbackgroundcolor <- 'gray77'
stripbordercolor     <- 'black'
striptextsize        <-  .75  # cex
stripheight          <- 2
dotplotbordercolor   <- 'black'
dotplotlinecolor     <- 'gray57'  
dotcolor      <- 'black' #gray(.28) # lower numbers are darker  
xlabsize      <-  .9   # cex
ylabsize      <-  .9  # cex
xaxissize     <-  .75
yaxissize     <-  .825
panelwidth    <- 4.3   # inches
panelheight   <- 5
  panelLayout   <- c(1,1) # columns, then rows
  xlist         <- list(draw=T, alternating=1, labels=c('10%', '25%', '40%', '55%', '70%', '85%'), at=seq(.1,.9, by=.15), tck=c(.5,0), cex=xaxissize, col=dotplotbordercolor)    
xlab          <- 'Sample Percentages vs. U.S. Population Percentages'
  ylist         <- list(  draw=TRUE, alternating=1
                      , labels=c('identify Cheney as VP of USA', 'need for cognition (both)', 'need for cognition (responsibility)', 'need for cognition (complex tasks)', 'advanced degree', 'no education after high school', 'age 56 or older', 'age 30 or younger', 'female', 'West', 'South', 'Northeast', 'Midwest')
  					  , cex=yaxissize
  		              , at=c(1:13)
                      , tck=c(0,0), col="black")		  


                  
######################################################################
# LOAD ANES DATA AND PICK OUT THE WEIGHTS [2009 06 20]
######################################################################
# The ANES datasets used here are the 2004 ANES Time Series Study (ICPSR 4245)
# and (if ANES_2008==TRUE) the 2008 ANES Time Series Study (ICPSR 25383).  
#   See http://electionstudies.org/studypages/2004prepost/2004prepost.htm for 
# more information on the 2004 dataset.  It can be downloaded from 
# ftp://ftp.electionstudies.org/ftp/nes/studypages/2004prepost/anes2004TSdta.zip.
#   See http://electionstudies.org/studypages/2008prepost/2008prepost.htm for 
# more information on the 2008 dataset.  It can be downloaded from 
# ftp://ftp.electionstudies.org/ftp/nes/studypages/2008prepost/anes2008TSdta.zip.
#
# Data for the Cheney question on the 2008 ANES have not been released.  
# So I need to load the 2004 ANES even if I am mainly using the 2008 ANES.   
# 
NES2004.dir <- 'c:/school/data/NES/2004/'  # replace with path to 2004 ANES dataset
NES2008.dir <- 'c:/school/data/NES/2008/'  # replace with path to 2008 ANES dataset
NES2004 <- read.dta(paste(NES2004.dir, 'anes2004TS.dta', sep=''))
  # warnings about missing value labels can be safely ignored
NES2004weights.post          <- NES2004$V040102
NES2004weights.post[unclass(NES2004$V043116) %in%c(4,8,9,10)] <- 0 
if (ANES_2008) {
  NES2008             <- read.dta(paste(NES2008.dir, 'anes2008TS.dta', sep=''))
  NES2008weights.post <- NES2008$V080102 # post-election weights, centered at 1.0
  NES2008age          <- NES2008$V081104
  NES2008Rep          <- NES2008$V083097=='2. Republican' | NES2008$V083098b=='1. Closer to Republican' 
  NES2008weights.post[unclass(NES2008$V083098b)==5] <- 0    
} 



################################################################
# SET UP DATA FOR DOTPLOTS [2009 03 10]
################################################################
# 2009 03 11: To get the row order right (e.g., female at top), 
# just add a new column, "rownumfromtop".  This will turn out 
# to be the best way to do it.  -When this is done-, add row 
# labels.
proportions <- expand.grid(characteristic=c('Cheney', 'ncog.both', 
                                            'ncog.responsibility', 
                                            'ncog.complex', 
                                            '>B.A.', '<=HS', 
                                            '>55', '<=30', 
                                            'female', 'West', 'South', 
                                            'Northeast', 'North.Central'), 
                           sample=c('SSI', 'ANES', 'CPS')) 
proportions$successes <- proportions$failures <- rep(NA, nrow(proportions))
proportions$successes[proportions$characteristic=='female'        & proportions$sample=='SSI'] <- sum( data$female, na.rm=TRUE)
proportions$failures [proportions$characteristic=='female'        & proportions$sample=='SSI'] <- sum(!data$female, na.rm=TRUE)
proportions$successes[proportions$characteristic=='<=30'          & proportions$sample=='SSI'] <- sum( data$age[data$age>=13 & data$age<=90]<=30, na.rm=TRUE)
proportions$failures [proportions$characteristic=='<=30'          & proportions$sample=='SSI'] <- sum( data$age[data$age>=13 & data$age<=90]>30, na.rm=TRUE)
proportions$successes[proportions$characteristic=='>55'          & proportions$sample=='SSI'] <- sum( data$age[data$age>=13 & data$age<=90]>55, na.rm=TRUE)
proportions$failures [proportions$characteristic=='>55'          & proportions$sample=='SSI'] <- sum( data$age[data$age>=13 & data$age<=90]<=55, na.rm=TRUE)
proportions$successes[proportions$characteristic=='<=HS'          & proportions$sample=='SSI'] <- sum( data$education[data$age>=25 & data$age<=90]<='12th', na.rm=TRUE)
proportions$failures [proportions$characteristic=='<=HS'          & proportions$sample=='SSI'] <- sum( data$education[data$age>=25 & data$age<=90]>'12th',  na.rm=TRUE)
proportions$successes[proportions$characteristic=='>B.A.'         & proportions$sample=='SSI'] <- sum( data$education[data$age>=25 & data$age<=90]> 'B.A.', na.rm=TRUE)
proportions$failures [proportions$characteristic=='>B.A.'         & proportions$sample=='SSI'] <- sum( data$education[data$age>=25 & data$age<=90]<='B.A.', na.rm=TRUE)
proportions$successes[proportions$characteristic=='Northeast'     & proportions$sample=='SSI'] <- sum(data$region=='Northeast', na.rm=TRUE)
proportions$failures [proportions$characteristic=='Northeast'     & proportions$sample=='SSI'] <- sum(data$region!='Northeast', na.rm=TRUE)
proportions$successes[proportions$characteristic=='North.Central' & proportions$sample=='SSI'] <- sum(data$region=='North Central', na.rm=TRUE)
proportions$failures [proportions$characteristic=='North.Central' & proportions$sample=='SSI'] <- sum(data$region!='North Central', na.rm=TRUE)
proportions$successes[proportions$characteristic=='South'         & proportions$sample=='SSI'] <- sum(data$region=='South', na.rm=TRUE)
proportions$failures [proportions$characteristic=='South'         & proportions$sample=='SSI'] <- sum(data$region!='South', na.rm=TRUE)
proportions$successes[proportions$characteristic=='West'          & proportions$sample=='SSI'] <- sum(data$region=='West', na.rm=TRUE)
proportions$failures [proportions$characteristic=='West'          & proportions$sample=='SSI'] <- sum(data$region!='West', na.rm=TRUE)
proportions$successes[proportions$characteristic=='ncog.complex'  & proportions$sample=='SSI'] <- sum(data$ncog1> 'no preference', na.rm=TRUE) 
proportions$failures [proportions$characteristic=='ncog.complex'  & proportions$sample=='SSI'] <- sum(data$ncog1<='no preference', na.rm=TRUE)  
proportions$successes[proportions$characteristic=='ncog.responsibility' & proportions$sample=='SSI'] <- sum(data$ncog6>'neither like nor dislike', na.rm=TRUE)  
proportions$failures [proportions$characteristic=='ncog.responsibility' & proportions$sample=='SSI'] <- sum(data$ncog6<='neither like nor dislike', na.rm=TRUE) 
  # leave out middle category for greater compatibility for the 2008 ANES version of this question: branching causes far fewer people to take the middle position
proportions$successes[proportions$characteristic=='ncog.both' & proportions$sample=='SSI'] <- sum(data$ncog6>'neither like nor dislike'  & data$ncog1>'no preference', na.rm=TRUE)  
proportions$failures [proportions$characteristic=='ncog.both' & proportions$sample=='SSI'] <- sum(data$ncog6<='neither like nor dislike' | data$ncog1<='no preference', na.rm=TRUE)
proportions$successes[proportions$characteristic=='Cheney'        & proportions$sample=='SSI'] <- sum( data$gk.Cheney, na.rm=TRUE) 
proportions$failures [proportions$characteristic=='Cheney'        & proportions$sample=='SSI'] <- sum(!data$gk.Cheney, na.rm=TRUE) 

if (ANES_2008) {
    tmp <- na.omit(cbind(as.numeric(NES2008$V083311=='2. Female'), NES2008weights.post))
    proportions$successes[proportions$characteristic=='female'    & proportions$sample=='ANES'] <- tmp[,1]%*%tmp[,2] 
    proportions$failures [proportions$characteristic=='female'    & proportions$sample=='ANES'] <- (1-tmp[,1])%*%tmp[,2]    
    tmp <- na.omit(cbind(NES2008age, NES2008weights.post))
    proportions$successes[proportions$characteristic=='<=30'      & proportions$sample=='ANES'] <- as.numeric(tmp[,1]<=30)%*%tmp[,2] 
    proportions$failures [proportions$characteristic=='<=30'      & proportions$sample=='ANES'] <- as.numeric(tmp[,1]> 30)%*%tmp[,2]
    proportions$successes[proportions$characteristic=='>55'      & proportions$sample=='ANES'] <- as.numeric(tmp[,1]> 55)%*%tmp[,2] 
    proportions$failures [proportions$characteristic=='>55'      & proportions$sample=='ANES'] <- as.numeric(tmp[,1]<=55)%*%tmp[,2]    
    tmp <- na.omit(cbind(as.numeric(NES2008$V083217<=12)[NES2008age>=25], NES2008weights.post[NES2008age>=25]))
    proportions$successes[proportions$characteristic=='<=HS'      & proportions$sample=='ANES'] <- tmp[,1]%*%tmp[,2]
    proportions$failures [proportions$characteristic=='<=HS'      & proportions$sample=='ANES'] <- (1-tmp[,1])%*%tmp[,2]    
    tmp <- na.omit(cbind(as.numeric(NES2008$V083218b)[NES2008age>=25]%in%6:10, NES2008weights.post[NES2008age>=25]))
    proportions$successes[proportions$characteristic=='>B.A.'     & proportions$sample=='ANES'] <-  tmp[,1]%*%tmp[,2]
    proportions$failures [proportions$characteristic=='>B.A.'     & proportions$sample=='ANES'] <- (1-tmp[,1])%*%tmp[,2]
    proportions$successes[proportions$characteristic=='Northeast' & proportions$sample=='ANES'] <- as.numeric(unclass(NES2008$V081204)==1)%*%NES2008weights.post 
    proportions$failures [proportions$characteristic=='Northeast' & proportions$sample=='ANES'] <- as.numeric(unclass(NES2008$V081204)!=1)%*%NES2008weights.post
    proportions$successes[proportions$characteristic=='North.Central' & proportions$sample=='ANES'] <- as.numeric(unclass(NES2008$V081204)==2)%*%NES2008weights.post
    proportions$failures [proportions$characteristic=='North.Central' & proportions$sample=='ANES'] <- as.numeric(unclass(NES2008$V081204)!=2)%*%NES2008weights.post
    proportions$successes[proportions$characteristic=='South' & proportions$sample=='ANES']         <- as.numeric(unclass(NES2008$V081204)==3)%*%NES2008weights.post
    proportions$failures [proportions$characteristic=='South' & proportions$sample=='ANES']         <- as.numeric(unclass(NES2008$V081204)!=3)%*%NES2008weights.post    
    proportions$successes[proportions$characteristic=='West' & proportions$sample=='ANES']          <- as.numeric(unclass(NES2008$V081204)==4)%*%NES2008weights.post
    proportions$failures [proportions$characteristic=='West' & proportions$sample=='ANES']          <- as.numeric(unclass(NES2008$V081204)!=4)%*%NES2008weights.post   
    tmp <- na.omit(cbind(NES2008$V085171, NES2008weights.post))
    proportions$successes[proportions$characteristic=='ncog.complex' & proportions$sample=='ANES'] <- as.numeric(tmp[,1]==5)%*%tmp[,2] 
    proportions$failures [proportions$characteristic=='ncog.complex' & proportions$sample=='ANES'] <- as.numeric(tmp[,1]==4)%*%tmp[,2] 
    tmp <- na.omit(cbind(NES2008$V085170x, NES2008weights.post))
    proportions$successes[proportions$characteristic=='ncog.responsibility' & proportions$sample=='ANES'] <- as.numeric(tmp[,1]<6) %*%tmp[,2] 
    proportions$failures [proportions$characteristic=='ncog.responsibility' & proportions$sample=='ANES'] <- as.numeric(tmp[,1]>=6)%*%tmp[,2] 
    tmp <- na.omit(cbind(NES2008$V085171, NES2008$V085170x, NES2008weights.post))
    proportions$successes[proportions$characteristic=='ncog.both' & proportions$sample=='ANES'] <- as.numeric(tmp[,1]==5 & tmp[,2]<6)%*%tmp[,3] 
    proportions$failures [proportions$characteristic=='ncog.both' & proportions$sample=='ANES'] <- as.numeric(tmp[,1]==4 | tmp[,2]>=6)%*%tmp[,3]
} else {
    proportions$successes[proportions$characteristic=='female'    & proportions$sample=='ANES'] <- as.numeric(NES2004$V041109A=='2. Female')%*%NES2004weights.post 
    proportions$failures [proportions$characteristic=='female'    & proportions$sample=='ANES'] <- as.numeric(NES2004$V041109A=='1. Male')%*%NES2004weights.post
    proportions$successes[proportions$characteristic=='<=30'      & proportions$sample=='ANES'] <- as.numeric(NES2004$V043250<=30)%*%NES2004weights.post 
    proportions$failures [proportions$characteristic=='<=30'      & proportions$sample=='ANES'] <- as.numeric(NES2004$V043250>30)%*%NES2004weights.post
    proportions$successes[proportions$characteristic=='>55'      & proportions$sample=='ANES'] <- as.numeric(NES2004$V043250>55)%*%NES2004weights.post 
    proportions$failures [proportions$characteristic=='>55'      & proportions$sample=='ANES'] <- as.numeric(NES2004$V043250<=55)%*%NES2004weights.post
    proportions$successes[proportions$characteristic=='<=HS'      & proportions$sample=='ANES'] <- (ordered(NES2004$V043254)[NES2004$V043250>=25]<='3. High school diploma or equivalency test')%*%NES2004weights.post[NES2004$V043250>=25]
    proportions$failures [proportions$characteristic=='<=HS'      & proportions$sample=='ANES'] <- (ordered(NES2004$V043254)[NES2004$V043250>=25]> '3. High school diploma or equivalency test')%*%NES2004weights.post[NES2004$V043250>=25]
    proportions$successes[proportions$characteristic=='>B.A.'     & proportions$sample=='ANES'] <- (ordered(NES2004$V043254)[NES2004$V043250>=25]=='7. Advanced degree, including LLB')%*%NES2004weights.post[NES2004$V043250>=25]
    proportions$failures [proportions$characteristic=='>B.A.'     & proportions$sample=='ANES'] <- (ordered(NES2004$V043254)[NES2004$V043250>=25]< '7. Advanced degree, including LLB')%*%NES2004weights.post[NES2004$V043250>=25]
    proportions$successes[proportions$characteristic=='Northeast' & proportions$sample=='ANES'] <- as.numeric(unclass(NES2004$V041205)==1)%*%NES2004weights.post 
    proportions$failures [proportions$characteristic=='Northeast' & proportions$sample=='ANES'] <- as.numeric(unclass(NES2004$V041205)!=1)%*%NES2004weights.post
    proportions$successes[proportions$characteristic=='North.Central' & proportions$sample=='ANES'] <- as.numeric(unclass(NES2004$V041205)==2)%*%NES2004weights.post
    proportions$failures [proportions$characteristic=='North.Central' & proportions$sample=='ANES'] <- as.numeric(unclass(NES2004$V041205)!=2)%*%NES2004weights.post
    proportions$successes[proportions$characteristic=='South' & proportions$sample=='ANES']         <- as.numeric(unclass(NES2004$V041205)==3)%*%NES2004weights.post
    proportions$failures [proportions$characteristic=='South' & proportions$sample=='ANES']         <- as.numeric(unclass(NES2004$V041205)!=3)%*%NES2004weights.post
    proportions$successes[proportions$characteristic=='West' & proportions$sample=='ANES']          <- as.numeric(unclass(NES2004$V041205)==4)%*%NES2004weights.post
    proportions$failures [proportions$characteristic=='West' & proportions$sample=='ANES']          <- as.numeric(unclass(NES2004$V041205)!=4)%*%NES2004weights.post
}
tmp <- na.omit(cbind(NES2004$V045163, NES2004weights.post)) # 2008 data for this question haven't been released [2009 09 09]
proportions$successes[proportions$characteristic=='Cheney' & proportions$sample=='ANES'] <- as.numeric(tmp[,1]==1)%*%tmp[,2]
proportions$failures [proportions$characteristic=='Cheney' & proportions$sample=='ANES'] <- as.numeric(tmp[,1]!=1)%*%tmp[,2]


proportions$props <- proportions$successes/(proportions$successes+proportions$failures)
for (i in 1:nrow(proportions)) {
	if (proportions[i, 'sample']=='CPS') { next }
    draws <- rbeta(10000, proportions$successes[i]+1, proportions$failures[i]+1) 
	proportions$lower[i] <- quantile(draws, .025)
	proportions$upper[i] <- quantile(draws, .975)
	# don't worry about error message about NAs [2009 06 21]
}


# Check ANES figures on <HS education [2009 06 22]
if (!ANES_2008) {
    round(table(data$education[data$age>25 & data$age.legit])/length(data$education[data$age>25 & data$age.legit]), 2)
    round(table(NES2004$V043254)/length(NES2004$V043254), 2)
    NES.PID <- NES2004$V043114
    NES.partisan <- NES.PID%in%c('1. Republican', '2. Democrat')
    round(table(NES2004$V043254[NES.PID%in%c('1. Republican', '2. Democrat')])/length(NES2004$V043254[NES.PID%in%c('1. Republican', '2. Democrat')]), 2)
    as.numeric(ordered(NES2004$V043254[NES.partisan])<='3. High school diploma or equivalency test', na.rm=TRUE)%*%NES2004weights.post[NES.partisan]
      sum(NES2004weights.post[NES.partisan]) # 643  
      # 277.6 / 643 = 43% of partisans have no more than a high-school education
    as.numeric(ordered(NES2004$V043254[NES.partisan & NES.age>=25])<='3. High school diploma or equivalency test', na.rm=TRUE)%*%NES2004weights.post[NES.partisan & NES.age>=25]
      sum(NES2004weights.post[NES.partisan & NES.age>=25]) 
      # 254.9 / 589.9 = 43% of partisans 25 or older have no high-school education
}


# Add CPS proportions [2009 06 20]
proportions[proportions$characteristic=='female'        & proportions$sample=='CPS', 5:7] <- c(.513, -10, -10)
proportions[proportions$characteristic=='<=30'          & proportions$sample=='CPS', 5:7] <- c(.222, -10, -10)
proportions[proportions$characteristic=='>55'           & proportions$sample=='CPS', 5:7] <- c(.310, -10, -10)
proportions[proportions$characteristic=='<=HS'          & proportions$sample=='CPS', 5:7] <- c(.459, -10, -10)
proportions[proportions$characteristic=='>B.A.'         & proportions$sample=='CPS', 5:7] <- c(.099, -10, -10)
proportions[proportions$characteristic=='North.Central' & proportions$sample=='CPS', 5:7] <- c(.223, -10, -10)
proportions[proportions$characteristic=='Northeast'     & proportions$sample=='CPS', 5:7] <- c(.185, -10, -10)
proportions[proportions$characteristic=='South'         & proportions$sample=='CPS', 5:7] <- c(.362, -10, -10)
proportions[proportions$characteristic=='West'          & proportions$sample=='CPS', 5:7] <- c(.229, -10, -10)


# strip out CI information for CPS proportions, which was erroneously generated [2009 06 21]
proportions[proportions$sample=='CPS', 6:7] <- c('NA', 'NA')


# change CPS proportions to "NA" when ANES proportions can and should be used [2009 08 11]
for (i in as.character(proportions$characteristic[proportions$sample=='ANES'])) {
  if (!is.na(proportions[proportions$characteristic==i & proportions$sample=='ANES', 5])) { 
    proportions[proportions$characteristic==i & proportions$sample=='CPS', 5:7] <- c(NA, NA, NA)
  }
}

# strip out ANES CIs that were erroneously generated [2009 08 11]
for (i in as.character(proportions$characteristic[proportions$sample=='ANES'])) {
    if (is.na(proportions[proportions$characteristic==i & proportions$sample=='ANES', 5])) { 
        proportions[proportions$characteristic==i & proportions$sample=='ANES', 5:7] <- c(NA, NA, NA)
    }
}


################################################################
# COMPARE PROPORTIONS [2009 07 16]
################################################################
SSI.props   <- proportions$props[proportions$sample=='SSI']
ref.props  <- proportions$props[proportions$sample=='CPS'] 
ANES.props <- proportions$props[proportions$sample=='ANES']
for (i in 1:length(ref.props)) { if(is.na(ref.props[i])) { ref.props[i] <- ANES.props[i] } }
diffs <- data.frame(proportions$characteristic[1:length(SSI.props)], round(SSI.props-ref.props, 2))



################################################################
# SET UP PANEL STRIPS [2009 03 10]
################################################################
horiz.strip <- function(...) {
  grid.polygon(x=c(0,1,1,0,0), y=c(0,0,1,1,0), gp=gpar(col=stripbordercolor, fill=stripbackgroundcolor, lwd=.5))
	if      (panel.number()==4) { ltext(.5, .5, paste('services and spending\nDem subjects (N=', strip.N.Dem.serv, ')', sep=''), font=2, cex=striptextsize) }  
	else if (panel.number()==1) { ltext(.5, .5, paste('services and spending\nGOP subjects (N=', strip.N.GOP.serv, ')', sep=''), font=2, cex=striptextsize) }  
}


################################################################
# PANEL FUNCTION [2009 03 10]
################################################################
myPanel <- function(...) {
    panel.Dotplot(...)
    trellis.par.set("clip", list(panel="off", strip="off")) # for permitting panel.axis(outside=T, ...)
}


################################################################
# PRINT TO SCREEN
################################################################
trellis.par.set("axis.line",  list(alpha=1, col=dotplotbordercolor, lty=1, lwd=.5))
trellis.par.set("clip",       list(panel="off", strip="off"))                      # for permitting panel.axis(outside=T, ...)
trellis.par.set("dot.symbol", list(alpha=1, cex=.5, col=dotcolor, font=1, pch=16)) # black dot (not grey) to mark the mean
trellis.par.set("dot.line",   list(alpha=1, col=dotplotlinecolor, lty=1, lwd=1))   # background lines in each row
trellis.par.set("plot.line",   list(alpha=1, col='red', lty=1, lwd=1))             # no obvious effect
trellis.par.set("reference.line",   list(alpha=1, col='red', lty=1, lwd=1))        # no obvious effect
trellis.par.set("superpose.line",   list(alpha=1, col='black', lty=1, lwd=1))      # CI lines in each row
tmp <- trellis.par.get('layout.heights'); tmp$axis.xlab.padding <- 1.25; trellis.par.set('layout.heights', tmp)
trellis.par.set("par.xlab.text", list(alpha=1, cex=xlabsize, col=dotplotbordercolor, font=1, lineheight=1))
sample.props <- Dotplot(characteristic ~ Cbind(props, lower, upper), 
                        groups=sample, data=proportions,		
                        xlab=xlab, ylab='',
                        xlim=c(.05, .9),
                        ylim=c(.4, max(ylist$at)+.6),
                        pch=c('S', 'N', 'C'),            # for the members of "samples": SSI, ANES, CPS		
                        font=c(1, 1),                    # plotted letters in plain text (1=plain, 2=bold)
                        cex=c(1, 1),                     # plotted letters not too small                  
                        col='black',                     # plotted letters in black
                        panel=myPanel,
                        layout=panelLayout,              # columns, then rows
                        scales=list(y=ylist, x=xlist))
print(sample.props, panel.width=list(panelwidth, "inches"), panel.height=list(panelheight, "inches"))

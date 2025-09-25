# Figure_A5.R
# John G. Bullock
# 2011 August 28

# Created from Medicaid_2010_05_SSI_CPS_NES_comparison.R

# Creates Figure A5 in the appendix to  
# 
#    Bullock, John G.  2011.  "Elite Influence on Public Opinion in an 
#    Informed Electorate."  American Political Science Review 105 (3): 496-515.

load('Experiment_2.RData')
library(foreign)
library(pscl)
library(grid)
library(lattice)
library(Hmisc)
library(Design)



################################################################
# SET UP FIGURE PARAMETERS [2009 03 10]
################################################################
stripbackgroundcolor <- 'gray77'
stripbordercolor     <- 'black'
striptextsize        <-  .75    # cex
stripheight          <- 2
dotplotbordercolor   <- 'black'
dotplotlinecolor     <- 'gray57'  
dotcolor             <- 'black' # gray(.28) # lower numbers are darker  
xlabsize             <-  .9     # cex
ylabsize             <-  .9     # cex
xaxissize            <-  .75
yaxissize            <-  .825
panelwidth           <- 4.3     # inches
panelheight          <- 5
panelLayout          <- c(1,1)  # columns, then rows
xlist                <- list(draw=TRUE, alternating=1, labels=c('10%', '25%', '40%', '55%', '70%', '85%'), at=seq(.1,.9, by=.15), tck=c(.5,0), cex=xaxissize, col=dotplotbordercolor)    
xlab                 <- 'Sample Percentages vs. U.S. Population Percentages'
ylist                <- list(draw=TRUE, alternating=1,
                             labels=c('need for cognition (both)', 'need for cognition (responsibility)', 'need for cognition (complex tasks)', 'advanced degree', 'no education after high school', 'age 56 or older', 'age 30 or younger', 'female', 'West', 'South', 'Northeast', 'Midwest'),
  					                 cex=yaxissize,
                             tck=c(0,0), col="black")
ylist$at             <- 1:length(ylist$labels)



######################################################################
# LOAD ANES DATA AND PICK OUT THE WEIGHTS [2009 06 20]
######################################################################
# The ANES dataset used here is the 2008 ANES Time Series Study (ICPSR 25383).  
#   See http://electionstudies.org/studypages/2008prepost/2008prepost.htm for 
# more information on the 2008 dataset.  It can be downloaded from 
# ftp://ftp.electionstudies.org/ftp/nes/studypages/2008prepost/anes2008TSdta.zip.
#
# Data for the Cheney question on the 2008 ANES have not been released.  
# So I need to load the 2004 ANES even if I am mainly using the 2008 ANES.   
# 
NES2008.dir <- 'c:/school/data/NES/2008/'  # replace with path to 2008 ANES dataset
NES2008 <- read.dta(paste(NES2008.dir, 'anes2008TS.dta', sep=''))
  # warnings about missing value labels can be safely ignored
NES2008weights.post <- NES2008$V080102 # post-election weights, centered at 1.0
NES2008age          <- NES2008$V081104
NES2008Republican   <- NES2008$V083097=='2. Republican' | NES2008$V083098b=='1. Closer to Republican'
NES2008Democrat     <- NES2008$V083097=='1. Democrat'   | NES2008$V083098b=='5. Closer to Democratic'
NES2008Republican[is.na(NES2008Republican)] <- FALSE
NES2008weights.post[unclass(NES2008$V083098b)==5] <- 0



################################################################
# SET UP DATA FOR DOTPLOTS [2009 03 10]
################################################################
characteristics       <- c('ncog.both', 'ncog.responsibility', 'ncog.complex', '>B.A.', '<=HS', '>55', '<=30', 'female', 'West', 'South', 'Northeast', 'North.Central')
proportions           <- expand.grid(characteristic=characteristics , sample=c('SSI', 'ANES')) 
proportions$successes <- proportions$failures <- rep(NA, nrow(proportions))

proportions$successes[proportions$characteristic=='female'        & proportions$sample=='SSI'] <- sum( data$female, na.rm=TRUE)
proportions$failures [proportions$characteristic=='female'        & proportions$sample=='SSI'] <- sum(!data$female, na.rm=TRUE)
proportions$successes[proportions$characteristic=='<=30'          & proportions$sample=='SSI'] <- sum( data$age[data$age>=13 & data$age<=90]<=30, na.rm=TRUE)
proportions$failures [proportions$characteristic=='<=30'          & proportions$sample=='SSI'] <- sum( data$age[data$age>=13 & data$age<=90]>30,  na.rm=TRUE)
proportions$successes[proportions$characteristic=='>55'           & proportions$sample=='SSI'] <- sum( data$age[data$age>=13 & data$age<=90]>55, na.rm=TRUE)
proportions$failures [proportions$characteristic=='>55'           & proportions$sample=='SSI'] <- sum( data$age[data$age>=13  & data$age<=90]<=55, na.rm=TRUE)
proportions$successes[proportions$characteristic=='<=HS'          & proportions$sample=='SSI'] <- sum( data$education[data$age>=25 & data$age<=90]<='12th', na.rm=TRUE)
proportions$failures [proportions$characteristic=='<=HS'          & proportions$sample=='SSI'] <- sum( data$education[data$age>=25 & data$age<=90]>'12th',  na.rm=TRUE)
proportions$successes[proportions$characteristic=='>B.A.'         & proportions$sample=='SSI'] <- sum( data$education[data$age>=25 & data$age<=90]> 'B.A.', na.rm=TRUE)
proportions$failures [proportions$characteristic=='>B.A.'         & proportions$sample=='SSI'] <- sum( data$education[data$age>=25 & data$age<=90]<='B.A.', na.rm=TRUE)
proportions$successes[proportions$characteristic=='Northeast'     & proportions$sample=='SSI'] <- sum(data$region=='Northeast',     na.rm=TRUE)
proportions$failures [proportions$characteristic=='Northeast'     & proportions$sample=='SSI'] <- sum(data$region!='Northeast',     na.rm=TRUE)
proportions$successes[proportions$characteristic=='North.Central' & proportions$sample=='SSI'] <- sum(data$region=='North Central', na.rm=TRUE)
proportions$failures [proportions$characteristic=='North.Central' & proportions$sample=='SSI'] <- sum(data$region!='North Central', na.rm=TRUE)
proportions$successes[proportions$characteristic=='South'         & proportions$sample=='SSI'] <- sum(data$region=='South',         na.rm=TRUE)
proportions$failures [proportions$characteristic=='South'         & proportions$sample=='SSI'] <- sum(data$region!='South',         na.rm=TRUE)
proportions$successes[proportions$characteristic=='West'          & proportions$sample=='SSI'] <- sum(data$region=='West',          na.rm=TRUE)
proportions$failures [proportions$characteristic=='West'          & proportions$sample=='SSI'] <- sum(data$region!='West',          na.rm=TRUE)
proportions$successes[proportions$characteristic=='ncog.complex'  & proportions$sample=='SSI'] <- sum(data$ncog1> 'no preference', na.rm=TRUE) 
proportions$failures [proportions$characteristic=='ncog.complex'  & proportions$sample=='SSI'] <- sum(data$ncog1<='no preference', na.rm=TRUE)  
proportions$successes[proportions$characteristic=='ncog.responsibility' & proportions$sample=='SSI'] <- sum(data$ncog6>'neither like nor dislike',  na.rm=TRUE)  
proportions$failures [proportions$characteristic=='ncog.responsibility' & proportions$sample=='SSI'] <- sum(data$ncog6<='neither like nor dislike', na.rm=TRUE) 
  # leave out middle category for greater compatibility for the 2008 ANES version of this question: branching causes far fewer people to take the middle position
proportions$successes[proportions$characteristic=='ncog.both' & proportions$sample=='SSI'] <- sum(data$ncog6>'neither like nor dislike'  & data$ncog1>'no preference', na.rm=TRUE)  
proportions$failures [proportions$characteristic=='ncog.both' & proportions$sample=='SSI'] <- sum(data$ncog6<='neither like nor dislike' | data$ncog1<='no preference', na.rm=TRUE)
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
 
proportions$props <- proportions$successes/(proportions$successes+proportions$failures)
proportions$lower <- proportions$upper <- NA
for (i in 1:nrow(proportions)) {
  if (is.na(proportions[i, 'failures'])) { next }
  draws <- rbeta(10000, proportions$successes[i]+1, proportions$failures[i]+1) 
	proportions$lower[i] <- quantile(draws, .025)
	proportions$upper[i] <- quantile(draws, .975)
}



################################################################
# PRINT FIGURE TO SCREEN
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
                        groups=sample, 
                        data=proportions,		
                        xlab=xlab, ylab='',
                        xlim=c(.05, .9), ylim=c(.4, max(ylist$at)+.6),
                        pch=c('S', 'N', 'C'),  # for the members of "samples": SSI, ANES, CPS		
                        font=c(1, 1),          # plotted letters in plain text (1=plain, 2=bold)
                        cex=c(1, 1),           # plotted letters not too small                  
                        col='black',           # plotted letters in black
                        layout=panelLayout,    # columns, then rows
                        scales=list(y=ylist, x=xlist))
print(sample.props, panel.width=list(panelwidth, "inches"), panel.height=list(panelheight, "inches"))

rm(list=ls())
setwd("~/Dropbox/Zimbabwe/ANALYSIS/replication")

library(ggplot2)
library(foreign)
library(plyr)
library(dplyr)
library(xtable)
library(TeachingDemos)
library(reporttools)
library(gridExtra)

# Figure 3 in manuscript

ZIMB <- read.dta("allrounds_updated.dta", convert.factors = F)

row_to_keep = which(ZIMB$edu !="NA")
ZIMB = ZIMB[row_to_keep,]

table(ZIMB$edu)
ZIMB$edu=factor(ZIMB$edu, labels=c("No schooling", "Incomplete primary", "Complete primary", "Incomplete high", "Complete high", "Incomplete college", "Complete college"))
length(ZIMB$edu)

pdf("EduDistribution.pdf", width=10, height=7)
ggplot(ZIMB, aes(x=edu)) +geom_histogram(binwidth=.55) +
  ggtitle("Education Attainment Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_x_discrete(name="") +  theme_bw() + 
  theme(panel.grid.major.x =element_blank(),
        panel.grid.minor.x =element_blank(),
        panel.grid.major.y =element_line(color="grey60", linetype="dashed"),
        plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10, face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=14,face="bold"), 
        axis.ticks.y = element_blank())
dev.off()  

#########################################
# # Secondary enrollment - figure 2 in manuscript
#########################################
library(gdata) 
DATA = read.xls("enrollment data.xlsx")
enroll = DATA[1:2,]

######################################

######################################

Secondary = DATA[c(2, 4),]
Secondaryl=reshape(Secondary, direction="long", varying=list(names(Secondary)[2:9]), v.names="Value",
                 idvar=c("Enrollment"), timevar="Year", times=1979:1986)

Secondaryl$students=Secondaryl$Value/1000

pdf("EduSecondary.pdf", width=10, height=7)
ggplot(Secondaryl, aes(x=Year,y=students,  shape=Enrollment, colour=Enrollment)) +
  ggtitle("Secondary Education Expansion") + theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_line(aes(group=factor(Enrollment)),size=1.5) + geom_point(size=4) + theme_bw() +
  scale_y_continuous(name="N. enrolled students (in 1,000)", breaks = seq(min(0), max(600), by = 100), limits = c(0, 600)) +
  scale_x_continuous(name="", limits=c(1979,1986)) +
  theme(legend.position=c(0,1), legend.justification=c(0,1),
        panel.grid.major.y = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13, face="bold"),
        axis.text.y = element_text(colour="grey20",size=13,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=14,face="bold"), 
        axis.ticks.y = element_blank())
dev.off()


######################################
# Polity IV over-time - figure 1 
######################################
polity2 <- read.dta("p4.dta", convert.factors = F)
names(polity2)

#row_to_keep = which(polity2$countryname =="Zimbabwe" | polity2$countryname =="Nigeria" | polity2$countryname =="Benin")
#polity = polity2[row_to_keep,]
polity = polity2[polity2$year<2013 & polity2$year>1998,]

pdf("Polity2B.pdf", width=12, height=7)
ggplot(polity, aes(y=polity2, x=year, color=countryname, linetype=countryname)) + geom_line(aes(group=factor(countryname)),size=1.5) + scale_color_brewer(palette="Set1")+
  geom_point(size=2) + theme_bw() + theme(legend.position=c(1,0), legend.justification=c(1,0))+
  scale_y_continuous(name="Polity2", limits=c(-8,8), breaks=-8:8) +
  scale_x_continuous(name="", breaks = seq(1999, 2012, 2)) +
  ggtitle("Polity-2 Score by Country-Year") + theme(plot.title = element_text(lineheight=.8, face="bold"))  
dev.off()


###############################################################################
# zimbabwe political participation in comparative perspective
# Figure 1 in online appendix
###############################################################################

AB45 <- read.dta("GGR_FinalData.dta", convert.factors = F)
str(AB45)
names(AB45)

###############################################################################
# AB5: Key correlated at the individual-level
###############################################################################
AB45$Community =  ifelse(AB45$Q26A>1, 1, 0)
AB45$Issue =  ifelse(AB45$Q26B>1, 1, 0)
AB45$Vote =  ifelse(AB45$Q27==1, 1,0)
AB45$Councilor =  ifelse(AB45$Q30A>0, 1, 0)

PolitialPart <- AB45 %>%
  select(ROUND, CABBR, Country, Community, Issue, Vote, Councilor, Withinwt) %>% 
  filter (ROUND==5) %>%
  group_by(CABBR) %>%
  summarize(Community = weighted.mean(Community, Withinwt, na.rm=T), 
            Issue = weighted.mean(Issue, Withinwt, na.rm=T), 
            Voting = weighted.mean(Vote, Withinwt, na.rm=T),
            Councilor = weighted.mean(Councilor, Withinwt, na.rm=T))

PolitialPartMean <- reshape(PolitialPart, varying = c("Community", "Issue", "Councilor", "Voting"), 
                         v.names = "Mean",
                         timevar = "Action", 
                         times = c("Attend Community meeting", "Raise issues with others", 
                                   "Contact Councilor", "Vote last elections"), 
                         new.row.names = 1:1000, direction = "long")

PolitialPartMean$CABBR = factor(PolitialPartMean$CABBR)
PolitialPartMean$Action = factor(PolitialPartMean$Action, levels=c("Attend Community meeting", "Raise issues with others", 
                                                                   "Contact Councilor", "Vote last elections"))

# Create Zimbabwe dummy
PolitialPartMean$ZIM = ifelse(PolitialPartMean$CABBR=="ZIM", 1,0)
PolitialPartMean$ZIM = factor(PolitialPartMean$ZIM , labels=c("Non-Zimb", "Zimbabwe"))
head(PolitialPartMean)
str(PolitialPartMean)

PolitialPartMean$Action_r = with(PolitialPartMean, factor(Action, levels = rev(levels(Action))))

AB5Fig=ggplot(PolitialPartMean,aes(x = Mean, y=Action_r)) + 
  geom_point(size=4, aes(colour=ZIM, shape=ZIM)) +  
  scale_x_continuous(name="Population share" , breaks = round(seq(min(0), max(1), by = 0.25),2), limits = c(0, 1)) +
  scale_y_discrete(name="") +
  ggtitle("Participation (Round-5)") + theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13),
        axis.text.y = element_text(colour="grey20",size=14,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=14,face="bold"), 
        axis.ticks.y = element_blank())
print(AB5Fig)

###############################################################################
# AB3: Key correlated at the individual-level
###############################################################################
AB3 <- read.dta("AB3_participation.data", convert.factors = F)
str(AB3)
names(AB3)

###############################################################################
# AB5: Key correlated at the individual-level
###############################################################################
AB3$Voted =  ifelse(AB3$q30==1, 1, 0)
AB3$Community =  ifelse(AB3$q31a>1, 1, 0)
AB3$Issue =  ifelse(AB3$q31b>1, 1, 0)
AB3$Councilor =  ifelse(AB3$q32a>0, 1, 0)

PolitialPart <- AB3 %>%
  select(Country, Community, Issue, Voted, Councilor,  withinwt) %>% 
  group_by(Country) %>%
  summarize(Community = weighted.mean(Community, withinwt, na.rm=T), 
            Issue = weighted.mean(Issue, withinwt, na.rm=T), 
            Voting = weighted.mean(Voted, withinwt, na.rm=T),
            Councilor = weighted.mean(Councilor, withinwt, na.rm=T))

PolitialPartMean <- reshape(PolitialPart, varying = c("Community", "Issue", "Councilor", "Voting"), 
                            v.names = "Mean",
                            timevar = "Action", 
                            times = c("Attend Community meeting", "Raise issues with others", 
                                      "Contact Councilor", "Vote last elections"), 
                            new.row.names = 1:1000, direction = "long")

PolitialPartMean$Country = factor(PolitialPartMean$Country)
PolitialPartMean$Action = factor(PolitialPartMean$Action, levels=c("Attend Community meeting", "Raise issues with others", 
                                                                   "Contact Councilor", "Vote last elections"))

# Create Zimbabwe dummy
PolitialPartMean$ZIM = ifelse(PolitialPartMean$Country=="zimbabwe", 1,0)
PolitialPartMean$ZIM = factor(PolitialPartMean$ZIM , labels=c("Non-Zimb", "Zimbabwe"))
head(PolitialPartMean)
str(PolitialPartMean)

PolitialPartMean$Action_r = with(PolitialPartMean, factor(Action, levels = rev(levels(Action))))

AB3Fig= ggplot(PolitialPartMean,aes(x = Mean, y=Action_r)) + 
  geom_point(size=4, aes(colour=ZIM, shape=ZIM)) +  
  scale_x_continuous(name="Population share" , breaks = round(seq(min(0), max(1), by = 0.25),2), limits = c(0, 1)) +
  scale_y_discrete(name="") +
  ggtitle("Participation (Round-3)") + theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 1.5),
        axis.title.y = element_text(colour="grey20",size=14,face="bold"),
        axis.text.x = element_text(colour="grey20",size=13),
        axis.text.y = element_text(colour="grey20",size=14,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=14,face="bold"), 
        axis.ticks.y = element_blank())
print(AB3Fig)

library(Rmisc)
pdf("ParticipationComparison.pdf", width=15, height=7)
multiplot(AB3Fig, AB5Fig, cols=2)
dev.off()
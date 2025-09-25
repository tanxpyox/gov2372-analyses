require(RCurl)
require(XML)

###DO NOT RUN###
#This code only shows you how we downloaded the transcripts#
#Oyez has since changed their website#
#This code no longer works#
#cases.tab is the output#
###DO NOT RUN###

#read in a .csv file that has all the case urls
cases<-read.table("cases.tab",header=TRUE,as.is=TRUE,sep="\t")
advocate_links<-list()
advocate_names<-list()
advocate_roles<-list()
for(i in 1:NROW(cases)){
	print(i)
	case_url<-paste("http://www.oyez.org",cases[i,"url"],sep="")
	caseSource<-getURL(case_url)
	advocate_links[[i]]<-sapply(getNodeSet(htmlTreeParse(caseSource, useInternalNodes = TRUE), "//fieldset[@class='fieldgroup group_advocates']//div[@class='field']//div[@class='field-label']/a"),xmlGetAttr, "href")
	advocate_names[[i]]<-sapply(getNodeSet(htmlTreeParse(caseSource, useInternalNodes = TRUE), "//fieldset[@class='fieldgroup group_advocates']//div[@class='field']//div[@class='field-label']/a"),xmlValue)
	advocate_roles[[i]]<-sapply(getNodeSet(htmlTreeParse(caseSource, useInternalNodes = TRUE), "//fieldset[@class='fieldgroup group_advocates']//div[@class='field']//div[@class='small']"),xmlValue)
}
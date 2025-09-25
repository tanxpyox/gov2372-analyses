require(RCurl)
require(XML)

###DO NOT RUN###
#This code only shows you how we downloaded the transcripts#
#Oyez has since changed their website#
#This code no longer works#
#cases.tab is the output#
###DO NOT RUN###

#read in a .csv file that has all the cases...an example can be found in /intial_results/
cases<-read.table("cases.tab",header=TRUE,as.is=TRUE,sep="\t")

###get links
cases$argument_link<-NA
cases$transcript_link<-NA
cases$opinion_link<-NA
cases$mp3_link<-NA
for(i in 1:NROW(cases)){
	print(i)
	
	case_url<-paste("http://www.oyez.org",cases[i,"url"],sep="")
	caseSource<-getURL(case_url)
	case_links<-sapply(getNodeSet(htmlTreeParse(caseSource, useInternalNodes = TRUE), "//div[@class='field case-player']/a"),xmlGetAttr, "rel")
	mp3_links<-sapply(getNodeSet(htmlTreeParse(caseSource, useInternalNodes = TRUE), "//div[@class='media-player-container']/a"),xmlGetAttr, "href")
	cases[i,"argument_link"]<-ifelse(length(case_links[grepl("argument",case_links)])==0,NA,case_links[grepl("argument",case_links)])
	cases[i,"transcript_link"]<-ifelse(length(case_links[grepl("transcript",case_links)])==0,NA,case_links[grepl("transcript",case_links)])
	cases[i,"opinion_link"]<-ifelse(length(case_links[grepl("opinion",case_links)])==0,NA,case_links[grepl("opinion",case_links)])
	cases[i,"mp3_link"]<-ifelse(length(mp3_links[grepl("mp3",mp3_links)])==0,NA,mp3_links[grepl("mp3",mp3_links)])
}
write.csv(cases,"cases.csv",row.names=FALSE)

###elimate double docket numbers
my_dockets<-unique(cases$docket_number)
cases$good<-0
for(i in 1:length(my_dockets)){
	print(i)
	temp_cases<-cases[cases$docket_number==my_dockets[i],]
	if(NROW(temp_cases)==1){
		cases[row.names(temp_cases)[1],"good"]<-1
	}
	
	if(NROW(temp_cases)>1){
		cases[row.names(temp_cases)[1],"good"]<-1
	}
}
cases<-cases[cases$good==1,]
cases<-cases[,-NCOL(cases)]
write.csv(cases,"cases.csv",row.names=FALSE)

###first check argument_link
cases<-read.csv("cases.csv",as.is=TRUE)
cases$transcript<-0
cases$mp3<-0
for(i in 3030:NROW(cases)){
	print(i)
	if(is.na(cases[i,"mp3_link"])==FALSE){
		if(is.na(cases[i,"argument_link"])==FALSE){
			###create directory
			dir.create(paste("/Dropbox/oral_arguments/",cases[i,"year"],"/",cases[i,"docket_number"],"/",sep=""),recursive=TRUE)
			
			###download mp3
			download.file(paste("http://www.oyez.org/",unlist(strsplit(cases[i,"mp3_link"],"api/media/"))[2],sep=""),paste("/Dropbox/oral_arguments/",cases[i,"year"],"/",cases[i,"docket_number"],"/",cases[i,"docket_number"],"_argument.mp3",sep=""),mode="wb",quiet=TRUE)
			
			###record done
			cases[i,"mp3"]<-1
			
			###download transcript
			transcript_url<-paste("http://www.oyez.org/sites/default/files",cases[i,"argument_link"],sep="")
			transcriptSource<-getURL(transcript_url)
			
			if(transcriptSource!=""){
				if(length(unlist(sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//text"),xmlGetAttr, "syncTime")))>0){
					###get lines
					temp_lines<-cbind(sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//text"),xmlValue),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//text"),xmlGetAttr, "syncTime"))
					colnames(temp_lines)<-c("text","sync_time")
					
					temp_lines<-data.frame(temp_lines)
					temp_lines$text<-as.character(temp_lines$text)
					temp_lines$sync_time<-as.numeric(as.character(temp_lines$sync_time))
					write.csv(temp_lines,"/Dropbox/oral_arguments/temp_lines.csv",row.names=FALSE)
				
					###get turns
					temp_turns<-cbind(sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//turn"),xmlGetAttr, "speaker"),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//turn"),xmlGetAttr, "startTime"),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//turn"),xmlGetAttr, "stopTime"))
					colnames(temp_turns)<-c("speaker","start_time","stop_time")
					write.csv(temp_turns,"/Dropbox/oral_arguments/temp_turns.csv",row.names=FALSE)
				
					###add speaker
					temp_lines<-read.csv("/Dropbox/oral_arguments/temp_lines.csv",as.is=TRUE)
					temp_turns<-read.csv("/oral_arguments/temp_turns.csv",as.is=TRUE)
					speaker_id<-sapply(temp_lines$sync_time,function(x){return(temp_turns[x>=temp_turns$start_time&temp_turns$stop_time>x,"speaker"][1])},USE.NAMES=FALSE)
					temp_lines<-cbind(speaker_id,temp_lines)
				
					###speaker info
					temp_speakers<-cbind(sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//speaker"),xmlValue),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//speaker"),xmlGetAttr, "id"),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//speaker"),xmlGetAttr, "type"),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//speaker"),xmlGetAttr, "gender"))
					colnames(temp_speakers)<-c("name","speaker_id","type","gender")
					write.csv(temp_speakers,"/Dropbox/oral_arguments/temp_speakers.csv",row.names=FALSE)
					temp_speakers<-read.csv("/Dropbox/oral_arguments/temp_speakers.csv",as.is=TRUE)
					temp_transcript<-merge(temp_lines,temp_speakers,by="speaker_id")
					temp_transcript<-temp_transcript[order(temp_transcript$sync_time),]
					write.csv(temp_transcript,paste("/Dropbox/oral_arguments/",cases[i,"year"],"/",cases[i,"docket_number"],"/",cases[i,"docket_number"],"_transcript.csv",sep=""),row.names=FALSE)
					
					###record done
					cases[i,"transcript"]<-1
				}	
			}
		}
	}
}

###second check transcript_link
cases<-read.csv("cases.csv",as.is=TRUE)
cases$argument<-cases$transcript
cases$transcript<-0
for(i in 2598:NROW(cases)){
	print(i)
	if(cases[i,"argument"]==0){
		if(is.na(cases[i,"transcript_link"])==FALSE&is.na(cases[i,"mp3_link"])==FALSE){
			###download transcript
			transcript_url<-paste("http://www.oyez.org/sites/default/files",cases[i,"transcript_link"],sep="")
			transcriptSource<-getURL(transcript_url)
			
			if(transcriptSource!=""){
				if(length(unlist(sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//text"),xmlGetAttr, "syncTime")))>0){
					###create directory
			dir.create(paste("/Dropbox/oral_arguments/",cases[i,"year"],"/",cases[i,"docket_number"],"/",sep=""),recursive=TRUE)
			
					###download mp3
					download.file(paste("http://www.oyez.org/",unlist(strsplit(cases[i,"mp3_link"],"api/media/"))[2],sep=""),paste("/Dropbox/oral_arguments/",cases[i,"year"],"/",cases[i,"docket_number"],"/",cases[i,"docket_number"],"_argument.mp3",sep=""),mode="wb",quiet=TRUE)
			
					###get lines
					temp_lines<-cbind(sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//text"),xmlValue),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//text"),xmlGetAttr, "syncTime"))
					colnames(temp_lines)<-c("text","sync_time")
					
					temp_lines<-data.frame(temp_lines)
					temp_lines$text<-as.character(temp_lines$text)
					temp_lines$sync_time<-as.numeric(as.character(temp_lines$sync_time))
					write.csv(temp_lines,"/Dropbox/oral_arguments/temp_lines.csv",row.names=FALSE)
				
					###get turns
					temp_turns<-cbind(sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//turn"),xmlGetAttr, "speaker"),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//turn"),xmlGetAttr, "startTime"),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//turn"),xmlGetAttr, "stopTime"))
					colnames(temp_turns)<-c("speaker","start_time","stop_time")
					write.csv(temp_turns,"/Dropbox/oral_arguments/temp_turns.csv",row.names=FALSE)
				
					###add speaker
					temp_lines<-read.csv("/Dropbox/oral_arguments/temp_lines.csv",as.is=TRUE)
					temp_turns<-read.csv("/Dropbox/oral_arguments/temp_turns.csv",as.is=TRUE)
					speaker_id<-sapply(temp_lines$sync_time,function(x){return(temp_turns[x>=temp_turns$start_time&temp_turns$stop_time>x,"speaker"][1])},USE.NAMES=FALSE)
					temp_lines<-cbind(speaker_id,temp_lines)
				
					###speaker info
					temp_speakers<-cbind(sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//speaker"),xmlValue),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//speaker"),xmlGetAttr, "id"),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//speaker"),xmlGetAttr, "type"),sapply(getNodeSet(xmlTreeParse(transcriptSource, useInternalNodes = TRUE), "//speaker"),xmlGetAttr, "gender"))
					colnames(temp_speakers)<-c("name","speaker_id","type","gender")
					write.csv(temp_speakers,"/Dropbox/oral_arguments/temp_speakers.csv",row.names=FALSE)
					temp_speakers<-read.csv("/Dropbox/oral_arguments/temp_speakers.csv",as.is=TRUE)
					temp_transcript<-merge(temp_lines,temp_speakers,by="speaker_id")
					temp_transcript<-temp_transcript[order(temp_transcript$sync_time),]
					write.csv(temp_transcript,paste("/Dropbox/oral_arguments/",cases[i,"year"],"/",cases[i,"docket_number"],"/",cases[i,"docket_number"],"_transcript.csv",sep=""),row.names=FALSE)
					
					###record done
					cases[i,"transcript"]<-1
				}	
			}
		}
	}
}
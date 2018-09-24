library(XML)
library(stringr)

setwd("C:/Users/VBKobayashi/Documents/publications/waop_submission/peps")

abst<-"abstract"

year1=c(2014)
volume1=c(67)
#issue1=rep(c(1,2,3,4),4)

parserwiley<-function(weblink){
	rawpage<-htmlTreeParse(weblink, useInternal=TRUE)
	all.links<-xpathSApply(rawpage,"//@href")
	present.abs<-sapply(all.links, str_extract, pattern=abst, USE.NAMES=TRUE)
	all.links.abstract<-all.links[!is.na(present.abs)]
	return(as.vector(all.links.abstract))
	}

myfunctionparser<-function(year,volume){

	webpage<-character()
	for(i in 1:length(year)){
		for(j in 1:3){
			webpage<-append(webpage,paste("http://onlinelibrary.wiley.com/doi/10.1111/peps.", year[i], ".", volume[i],".", "issue-",j,"/issuetoc", sep=""))
		}
	}

	regname<-"peps\\.[[:digit:]]{4}\\.[[:digit:]]+\\.issue\\-[1-4]"

	for(k in webpage){

		getalllinks<-unique(parserwiley(k))
			for(l in 1:length(getalllinks)){
				rawpagejournal<-htmlTreeParse(paste("http://onlinelibrary.wiley.com",getalllinks[l], sep="" ), useInternal=TRUE, encoding="utf-8")
				actualabstract<-xpathSApply(rawpagejournal, "//div[@class='para']",xmlValue)
				if(length(actualabstract!=0)){
				writeLines(actualabstract, paste(str_extract(k,regname),"-",l,".txt", sep=""))
			}
		}
	}

}

myfunctionparser(year1, volume1)
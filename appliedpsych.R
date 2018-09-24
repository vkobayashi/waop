library(XML)
library(stringr)
library(RCurl)
library(httr)

setwd("C:/Users/VBKobayashi/Documents/publications/waop_submission/")

volume1<-seq(1,99)
year1<-c(seq(1917,1972),1973,1973,seq(1974,2014))
#issue1<-c(rep(seq(1,6),6),rep(seq(1,4),6),rep(seq(1,6),25),1,2,3,4)

#vol<-GET("http://psycnet.apa.org/journals/apl/")
#vol1<-htmlTreeParse(vol, useInternal=TRUE)
#links2vol<-xpathSApply(vol1,"//ol[@class='bwaVolumes']//li//a[@href]", xmlGetAttr, "href")


colissue<-function(linkvol){
yes<-GET(paste("http://psycnet.apa.org/journals/apl/",linkvol, sep=""))
yes2<-htmlTreeParse(yes, useInternal=TRUE)
#s<-paste("//ol[@class='bwaIssues']//a[contains(@href, 'apl/",linkvol,"/')]", sep="")
links2issue<-xpathSApply(yes2,"//ol[@class='bwaIssues']//a[@href]",xmlGetAttr, "href")
return(links2issue)
}

#colissue(1)

colart<-function(allissue){
                           ai<-GET(paste("http://psycnet.apa.org",allissue, sep=""))
                           ai2<-htmlTreeParse(ai, useInternal=TRUE)
                           #tai<- paste("//a[contains(@href, '",allissue,"')]", sep="")
                           links2abs<-xpathSApply(ai2,"//div[@class='bwaazTitle']//a[@href]",xmlGetAttr, "href")
                           return(links2abs)
}

colabs<-function(linkabs){
                         bi<-GET(paste("http://psycnet.apa.org",linkabs, sep=""))
                         bi2<-htmlTreeParse(bi, useInternal=TRUE, encoding="UTF-8")
                         theabs<-xpathSApply(bi2,"//li[@class='rdAbstract']", xmlValue)
                         return(theabs)
                         } 

yearind<-0
for(i in volume1){
      yearind<-yearind+1
      listissue<-colissue(i)
      issueind=0
      for(k in listissue){
            issueind=issueind+1
            listart<-colart(k)
            absind=0
            for(l in listart){
                  absind=absind+1
                  actualabs<-colabs(l)
                  if(!is.null(unlist(actualabs))){
                       writeLines(actualabs, paste("volume-",i,"year-",year1[yearind],"issue-",issueind,"paper-",absind,".txt", sep=""))
                  }
            }
      }
}      
            


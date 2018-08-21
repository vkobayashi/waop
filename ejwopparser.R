setwd("C:/Users/alluring/Documents/PHD_PROJECTS/submissions/waop_submission/ejwop")

vol2_18=c(1)
issue2_18=c(1,"2-3",4)
year2_18=c(1991)

#### try analysis

### links to abstract

links2abstract<-function(hrefabstract){
rawejwop<-htmlTreeParse(hrefabstract, useInternal=TRUE)
linksejwop<-as.vector(xpathSApply(rawejwop,"//@href"))
ejwopabs<-str_extract(linksejwop, pattern="BresultBean")
return(linksejwop[!is.na(ejwopabs)])
}

### extract the actual abstract

absextract<-function(abslink){
             abspage<-htmlTreeParse(abslink, useInternal=TRUE)
             actualabstract<-xpathSApply(abspage, "//p[@class='first last' or @class='last']",xmlValue)
             return(actualabstract[1])
}



yearind=0
for(j in vol2_18){
      yearind=yearind+1
      for(k in issue2_18){
      abslinks<-links2abstract(paste("http://www.tandfonline.com/toc/pewo20/",j,"/",k, sep=""))
      for(l in 1:length(abslinks)){
      #print(l)
            realabs<-absextract(paste("http://www.tandfonline.com", abslinks[l],sep=""))
            if(!is.null(unlist(realabs))) {
            writeLines(realabs, paste("ejwop_vol-",j,"issue-",k,"number-",l,year2_18[yearind],"txt", sep="."))
            }
      }
      }
}


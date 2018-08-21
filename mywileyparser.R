library(XML)
library(stringr)

#setwd("C:/Users/VBKobayashi/Documents/publications/waop_submission")

################################
# JOOP Crawler
################################

year1<-seq(1975, 2013)
volume1<-seq(48,86)
year2<-c(2014)
volume2<-c(87)

abst<-"abstract"

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
		for(j in 1:2){
			webpage<-append(webpage,paste("http://onlinelibrary.wiley.com/doi/10.1111/joop.", year[i], ".", volume[i],".", "issue-",j,"/issuetoc", sep=""))
		}
	}
	
	regname<-"joop\\.[[:digit:]]{4}\\.[[:digit:]]{2}\\.issue\\-[1-4]"

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

myfunctionparser(year2, volume2)

##############################
# Text Mining on JOOP Abstracts
##############################
library(tm)
library(SnowballC)
library("RKEA")
library(slam)
library(sna)
library(stringr)
library(openNLP)
library(NLP)
library(slam)

#cname<-file.path(".","ejwop")
cname=file.path(getwd(),"submissions","waop_submission","ejwop")
docs2<-Corpus(DirSource(cname, encoding="utf-8"))
docs <-docs2


docs<-tm_filter(docs, FUN=function(x) !any(grep("image original",x)))
docs<-tm_filter(docs, FUN=function(x) !any(grep("paperback",x)))
docs<-tm_filter(docs, FUN=function(x) !any(grep("www",x)))

docs <- tm_map(docs, tolower)

mytransformations<-function(sent){
  sent <- gsub("[[:space:]]?e\\.g\\.[[:space:]]?", " ", sent)
  sent <- gsub("([[:space:]]+)e\\.?t\\.?c([[:space:]]+)", " ", sent)
  sent <- gsub("[[:space:]]?i\\.e\\.[[:space:]]?", " ", sent)
  sent <- gsub("([[:space:]]+)\\–([[:space:]]+)"," ", sent)
  sent <- gsub("\\,"," ", sent)
  sent <- gsub("\\;"," ", sent)
  sent <- gsub("\\:"," ", sent)
  sent <- gsub("\\("," ", sent)
  sent <- gsub("\\)"," ", sent)
  sent <- gsub("“"," ", sent)
  sent <- gsub("”"," ", sent)
  sent <- gsub('"'," ", sent)
  sent <- gsub("[\\.+[:space:]]"," ", sent)
  sent <- gsub("’s", " ", sent)
  sent <- gsub("'s", " ", sent)
  sent <- gsub("s’", " ", sent)
  sent <- gsub("s'", " ", sent)
  sent <- gsub("([[:space:]]+)[[:digit:]]+([[:space:]]+)", " ", sent)
  sent <- gsub("\\-", " ", sent)
  sent <-gsub("<[[:digit:]]+>","", sent)
  sent <-gsub("/","", sent)
#  sent<-gsub("w & o", "w&o", sent)
#  sent<-gsub("work & organizational", "work&organizational", sent)
  sent <- str_trim(sent)
  return(sent)
}

docs<-tm_map(docs, mytransformations)

docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, stripWhitespace)
docs<-tm_map(docs, str_trim)

sent_token_annotator<-Maxent_Sent_Token_Annotator()
word_token_annotator<-Maxent_Word_Token_Annotator()
pos_tag_annotator<-Maxent_POS_Tag_Annotator()

retain_poswords<-function(sent){
                                a2<-annotate(sent, list(sent_token_annotator,word_token_annotator))
                                splitsent<- unlist(strsplit(sent," "))
                                a3<-annotate(sent, pos_tag_annotator,a2)
                                a3w<-subset(a3, type=="word")
                                tags<-sapply(a3w$features,'[[',"POS")
                                
                                if(length(tags)==length(splitsent)){
                                       newsent<-paste(splitsent[tags %in% c("NN","NNS","NNP","NNPS","VBG","JJ")],collapse=" ")
                                       return(newsent)
                                       }
                                #else print("error")
}
                                

docs<-tm_map(docs, retain_poswords)
docs<-tm_filter(docs, FUN=function(x) !is.null(x))


docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, PlainTextDocument)
dtm<-DocumentTermMatrix(docs, control=list(minWordLength=3))

summary(col_sums(dtm))

term_tfidf<-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j,mean) * log2(nDocs(dtm)/col_sums(dtm>0))
summary(term_tfidf)

dtm<-dtm[, term_tfidf >= 0.1]
dtm<-dtm[row_sums(dtm)>0,]
summary(col_sums(dtm))

library("topicmodels")
k <- 20
SEED <- 2010
joop_TM <-
list(VEM = LDA(dtm, k = k, control = list(seed = SEED)),
VEM_fixed = LDA(dtm, k = k,
control = list(estimate.alpha = FALSE, seed = SEED)),
Gibbs = LDA(dtm, k = k, method = "Gibbs",
control = list(seed = SEED, burnin = 1000,
thin = 100, iter = 1000)),
CTM = CTM(dtm, k = k,
control = list(seed = SEED,
var = list(tol = 10^-4), em = list(tol = 10^-3))))

sapply(joop_TM[1:2], slot, "alpha")

sapply(joop_TM, function(x)
mean(apply(posterior(x)$topics,
1, function(z) - sum(z * log(z)))))

Topic <- topics(joop_TM[["VEM"]], 1)

Terms <- terms(joop_TM[["VEM"]], 20)
Terms[,1:5]

which(Topic==19)

class(dtm)
#library(RefManageR)
library(readr)
library(tm)
library(stringr)
library(slam)
library(topicmodels)

load("topics_employability.rda")

###################
### Topic Modeling
##################

library(topicmodels)
library(tm)
docs2<-VCorpus(VectorSource(bib_abstracts))
docs <-docs2

# docs<-tm_filter(docs, FUN=function(x) !any(grep("image original",x)))
# docs<-tm_filter(docs, FUN=function(x) !any(grep("paperback",x)))
# docs<-tm_filter(docs, FUN=function(x) !any(grep("www",x)))
# docs<-tm_filter(docs, FUN=function(x) !any(grep("Abstract = {",content(x),fixed=TRUE)))
# docs<-tm_filter(docs, FUN=function(x) !any(grep("(C) [[:digit:]]{4} Elsevier B.V. All rights reserved.}",content(x))))
#docs <- tm_map(docs, tolower)

sent=docs[[200]]$content
mytransformations<-function(sent){
  sent<-gsub(pattern="[[:digit:]]+","",sent)
  sent<-gsub(pattern="Abstract = {","",sent, fixed=TRUE)
  sent<-gsub(pattern="(C)  Elsevier B.V. All rights reserved.},","",sent, fixed=TRUE)
  sent <- gsub("[[:space:]]?e\\.g\\.[[:space:]]?", " ", sent)
  sent <- gsub("([[:space:]]+)e\\.?t\\.?c([[:space:]]+)", " ", sent)
  sent <- gsub("[[:space:]]?i\\.e\\.[[:space:]]?", " ", sent)
  sent <- gsub("([[:space:]]+)\\?([[:space:]]+)"," ", sent)
  sent <- gsub("\\,"," ", sent)
  sent <- gsub("\\;"," ", sent)
  sent <- gsub("\\:"," ", sent)
  sent <- gsub("\\("," ", sent)
  sent <- gsub("\\)"," ", sent)
  # sent <- gsub("?"," ", sent)
  # sent <- gsub("?"," ", sent)
  sent <- gsub('"'," ", sent)
  sent <- gsub("[\\.+[:space:]]"," ", sent)
  # <- gsub("?s", " ", sent)
  sent <- gsub("'s", " ", sent)
  #sent <- gsub("s?", " ", sent)
  sent <- gsub("s'", " ", sent)
  #sent <- gsub("([[:space:]]+)[[:digit:]]+([[:space:]]+)", " ", sent)
  sent <- gsub("\\-", " ", sent)
  #sent <-gsub("<[[:digit:]]+>","", sent)
  sent <-gsub("/","", sent)
  #  sent<-gsub("w & o", "w&o", sent)
  #  sent<-gsub("work & organizational", "work&organizational", sent)
  sent <- str_trim(sent)
  return(sent)
}

docs<-tm_map(docs, content_transformer(mytransformations))

docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, stripWhitespace)
docs<-tm_map(docs, content_transformer(str_trim))

# sent_token_annotator<-Maxent_Sent_Token_Annotator()
# word_token_annotator<-Maxent_Word_Token_Annotator()
# pos_tag_annotator<-Maxent_POS_Tag_Annotator()

# retain_poswords<-function(sent){
#   a2<-annotate(sent, list(sent_token_annotator,word_token_annotator))
#   splitsent<- unlist(strsplit(sent," "))
#   a3<-annotate(sent, pos_tag_annotator,a2)
#   a3w<-subset(a3, type=="word")
#   tags<-sapply(a3w$features,'[[',"POS")
#   
#   if(length(tags)==length(splitsent)){
#     newsent<-paste(splitsent[tags %in% c("NN","NNS","NNP","NNPS","VBG","JJ")],collapse=" ")
#     return(newsent)
#   }
#   #else print("error")
# }


#docs<-tm_map(docs, retain_poswords)
docs<-tm_filter(docs, FUN=function(x) !is.null(x))


#docs <- tm_map(docs, stemDocument)
#docs <- tm_map(docs, PlainTextDocument)
dtm<-DocumentTermMatrix(docs, control=list(minWordLength=3))

summary(col_sums(dtm))

term_tfidf<-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j,mean) * log2(nDocs(dtm)/col_sums(dtm>0))
summary(term_tfidf)

dtm<-dtm[, term_tfidf >= 0.11]
dtm<-dtm[row_sums(dtm)>0,]
summary(col_sums(dtm))

library("topicmodels")
k <- 100
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

Topic <- topics(joop_TM[["CTM"]], 1)

Terms <- terms(joop_TM[["CTM"]], 10)
Terms[,1:10]

which(Topic==19)

class(dtm)

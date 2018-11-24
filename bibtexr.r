library(RefManageR)
library(readr)
library(tm)
library(stringr)
library(slam)
library(ldatuning)
library(topicmodels)
library(dplyr)
library(stringi)
library(LDAvis)

#setwd("C:/Users/alluring/Downloads")

# bib = ReadBib(file="My EndNote Library Employability Deduplicated BibTeX ABS 9933.bib",
#               check=FALSE)
# d.bibEQL  = as.data.frame(bib)
# 
bib=read_lines("My EndNote Library Employability Deduplicated BibTeX ABS 9933.bib")
bib_abstracts=bib[grepl("Abstract = ", bib)]
bib_title = bib[grepl("title = ", bib)]
bib_publisher = bib[grepl("publisher = ", bib)]
bib_type = bib[grepl("type = ", bib)]
bib_journal = bib[grepl("journal = ", bib)]



#d.bibEQL  = as.data.frame(bib)



###################
### Topic Modeling
##################

library(topicmodels)
library(tm)
library(data.table)
library(stringr)
library(slam)

load("employ_journals_data.RData")
dim(bibfdt) # 9,932    30

bibfdt = bibfdt[!is.na(ABSTRACT),.(JOURNAL,TITLE,YEAR,ABSTRACT)]
dim(bibfdt) # 9,319  30

bibfdt_complete = unique(bibfdt, by=c("TITLE","ABSTRACT"))
dim(bibfdt_complete) # 9295  4

head(bibfdt_complete)
head(bibfdt_complete[,.(TITLE)],20)

bibfdt_complete$ABSTRACT <- tolower(bibfdt_complete$ABSTRACT)

bibfdt_complete$ABSTRACT = gsub(pattern="[^[:alnum:]]", replacement=" ", bibfdt_complete$ABSTRACT)
bibfdt_complete$ABSTRACT = gsub(pattern="¹|²|³", replacement=" ", bibfdt_complete$ABSTRACT)
bibfdt_complete$ABSTRACT = gsub(pattern="â", replacement=" ", bibfdt_complete$ABSTRACT)
bibfdt_complete$ABSTRACT = gsub(pattern="ã", replacement=" ", bibfdt_complete$ABSTRACT)
bibfdt_complete$ABSTRACT = gsub(pattern="å", replacement=" ", bibfdt_complete$ABSTRACT)



bibfdt_complete$ABSTRACT = removeNumbers(bibfdt_complete$ABSTRACT)
bibfdt_complete$ABSTRACT = stripWhitespace(removeWords(bibfdt_complete$ABSTRACT,c("â","NA")))
bibfdt_complete$ABSTRACT = trimws(bibfdt_complete$ABSTRACT)

bibfdt_complete= bibfdt_complete[ABSTRACT!=""]


docs<-VCorpus(VectorSource(bibfdt_complete$ABSTRACT))
#docs <-docs2
# docs<-tm_filter(docs, FUN=function(x) !any(grep("image original",x)))
# docs<-tm_filter(docs, FUN=function(x) !any(grep("paperback",x)))
# docs<-tm_filter(docs, FUN=function(x) !any(grep("www",x)))
# docs<-tm_filter(docs, FUN=function(x) !any(grep("Abstract = {",content(x),fixed=TRUE)))
# docs<-tm_filter(docs, FUN=function(x) !any(grep("(C) [[:digit:]]{4} Elsevier B.V. All rights reserved.}",content(x))))
#docs <- tm_map(docs, tolower)

# sent=docs[[200]]$content
# mytransformations<-function(sent){
#   sent<-gsub(pattern="[[:digit:]]+","",sent)
#   sent<-gsub(pattern="Abstract = {","",sent, fixed=TRUE)
#   sent<-gsub(pattern="(C)  Elsevier B.V. All rights reserved.},","",sent, fixed=TRUE)
#   sent <- gsub("[[:space:]]?e\\.g\\.[[:space:]]?", " ", sent)
#   sent <- gsub("([[:space:]]+)e\\.?t\\.?c([[:space:]]+)", " ", sent)
#   sent <- gsub("[[:space:]]?i\\.e\\.[[:space:]]?", " ", sent)
#   sent <- gsub("([[:space:]]+)\\?([[:space:]]+)"," ", sent)
#   sent <- gsub("\\,"," ", sent)
#   sent <- gsub("\\;"," ", sent)
#   sent <- gsub("\\:"," ", sent)
#   sent <- gsub("\\("," ", sent)
#   sent <- gsub("\\)"," ", sent)
#   # sent <- gsub("?"," ", sent)
#   # sent <- gsub("?"," ", sent)
#   sent <- gsub('"'," ", sent)
#   sent <- gsub("[\\.+[:space:]]"," ", sent)
#   # <- gsub("?s", " ", sent)
#   sent <- gsub("'s", " ", sent)
#   #sent <- gsub("s?", " ", sent)
#   sent <- gsub("s'", " ", sent)
#   #sent <- gsub("([[:space:]]+)[[:digit:]]+([[:space:]]+)", " ", sent)
#   sent <- gsub("\\-", " ", sent)
#   #sent <-gsub("<[[:digit:]]+>","", sent)
#   sent <-gsub("/","", sent)
#   #  sent<-gsub("w & o", "w&o", sent)
#   #  sent<-gsub("work & organizational", "work&organizational", sent)
#   sent <- str_trim(sent)
#   return(sent)
# }

#docs<-tm_map(docs, content_transformer(mytransformations))

docs <- tm_map(docs, removeWords, stopwords("english"))
#docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, stripWhitespace)
docs<-tm_map(docs, content_transformer(str_trim))

docs[[1]]$content

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


docs = tm_filter(docs, FUN=function(x) content(x)!="")
length(docs) # 9295


dtm = DocumentTermMatrix(docs, control=list(removeNumbers=TRUE))

dim(dtm) # 9295 26413
head(dimnames(dtm)[[2]],20)
# save(educ_dtm, file="educ_dtm_big.rda")
# load("educ_dtm_big.rda")

summary(col_sums(dtm))
names(head(sort(col_sums(dtm), decreasing = TRUE),200))

term_tf = col_sums(dtm>0)

summary(term_tf)
dtm = dtm[, term_tf > 1]
dtm=dtm[row_sums(dtm)>0,]
summary(col_sums(dtm))
dim(dtm) # 9295     14782

term_tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i],dtm$j, mean) *
  log2(nDocs(dtm)/col_sums(dtm>0))

summary(term_tfidf)
dtm = dtm[, term_tfidf >= .11]
dtm=dtm[row_sums(dtm)>0,]
summary(col_sums(dtm))
dim(dtm) # 9209     6818
names(head(sort(col_sums(dtm), decreasing = TRUE),200))

dtm$dimnames$Terms[1:50]



# #docs<-tm_map(docs, retain_poswords)
# docs<-tm_filter(docs, FUN=function(x) !is.null(x))
# 
# 
# #docs <- tm_map(docs, stemDocument)
# #docs <- tm_map(docs, PlainTextDocument)
# dtm<-DocumentTermMatrix(docs, control=list(minWordLength=3))
# 
# summary(col_sums(dtm))
# 
# term_tfidf<-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j,mean) * log2(nDocs(dtm)/col_sums(dtm>0))
# summary(term_tfidf)
# 
# dtm<-dtm[, term_tfidf >= 0.11]
# dtm<-dtm[row_sums(dtm)>0,]
# summary(col_sums(dtm))

library("topicmodels")
k <- 50
SEED <- 2010
employa_topic <-
  list(VEM = LDA(dtm, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(dtm, k = k,
                       control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(dtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000,
                                  thin = 100, iter = 1000)),
       CTM = CTM(dtm, k = k,
                 control = list(seed = SEED,
                                var = list(tol = 10^-4), em = list(tol = 10^-3))))


employa_topicCTM =CTM(dtm, k = k,
                      control = list(seed = SEED,
                                     var = list(tol = 10^-4), em = list(tol = 10^-3)))

#save(employa_topicCTM, file="I:\\CJKR\\DATA\\employability_topicmodelsCTM.rda")
load("employability_topicmodelsCTM.rda")

sapply(joop_TM[1:2], slot, "alpha")

sapply(joop_TM, function(x)
  mean(apply(posterior(x)$topics,
             1, function(z) - sum(z * log(z)))))

Topic <- topics(employa_topicCTM[["VEM"]], 1)

Terms <- terms(employa_topicCTM, 10)
Terms[,1:20]

which(Topic==19)

class(dtm)

########################################################
## Inter-topic relationship                            #
########################################################

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  
  temp_frequency <- doc_term
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = col_sums(temp_frequency)
  )
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq )
  
  return(json_lda)
}

attr_corpus_sub <- VCorpus(VectorSource(agg_attr_industry$description[as.numeric(dimnames(attr_dtm_new)$Docs)]))
names(attr_corpus_sub)<- dimnames(attr_dtm_new)$Docs

attr_json <- topicmodels_json_ldavis(fitted = attrTM_gibbs, corpus=attr_corpus_sub, doc_term=attr_dtm_new)
serVis(attr_json)

########################################################
## Cluster job titles                                  #
########################################################

### Jensen-Shannon divergence
jensenShannon <- function(x, y) {
  m <- 0.5 * (x + y)
  sqrt(0.5 * sum(x * log(x/m)) + 0.5 * sum(y * log(y/m)))
}

jobattr_theta <- posterior(attrTM_gibbs)$topics
jobattr_phi <- posterior(attrTM_gibbs)$terms

### PCA based on Jensen-Shannon divergence
jsSammon<-function (phi) 
{
  dist.mat <- proxy::dist(x = phi, method = jensenShannon,convert_similarities = FALSE)
  pca.fit <- stats::cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[, 1], y = pca.fit[, 2])
}

### Choose 5000 job titles
indx <- sample(1:nrow(jobattr_theta),5000)
jobattr_theta_sub <- jobattr_theta[indx,] 

### Compute distances
dist_jobattr <- proxy::dist(x=jobattr_theta_sub, method=jensenShannon, convert_similarities = FALSE)

### Cluster job titles
cluster_jobattr <-hclust(dist_jobattr, method="ward.D")
### Set the number of cluster equal to 100
jobattr_cluCut <- cutree(cluster_jobattr, k=100)

## Select cluster 20
clus100_mat <- jobattr_theta_sub[jobattr_cluCut==20,,drop=FALSE]
agg_attr_industry[rownames(clus100_mat),4]
hclust100 <- hclust(proxy::dist(x= clus100_mat, method=jensenShannon), method="ward.D")

## Dendogram of cluster 20
plot(hclust100, labels=substr(agg_attr_industry[rownames(clus100_mat),4], start=1,  stop =30), cex=.8)




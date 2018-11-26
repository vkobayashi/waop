library(readr)
library(stringr)
library(ldatuning)
library(dplyr)
library(stringi)
library(LDAvis)

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
bibfdt_complete$ABSTRACT = stripWhitespace(removeWords(bibfdt_complete$ABSTRACT,c("NA")))
bibfdt_complete$ABSTRACT = trimws(bibfdt_complete$ABSTRACT)

bibfdt_complete= bibfdt_complete[ABSTRACT!=""]
dim(bibfdt_complete) # 9,295 4

docs<-VCorpus(VectorSource(bibfdt_complete$ABSTRACT))

docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, stripWhitespace)
docs<-tm_map(docs, content_transformer(str_trim))

docs[[1]]$content

docs = tm_filter(docs, FUN=function(x) content(x)!="")
length(docs) # 9295

dtm = DocumentTermMatrix(docs, control=list(stopwords=TRUE))

dim(dtm) # 9295 26413
head(dimnames(dtm)[[2]],20)

summary(col_sums(dtm))
names(head(sort(col_sums(dtm), decreasing = TRUE),200))

term_tf = col_sums(dtm>0)

summary(term_tf)
dtm = dtm[, term_tf > 2]
dtm=dtm[row_sums(dtm)>0,]
summary(col_sums(dtm))
dim(dtm) # 9295     11431

term_tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i],dtm$j, mean) *
  log2(nDocs(dtm)/col_sums(dtm>0))

summary(term_tfidf)
dtm = dtm[, term_tfidf >= .103]
dtm=dtm[row_sums(dtm)>0,]
summary(col_sums(dtm))
dim(dtm) # 9269     5696
names(head(sort(col_sums(dtm), decreasing = TRUE),200))

dtm$dimnames$Terms[1:50]

###################
### Topic Modeling
##################

library("topicmodels")
k <- 50
SEED <- 2010
# employa_topic <-
#   list(VEM = LDA(dtm, k = k, control = list(seed = SEED)),
#        VEM_fixed = LDA(dtm, k = k,
#                        control = list(estimate.alpha = FALSE, seed = SEED)),
#        Gibbs = LDA(dtm, k = k, method = "Gibbs",
#                    control = list(seed = SEED, burnin = 1000,
#                                   thin = 100, iter = 1000)),
#        CTM = CTM(dtm, k = k,
#                  control = list(seed = SEED,
#                                 var = list(tol = 10^-4), em = list(tol = 10^-3))))


employa_topicCTM =CTM(dtm, k = k,
                      control = list(seed = SEED,
                                     var = list(tol = 10^-4), em = list(tol = 10^-3)))

save(employa_topicCTM, file="employability_topicmodelsCTM.rda")
save(dtm,docs,bibfdt_complete, file="employa_dtm_corpus.rda")
load("employability_topicmodelsCTM.rda")
load("employa_dtm_corpus.rda")

Topic <- topics(employa_topicCTM, 1)

Terms <- terms(employa_topicCTM, 10)
Terms[,1:20]

which(Topic==1)

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

#attr_corpus_sub <- VCorpus(VectorSource(agg_attr_industry$description[as.numeric(dimnames(attr_dtm_new)$Docs)]))
employ_corpus = VCorpus(VectorSource(bibfdt_complete$ABSTRACT[as.numeric(dimnames(dtm)$Docs)]))

names(employ_corpus)<- dimnames(dtm)$Docs

employ_json <- topicmodels_json_ldavis(fitted = employa_topicCTM, corpus=employ_corpus, doc_term=dtm)
serVis(employ_json)

########################################################
## Cluster job titles                                  #
########################################################

### Jensen-Shannon divergence
jensenShannon <- function(x, y) {
  m <- 0.5 * (x + y)
  sqrt(0.5 * sum(x * log(x/m)) + 0.5 * sum(y * log(y/m)))
}

employa_theta <- posterior(employa_topicCTM)$topics
employa_phi <- posterior(employa_topicCTM)$terms

### PCA based on Jensen-Shannon divergence
jsSammon<-function (phi) 
{
  dist.mat <- proxy::dist(x = phi, method = jensenShannon,convert_similarities = FALSE)
  pca.fit <- stats::cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[, 1], y = pca.fit[, 2])
}

### Choose 5000 job titles
#indx <- sample(1:nrow(employa_theta),5000)
employa_theta_sub <- employa_theta[indx,] 
employa_theta_sub <- employa_theta 

### Compute distances
dist_employa <- proxy::dist(x=employa_theta_sub, method=jensenShannon, convert_similarities = FALSE)

### Cluster job titles
cluster_employa <-hclust(dist_employa, method="ward.D")
### Set the number of cluster equal to 100
employa_cluCut <- cutree(cluster_employa, k=100)

## Select cluster 20
clus100_mat <- employa_topicCTM_theta_sub[jobattr_cluCut==20,,drop=FALSE]
bibfdt_complete[rownames(clus100_mat),4]
hclust100 <- hclust(proxy::dist(x= clus100_mat, method=jensenShannon), method="ward.D")

## Dendogram of cluster 20
plot(hclust100, labels=substr(bibfdt_complete[rownames(clus100_mat),4], start=1,  stop =30), cex=.8)




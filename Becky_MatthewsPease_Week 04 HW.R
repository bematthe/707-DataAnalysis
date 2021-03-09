##Load Libraries
library(wordcloud)
library(tm)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(cluster)
library(stringi)
library(Matric)
library(tidytext)
library(plyr)
library(ggplot2)
library(factoextra)
library(mclust)
library(dplyr)
library(stringr)

folder <- "C:\\Users\\becky\\Desktop\\FedPapersCorpus"
list.files(path=folder)
list.files(path=folder, pattern=".txt")
filelist <- list.files(path=folder, pattern=".txt")

## Remove gaps
paste(folder, "\\", filelist)
paste(folder, "\\", filelist, sep="")
filelist <- paste(folder, "\\", filelist, sep="")

## Collapse empty lines
lapply(filelist, FUN=readLines)
a <- lapply(filelist, FUN=readLines)
lapply(a, FUN=paste, collapse=" ")
corpus <- lapply(a, FUN=paste, collapse=" ")
gsub(pattern="\\W", replace=" ", corpus)

## Remove stopwords, extra white space, and convert to lower case
corpus2 <- gsub(pattern="\\W", replace=" ", corpus)
corpus2 <- gsub(pattern="\\d", replace=" ", corpus)
corpus2 <- tolower(corpus2)
removeWords(corpus2, stopwords("english"))
gsub(pattern="\\b[A-z]\\b[1]", replace=" ", corpus2)
corpus <- gsub(pattern="\\b[A-z]\\b[1]", replace=" ", corpus2)
stripWhitespace(corpus2)
corpus2 <- stripWhitespace(corpus2)

## Convert to corpus files
dfCorpus <- Corpus(VectorSource(corpus2$V1))
inspect
dfCorpus = Corpus(VectorSource(df$x))
inspect(dfCorpus)

## View files and data
Corpus(DirSource(courpus2))
(numberFedPapers <- length(corpus2))
(summary(corpus2))
(meta(corpus2[[1]]))
DTM <- as.matrix(PapersDTM) 
(numberFedPapers <- length(corpus2))
(summary(corpus2))

## Remove punctuation and numbers
(getTransformations())
(nFedPapersCorpus <- length(corpus2))
## Ignore rare words that appear less than 1% of the time
(minTermFreq <- nFedPapersCorpus * 0.0001)

## Ignore overly common words that appear in more than 50% of the documents
(maxTermFreq <- nFedPapersCorpus * 1)

## Stop words
(STOPS <- stopwords('english'))
corpus(corpus2)

corp <- Corpus(VectorSource(corpus2))
PapersDTM <- DocumentTermMatrix(corp, control = list(
  removePunctuation = T,
  removeNumbers = T,
  tolower = T,
  stemming = T,
  remove_separators = T,
  stopwords = STOPS,
  bounds = list(global = c(minTermFreq, maxTermFreq))))

## Inspect Document Term Matrix
DTM <- as.matrix(PapersDTM)
(DTM[1:11,1:10])

## View word frequencies
WordFreq <- colSums(as.matrix(PapersDTM))
(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])
(Row_Sum_Per_doc <- rowSums((as.matrix(PapersDTM))))

## Create a normalized version of the Document Term Matrix
Papers_M <- as.matrix(PapersDTM)
Papers_M_N1 <- apply(Papers_M, 1, function(i) round(i/sum(i),3))
Papers_Matrix_Norm <- t(Papers_M_N1)

## Look at the original and the norm
(Papers_M[c(1:11),c(1000:1010)])
(Papers_Matrix_Norm[c(1:11),c(1000:1010)])

## Convert to and view matrix
Papers_dtm_matrix = as.matrix(PapersDTM)
str(Papers_dtm_matrix)
(Papers_dtm_matrix[c(1:11),c(2:10)])
Papers_DF <- as.data.frame(as.matrix(PapersDTM))
str(Papers_DF)
(Papers_DF$abolit)
(nrow(Papers_DF))

## Create example word clous for Hamilton, Madison, and disputed papers
DisputedPapersWC <- wordcloud(colnames(Papers_dtm_matrix), Papers_dtm_matrix[11, ])
(head(sort(as.matrix(PapersDTM[11,], decreasing = TRUE), n=50)))
HamiltonPapersWC <- wordcloud(colnames(Papers_dtm_matrix), Papers_dtm_matrix[12:62, ])
MadisonPapersHW <- wordcloud(colnames(Papers_dtm_matrix), Papers_dtm_matrix[63:77, ])

## Distance Measure
m <- Papers_dtm_matrix
m_norm <- Papers_Matrix_Norm
distMatrix_E <- dist(m, method="euclidean")
distMatrix_M <- dist(m, method="manhattan")
distMatrix_C <- dist(m, method="cosine")
distMatric_c_norm <- dist(m_norm, method="cosine")
groups_E <- hclust(distMatrix_E, method="ward.D")
rect.hclust(groups_E, k=2)
plot(groups_E, cex=0.5, font=22, hang=1, main="HAC Cluster Dendrogram with Euclidean Similarity")

## Cosine Similarity 
rect.hclust(groups_E, k=2)
groups_C <- hclust(distMatrix_C, method="ward.D")
plot(groups_C, cex=0.5, font=22, hang=1, main="HAC Cluster Dendrogram with Cosine Similarity")

## Cosine Similarity for Normalized Matrix
rect.hclust(groups_C, k=2)
rect.hclust(groups_C_n, k=2)
distMatric_c_norm <- dist(m_norm, method="cosine")
groups_C_n <- hclust(distMatric_c_norm, method="ward.D")
plot(groups_C_n, cex=0.5, font=22, hang=1, main="HAC Cluster Dendrogram with Cosine Similarity with Norm")

## Use k-means clustering methods
X <- m_norm
k2 <- kmeans(X, centers = 2, nstart = 100, iter.max = 50)
str(k2)

k3 <- kmeans(X, centers = 7, nstart = 50, iter.max = 50)
str(k3)

## K-means visualizations
distance1 <- get_dist(X, method = "manhattan")
fviz_dist(distance1, gradient = list(low = "#00AFBB", mid= "white", high = "#FC4E07"))

distance2 <- get_dist(X, method = "euclidean")
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid= "white", high = "#FC4E07"))

distance3 <- get_dist(X, method = "spearman")
fviz_dist(distance3, gradient = list(low = "#00AFBB", mid= "white", high = "#FC4E07"))

str(X)

kmeansFIT_1 <- kmeans(X, centers=4)
summary(kmeansFIT_1)
kmeansFIT_2 <- kmeans(X, centers=3)
summary(kmeansFIT_2)

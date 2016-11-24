Data <- file.path("/home/nlp/Documents/Documnet_Data/")
dir(Data)

#Load_Data
library(tm)
library(SnowballC)
docs <- Corpus(DirSource(Data))
summary(docs)

#Inspect
inspect(docs[4])


#Preprocessing
docs <-tm_map(docs,removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs,tolower)
docs <- tm_map(docs,removeWords,stopwords("english"))
#docs <- tm_map(docs,stemDocument)
docs <- tm_map(docs,PlainTextDocument)

#dtm
dtm <- DocumentTermMatrix(docs)
inspect(dtm)

#tdm
tdm <- TermDocumentMatrix(docs)
inspect(tdm)

#frequency 
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)

#removeSparse
dtms <- removeSparseTerms(dtm,0.1)
inspect(dtms)

#Word_Fre
freq[head(ord)]
head(table(freq),10)

freq <- colSums(as.matrix(dtm))
freq
freq <- sort(colSums(as.matrix(dtm)) , decreasing = TRUE)
head(freq,10)

findFreqTerms(dtm,lowfreq =50)

wf = data.frame(word = names(freq), freq=freq)
head(wf,19)

#Clustering
library(cluster)
d <- dist(t(dtm), method = "euclidian")
fit <- hclust(d=d, method = "ward.D")
fit
plot(fit,hang = -3)

#Kmeans_Clustering
library(fpc)
d <- dist(t(dtm) , method = "euclidian")
kfit <- kmeans(d,2)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   


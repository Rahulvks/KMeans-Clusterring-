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
kfit <- kmeans(d,3)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   



#Elbow Method
set.seed(123)
k.max <- 5
data <- d
wss <- sapply(1:k.max,function(k){kmeans(data, k, nstart=5 )$tot.withinss})

plot(1:k.max, wss,pch = 19, frame = FALSE)
abline(v = 3, lty =2)

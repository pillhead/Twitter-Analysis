rm(list=ls())
#install.packages(c("devtools", "rjson", "bit64", "httr"))
#install_github("twitteR", username="geoffjentry")
library(devtools)
library(twitteR)
library(tm)
library(wordcloud)
library(ggplot2)
library(fpc)
library(qdap)
library(igraph)



#Application Details
APIKey="YOUR_TWITTER_API_KEY"
APISecret="YOUR_TWITTER_API_SECRET"
AccessToken="YOUR_TWITTER_ACCESS_TOKEN"
AccessTokenSecret="YOUR_TWITTER_ACCESS_TOKEN_SECRET"
Auth<-setup_twitter_oauth(APIKey,APISecret,AccessToken,AccessTokenSecret)

#Fetching tweets of input Hashtag 
fetchHashtag <- function(hashtag,number) {
  tweets <- searchTwitter(hashtag, number, lang="en")
  return(tweets)
}


makeCorpus <- function(text){ #Function for making corpus and cleaning the tweets fetched
  twitterdf <- do.call("rbind", lapply(text, as.data.frame)) #store the fetched tweets as a data frame
  twitterdf$text <- sapply(twitterdf$text,function(row) iconv(row, "latin1", "ASCII", sub=""))#Removing emoticons from tweets
  twitterCorpus <- Corpus(VectorSource(twitterdf$text)) #Creating Corpus
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) #function to replace a pattern to white space using regex
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)") #match rt or via
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "@\\w+") #match @
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "[ \t]{2,}") #match tabs
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "[ |\n]{1,}") #match new lines
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "^ ") #match white space at begenning
  twitterCorpus <- tm_map(twitterCorpus, toSpace, " $") #match white space at the end
  twitterCorpus <- tm_map(twitterCorpus, PlainTextDocument)
  twitterCorpus <- tm_map(twitterCorpus, removeNumbers)
  twitterCorpus <- tm_map(twitterCorpus, removePunctuation)
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "http[[:alnum:]]*") #remove url from tweets
  twitterCorpus <- tm_map(twitterCorpus,removeWords,stopwords("en"))
  twitterCorpus <- tm_map(twitterCorpus, content_transformer(tolower))
  return(twitterCorpus)
}

#for (i in 1:100){
  #cat(paste("[[", i, "]] ", sep=""))
  #writeLines(strwrap(corp[[i]], width=73))
#}

#Wordcloud
makeWordcloud<-function (getText){ #plotting wordcloud
  twicorpus<-makeCorpus(getText)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf))) #Create TDM
  matrix<-as.matrix(myTdm)
  wordFreq <- sort(rowSums(matrix), decreasing=TRUE)#find frequency of words and sorting them in descending
  set.seed(375) 
  grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
  wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=grayLevels)
}




freqPlot<-function (getText){ #frequency plot of word count
  twicorpus<-makeCorpus(getText)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf)))
  matrix<-as.matrix(myTdm)
  termFrequency <- rowSums(matrix)
  termFrequency <- subset(termFrequency, termFrequency>=10)
  df <- data.frame(term=names(termFrequency), freq=termFrequency)
  ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + coord_flip()
}


#Clustering
hCluster<-function (content){ #hierarchical clustering 
  twicorpus<-makeCorpus(content)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf)))
  myTdm2 <- removeSparseTerms(myTdm, sparse=0.98) #removing sparse terms
  m2 <- as.matrix(myTdm2)
  distMatrix <- dist(scale(m2)) #calculating distance between terms
  fit <- hclust(distMatrix, method="ward.D") #clustering terms
  plot(fit)
  rect.hclust(fit, k=5) #cutting the tree into 5 clusters
  (groups <- cutree(fit, k=5))
}


kMeans<-function (content){ #k-means clustering
  twicorpus<-makeCorpus(content)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf)))
  myTdm2 <- removeSparseTerms(myTdm, sparse=0.98)
  m2 <- as.matrix(myTdm2)
  m3 <- t(m2) # creating transpose of matrix
  set.seed(122)
  k <- 8
  kmeansResult <- kmeans(m3, k)
  round(kmeansResult$centers, digits=3) #cluster centers
  for (i in 1:k) { #printing 15 terms of each cluster
    cat(paste("cluster ", i, ": ", sep=""))
    s <- sort(kmeansResult$centers[i,], decreasing=T)
    cat(names(s)[1:15], "\n")
  }
}



kMediod<-function (content){
  twicorpus<-makeCorpus(content)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf)))
  myTdm2 <- removeSparseTerms(myTdm, sparse=0.95)
  m2 <- as.matrix(myTdm2)
  m3 <- t(m2)
  pamResult <- pamk(m3, metric="manhattan")
  return(pamResult)
}


#Sentiment Analysis
tSentimen<-function (content){
  twicorpus<-makeCorpus(content)
  dataframe<-data.frame(text=unlist(sapply(twicorpus, `[`, "content")), stringsAsFactors=F) # storing corpus as data frame
  (poldat <- with(dataframe, polarity(text))) #getting polarity of the tweets
}

#Just for example, edit the hashtag and number of tweets to fetch
get<-fetchHashtag("#CocaCola",200) # fetching tweets of coca cola
freqPlot(get) #creating frequency plots
corp<-makeCorpus(get) #creating corpus
makeWordcloud(get) #creating wordcloud
hCluster(get) #hierarchical clustering 
kMeans(get) #k-means clustering
getSentiment1<-tSentimen(get) #fetching sentiment polarity
table(getSentiment1$all$polarity>0)
plot(getSentiment1) #polarity plot
plot(getSentiment1$all$polarity,ylab="All Polarity",xlab="Documents",pch=20,cex=1,col="darkblue") #individual polarity plot
mean(getSentiment1$all$polarity)

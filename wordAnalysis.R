library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)
library(rtweet)
library(maps)
library(tm)
library(wordcloud) 
library(syuzhet)
library(reactable)
library(wordcloud2)
library(stringr)
library(data.table)
library(tidytext)
library(topicmodels)

api_key <- "KeuRqYmscMaYYm3JoPp3Izl5y"
api_secret_key <- "69z1ytMqwCbcBRqqowFRbcHcipi7U6qQFhItt2geVAK8gBT5nJ"
access_token <- "4052336744-mAZn7yMEvijXVpk86Ap1kWXGVveSluAQbG6Q7oI"
access_token_secret <- "t0u0jl1qun5hmHR2wsRueSFwuOjkKcrQTAZW8BhrOSO65"

## authenticate link via browser
token <- create_token(
  app <- 'meyennTwit',
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

## check token ok
get_token()

### AIM: Comparison of Trump and Bidens Tweets
### 21/7/2020
### Dr Andrew Meyenn
### www.ajmblogger.com
### meyenna@gmail.com
### https://github.com/ameyenn/USElection

#### SECTION 1: define functions and Initialise

BigramTokenizer <- function(x,n) unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)

clean_tweets <- function(x) { #RDRR Stackoverflow - better than the TM mappings
  x %>%
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    str_replace_all("&amp;", "and") %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("^RT:? ") %>%
    str_remove_all("@[[:alnum:]]+") %>%
    str_remove_all("#[[:alnum:]]+") %>%
    str_replace_all("\\\n", " ") %>%
    str_to_lower() %>%
    str_trim("both")
}

removeURL <- function(x) gsub("http[^[:space:]]*", "", x) 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
removeUsername <- function(x) gsub("@[^[:space:]]*", "", x) 
removeSingle <- function(x) gsub(" . ", " ", x) 

mystopwords <- c((stopwords('english')),c("https", "t.co", "it's")) #or read in your stop word list

path<-"C:/Users/ameye/OneDrive/R" #Set your own path if using.
setwd(path)

### END INTIALISE

### SECTION 2: Get Data, Clean and create Coprus
### NOTE: the way the data was collected is shown below but has been commented out.

# searchString <- "@JoeBiden" #set the string to search on, here @JoeBiden or @RealDonaldTrump

### Use one of the following to retrieve the inital data
### The difference is that get_time_lines goes back in time to meet the number requested limit 3200
### search_tweets returns upto 18000 tweets over the last 6 to 8 days.

# df<- search_tweets(searchString, n = 5000,include_rts = FALSE, lang="en")
# df <- get_timeline(searchString, n= 1000)

### write Four files for reproducability using fwrite from datatable
### copy the files from the GitHub repository at https://github.com/ameyenn/USElection
# fwrite(TrumpJuly21SearchTweets, "C:/Users/ameye/OneDrive/R/ML/TrumpJuly21SearchTweets.csv")
# fwrite(TrumpJuly21GetTimeLine, "C:/Users/ameye/OneDrive/R/ML/TrumpJuly21GetTimeLine.csv")
# fwrite(BidenJuly21SearchTweets, "C:/Users/ameye/OneDrive/R/ML/BidenJuly21SearchTweets.csv")
# fwrite(BidenJuly21GetTimeLine, "C:/Users/ameye/OneDrive/R/ML/BidenJuly21GetTimeLine.csv")

### Read in the specific file for processing.

df<-read.csv(file.choose(), header=TRUE) #allows you to navigate to file location
dfCopy <- df # used to do the ngrams, need the context
reactable(df, searchable = T, filterable=T) #nice way to review the data, searchable and fiterable

### To isolate only Tweets set to filter out the following
df <- df[df$is_retweet == F,] #keep tweets that have not been retweeted
df <- df[is.na(df$reply_to_status_id), ] #keep tweets left not replied to

### Start cleaning the text column, some double is fine.

df$text<- clean_tweets(df$text) #the function seems to work well, there is double up.
df$text<- removeWords(df$text, mystopwords) #can remove stopwords using this call also.

#https://rpubs.com/Manorama/442299 -Reference: very clean code and function definitions

### create the Corpus or VCorpus, either will do. NOTE the corpus just holds the text column data

w <- VCorpus(VectorSource(df$text))
w <- tm_map(w, content_transformer(removeNumPunct))
w <- tm_map(w, removeNumbers)
w <- tm_map(w, content_transformer(removeURL))
w <- tm_map(w, removePunctuation)
w <- tm_map(w, content_transformer(tolower))
w <- tm_map(w, removeWords, mystopwords)
w <- tm_map(w, stripWhitespace)
w <- tm_map(w, content_transformer(removeUsername))
w <- tm_map(w, content_transformer(removeSingle))

### can get the text as a data frame in case you are wondering!
#dfConverted<-data.frame(text=unlist(sapply(w, `[`, "content")),stringsAsFactors=F)

tdm <- TermDocumentMatrix(w)
dtm <- DocumentTermMatrix(w) #used in topic modelling
###can get the tdm as a dataframe and also dtm
#dataframeTDM <- tidy(tdm)


#### END SECTION 2

#### SECTION 3 Processing using standard packages and methods

### 3a Check word frequencies

frequency <- findFreqTerms(tdm, lowfreq=15)
frequency

### 3b Plot the words with frequencies greater than limit unsorted
freq <- rowSums(as.matrix(tdm))
limit <- 5
freq <- subset(freq, freq >= limit)
dfreq <- data.frame(term = names(freq), freq = freq)
spoint <- 20
ggplot(dfreq[dfreq$freq>spoint,], aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()

### 3c Look at correlated word associations
corLimit <- 0.25
term<- "biden"
findAssocs(tdm, term, corLimit)
term<-"perfect"
findAssocs(tdm, term, corLimit)
term<-"obama"
findAssocs(tdm, term, corLimit)

#I have left this hear to help get the Rgraphviz package working for you.
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
#browseVignettes("Rgraphviz")

library("Rgraphviz") #allows the ploting of word connections, have fun.
plot(tdm, term =  findFreqTerms(tdm, lowfreq=20), corThreshold = 0.10, weighting = T) 

### 3d Plot wordclouds and barplot of word frequency sorted
m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = T)
words <- as.data.frame(word.freq)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,random.order = F)
# to use wordcloud2 need word, freq and that order
words$word <- rownames(words) #new col = rownames
words<-words[c(2,1)]          #interchance the cols
names(words)[1]<-"word"       #change the col names
names(words)[2]<-"freq"       #phew - must be an easier tricky way!

wordcloud2(words[words[,2]>3,], size=3, color='random-dark')

# we can plot these word frequencies also
t <- head(words, 10)
barplot(t[,1], names=t[,2], las=2)


### 3e Sentiment
### simple sentiment barplot of range of emotions
sentText <- get_nrc_sentiment(words$word)
a<-as.data.frame(sort(colSums(sentText)))
barplot(a[,1], names=row.names(a), las=2)

### Plot seniment change over time
s_v <- get_sentences(words$word)
s_v_sentiment <- get_sentiment(s_v)
plot(
  s_v_sentiment, 
  type="l", 
  main= searchString,
  xlab = "Time", 
  ylab= "Emotional Change +/-"
)
abline(h=0)

#investigate word groups, Not done
#pos <- which(sentText$positive > 0)
#s_v[pos]
#table(s_v[pos])

###  3f Cluster Plot
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2)) #note need to scale central mean
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6) # draw 6 groupings

### 3g Topic Modelling

plot(tdm, terms = findFreqTerms(tdm, lowfreq = 6)[1:25], corThreshold = 0.5)

#dtm <- as.DocumentTermMatrix(tdm) #this line errors sometimes, mystery.
rowTotals <- apply(dtm , 1, sum)
dtm2   <- dtm[rowTotals> 0, ] #leave out 0 rows 
lda <- LDA(dtm2, k = 6) # find n topics
term <- terms(lda, 4) # first 4 terms of every topic
term

### 3h Nework Graph
### To make sensible graph you will need a very small sample, say 1% per 100, need to play
n<-0.003
size<-nrow(df)*n #take small random sample
s<-sample(1:nrow(df), size, replace=FALSE)
spl<-df[s,]
net <- network_data(spl, "mention, retweet, reply")
attr(net, "idsn")
netStats <- network_graph(spl)
plot(netStats, vertex.size=1)

### 3i look at N-grams, in particular 3n 

#wDF<-data.frame(text=unlist(sapply(w, `[`, "content")), stringsAsFactors=F) #gets the cleaned text
#text<-as.character(wDF)
### do a little bit of cleaning of the dfCopy dataframe, we only need the text
text<- clean_tweets(dfCopy$text)
text<- removeWords(text, mystopwords)
ngram <- 3 #set size of the word group
ngList = BigramTokenizer(text, ngram) # get the set of 3 word groups
x <- as.data.frame(sort(table(ngList),decreasing=T)) #use table to get the counts, set as a df
x$ngList<-as.character(x$ngList) #make sure not blessed factors
head(x, 10)

### Plot wordclourd
wordcloud2(x[x$Freq>1,], size=0.5, color='random-dark') #single most informative visualisation!

### Plot barchart
ggplot(head(x,15), aes(reorder(ng,Freq), Freq)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")


### 3j Show location of tweets reference Reference https://rtweet-workshop.mikewk.com/#26
### Location in USA as state or set to world
df <- lat_lng(df)
## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
## plot lat and lng points onto state map
with(df, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))


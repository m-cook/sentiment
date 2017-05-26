#to access the elements of the list use list[[tweet no]][word no. in that tweet]
library(twitteR)
library(RColorBrewer)
#FROM TWITTER
api_key <- "fvz8l6kpyJ92RY4FSGt4uxPqX"
api_secret <- "Bd0pqxTouVK3Wh4s3Yff1JFiObvQDFsh8zklAJgUdVkoZvWyZm"
api_access_token <- "1667442678-jU68MV3aCoWmMTT9BiGMcWbC0kLEpuR88vW6pVN"
api_access_token_secret <- "8Yx728z6jZhCJqvXPDzefd2xq2vVIsgi4AcZIw0jaNu65"
setup_twitter_oauth(api_key,api_secret,api_access_token,api_access_token_secret)
tweet <- searchTwitteR("Kejriwal",n=3200,lang = 'en')
#creates the empty vector`
l = length(tweet)
tweeter <- vector(mode = "character", length = l)
df <- do.call("rbind", lapply(tweet, as.data.frame))
write.csv(df, file = "extract_tw.csv")
# Extract the text from each tweet status
for (i in 1:3200) tweeter[i] <- tweet[[i]]$getText()
tweeter
#FROM CSV
 extract_tw=read.csv("extract_tw.csv")
 tweeter=as.vector(extract_tw$text)
# Prepare the above text for sentiment analysis
# Remove @RT 
tweeter = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweeter)
# Then remove all "@people"
tweeter = gsub("@\\w+", "", tweeter)

library(tm)
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweeter))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus
#Stemming and Stem Completion
#stem words
myCorpus <- tm_map(myCorpus, stemDocument)
stemCompletion2 <- function(x, dictionary) {
x <- unlist(strsplit(as.character(x), " "))
x <- x[x != ""]
x <- stemCompletion(x, dictionary=dictionary)
x <- paste(x, sep="", collapse=" ")
PlainTextDocument(stripWhitespace(x))}
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
#frequent words
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 100)
freq_term <- data.frame(term = names(term.freq), freq = term.freq)
#ggplot
library(ggplot2)
plot=ggplot(freq_term, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))
#REPORT IN PPT
library('ReporteRs')
# Create a new powerpoint document
doc <- pptx()
# Add a new slide into the ppt document 
doc <- addSlide(doc, "Two Content" )
# add a slide title
doc<- addTitle(doc, "Editable vector graphics format versus raster format" )
boxplotFunc<- function(){
      ggplot(freq_term, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

      }
# Add an editable box plot
doc <- addPlot(doc,boxplotFunc, vector.graphic = TRUE )
#wordcloud
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
# plot word cloud
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 50,random.order = F, colors = pal)
#REPORT
pdf(file ="gg.pdf")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 50,random.order = F, colors = pal)
ggplot(freq_term, aes(x=term, y=freq)) + geom_bar(stat="identity")+ xlab("Terms2") + ylab("Count2") + coord_flip()+   theme(axis.text=element_text(size=7))
dev.off()


#Topic modeling
dtm <- as.DocumentTermMatrix(tdm)
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]  #remove all docs without words
library(topicmodels)
lda <- LDA(dtm.new, k = 8) 
# find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

##topics <- topics(lda) # 1st topic identified for every document (tweet)
#topics <- data.frame(date=as.IDate(tweets.df$created), topic=topics)
#ggplot(topics, aes(date, fill = term[topic])) +
# geom_density(position = "stack
require(devtools)
install_github("sentiment140", "okugami79")
# sentiment analysis
library(sentiment)
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)
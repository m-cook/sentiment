myCorpus <- Corpus(VectorSource(tweeter))
#mystopwords <- c((stopwords('english'),c("use","used","via","amp","big","u","im")))
myCorpus <- tm_map(myCorpus,removeWords,stopwords('english'))
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
tdm
#inspect frequent terms 
(freq.terms <- findFreqTerms(tdm, lowfreq = 15))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 30)
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
# plot word cloud
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10,
          random.order = F, colors = pal)

class(stopwords('english'))
findAssocs(tdm, "trump", 0.2)
findAssocs(tdm, "muslim", 0.2)
library(graph)
library(Rgraphviz)
plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)
freq.terms
#topic modelling
dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)
lda <- LDA(dtm, k = 15) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

library(data.table)
topics <- topics(lda) # 1st topic identified for every document (tweet)
topics <- data.frame(date=as.IDate(tweet$created), topic=topics)
ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")
class(topics)
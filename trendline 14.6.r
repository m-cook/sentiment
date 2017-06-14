getTermMatrixSentiment <- function() {
  tw=read.csv("extract_tw_oyo.csv")
  tweeter = tw$text
  dates=tw$created
  dates <- as.factor(strftime(dates, format="%m %d %y"))
  tweeter = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweeter)
  # Then remove all “@people”
  tweeter = gsub("@\\w+", "", tweeter)
  # Then remove all the punctuation
  tweeter = gsub("[[:punct:]]", "", tweeter)
  # Then remove numbers, we need only text for analytics
  tweeter = gsub("[[:digit:]]", "", tweeter)
  # the remove html links, which are not required for sentiment analysis
  tweeter = gsub("https\\w+","", tweeter)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweeter = gsub("[ \t]{2,}", "", tweeter)
  
  
  #tweeter = gsub("\\s+|\\s+$", "", tweeter)
  
  catch.error = function(x)
  {
    # let us create a missing value for test purpose
    y = NA
    # try to catch that error (NA) we just created
    catch_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(catch_error, "error"))
      y = tolower(x)
    # check result if error exists, otherwise the function works fine.
    return(y)
  }
  
  
  
  # Now we will transform all the words in lower case using catch.error function we just created above and with sapply function
  tweeter = sapply(tweeter, catch.error)
  
  # Also we will remove NAs, if any exists, from bjp_txt (the collected and refined text in analysis)
  tweeter = tweeter[!is.na(tweeter)]
  #to remove the emoticons
  tweeter = iconv(tweeter, "latin1", "ASCII", sub="")
  tweeter = gsub("RSVP", "", tweeter)
  
  #to remove first and last spaces from the text
  tweeter <- trimws(tweeter, which = c("both", "left", "right"))
  
  require(devtools)
  # sentiment analysis
  library(sentimentr)
  sentiments <- sentiment(tweeter)
  sentiment_matrix=as.data.frame(sentiments)
  #write.csv(sentiment_matrix,"senti_matrix.csv")
  senti_matrix=table(sentiments$sentiment)
  sentiment_table=aggregate(sentiment_matrix$sentiment , by=list(Category=sentiment_matrix$element_id), FUN=sum)
  names(sentiment_table )=c("tweet","sentiment")
  positive_tw <- subset(sentiment_table, sentiment>0)
  negetive_tw <- subset(sentiment_table, sentiment<0)
  neutral_tw <- subset(sentiment_table, sentiment==0)
  tweetfeed=data.frame(sentiment_table$tweet,tweeter,dates)
  names(tweetfeed)=c("id","tweet","date")
  
  
  
  library(dplyr)
  positive_tweets <- merge(tweetfeed, positive_tw, by.x = "id", by.y = "tweet")
  set.seed(1)
  pos=positive_tweets %>% group_by(date) %>%summarise(no_rows = length(date))
  
  negetive_tweets <- merge(tweetfeed, negetive_tw, by.x = "id", by.y = "tweet")
  set.seed(1)
  neg=negetive_tweets %>% group_by(date) %>%summarise(no_rows = length(date))
  zz <- merge(pos, neg, by.x = "date", by.y = "date",all = TRUE)
  zz[is.na(zz)] <- 0
  pos=data.frame(zz$date,zz$no_rows.x)
  names(pos)=c("date","no_rows")
  neg=data.frame(zz$date,zz$no_rows.y)
  names(neg)=c("date","no_rows")
  neutral_tweets <- merge(tweetfeed, neutral_tw, by.x = "id", by.y = "tweet")
  sentiment_count=data.frame(c("positive tweets","negetive tweets","neutral tweets"),c(nrow(positive_tweets),nrow(negetive_tweets),nrow(neutral_tweets)))
  names(sentiment_count)=c("Sentiment","Count")
  
  #for freq plot
  #pos
  myCorpus_pos <- Corpus(VectorSource(positive_tweets$tweet))
  tdm_pos <- TermDocumentMatrix(myCorpus_pos,control = list(wordLengths = c(4, 20)))
  term.freq_pos <- rowSums(as.matrix(tdm_pos))
  term.freq_pos <- subset(term.freq_pos, term.freq_pos >= 10)
  #neg
  myCorpus_neg <- Corpus(VectorSource(negetive_tweets$tweet))
  tdm_neg <- TermDocumentMatrix(myCorpus_neg,control = list(wordLengths = c(4, 20)))
  term.freq_neg <- rowSums(as.matrix(tdm_neg))
  term.freq_neg <- subset(term.freq_neg, term.freq_neg >= 10)
  
  return(list(sentiment_count,positive_tweets,negetive_tweets,pos,neg,term.freq_pos,term.freq_neg))
}
terms=getTermMatrixSentiment()

#Comparative line
data <- data.frame(terms[[4]]$date,terms[[4]]$no_rows,terms[[5]]$no_rows)
p <- plot_ly(data, x = ~terms[[4]]$date, y = ~terms[[4]]$no_rows, name = 'Positive Tweets', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~terms[[5]]$no_rows, name = 'Negative Tweets', mode = 'lines+markers') %>%
  layout(title = "Distribution of tweets",
         xaxis = list(title = "Time (hrs)"),
         yaxis = list(title = "No. of Tweets")
  )

data_neg <- data.frame(names(terms[[7]]),terms[[7]])
names(data_neg)=c("Term","Frequency")
data_neg <- data.frame(data_neg$Term,data_neg$Frequency)
names(data_neg)=c("Term","Frequency")
fn <- plot_ly(data_neg, x=~data_neg$Term,y=~data_neg$Frequency,
              type = "bar"
)%>%
  layout(title = "Frequency plot of Negetive tweets",
         xaxis = list(title = " "),
         yaxis = list(title = "Frequency")
  )

data_pos <- data.frame(names(terms[[6]]),terms[[6]])
names(data_pos)=c("Term","Frequency")
data_pos <- data.frame(data_pos$Term,data_pos$Frequency)
names(data_pos)=c("Term","Frequency")
fp <- plot_ly(data_pos, x=~data_pos$Term,y=~data_pos$Frequency,
              type = "bar"
)%>%
  layout(title = "Frequency plot of Positive tweets",
         xaxis = list(title = " "),
         yaxis = list(title = "Frequency")
  )

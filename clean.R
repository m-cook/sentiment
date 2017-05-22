#to access the elements of the list use list[[tweet no]][word no. in that tweet]
library(tm)
library(twitteR)
library(base64enc)
library(RCurl)
library(ROAuth)
#library(httr)
library(devtools)
library(SnowballC)
library(RColorBrewer)

api_key <- "fvz8l6kpyJ92RY4FSGt4uxPqX"
api_secret <- "Bd0pqxTouVK3Wh4s3Yff1JFiObvQDFsh8zklAJgUdVkoZvWyZm"
api_access_token <- "1667442678-jU68MV3aCoWmMTT9BiGMcWbC0kLEpuR88vW6pVN"
api_access_token_secret <- "8Yx728z6jZhCJqvXPDzefd2xq2vVIsgi4AcZIw0jaNu65"
setup_twitter_oauth(api_key,api_secret,api_access_token,api_access_token_secret)
#setup_twitter_oauth(api_key,api_secret,NULL,NULL)
#tweet <- searchTwitter("#IndiaFightsCorruption",n=100)
tweet <- searchTwitteR("Kejriwal",n=1000,lang = 'en',since = '2017-01-01' ,until = '2017-05-20')
#creates the empty vector`
l = length(tweet)
tweeter <- vector(mode = "character", length = l)

df <- do.call("rbind", lapply(tweet, as.data.frame))
write.csv(df, file = "wip2.csv")
# Extract the text from each tweet status
for (i in 1:1000) tweeter[i] <- tweet[[i]]$getText()
tweeter
# Now we will prepare the above text for sentiment analysis
# First we will remove retweet entities from the stored tweets (text)
tweeter = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweeter)
# Then remove all "@people"
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
# if anything else, you feel, should be removed, you can. For example "slang words" etc using the above function and methods.

# Since there can be some words in lower case and some in upper, we will try to eredicate this non-uniform pattern by making all the words in lower case. This makes uniform pattern.

# Let us first define a function which can handle "tolower error handling", if arises any, during converting all words in lower case.
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

#write the proper dataset after data cleaning
#df1 <- do.call("rbind", lapply(tweeter, as.data.frame))
#write.csv(df1, file = "wip_2k16.csv")
tweeter
class(tweeter)
corp <- Corpus(VectorSource(tweeter))
mystopwords <- c(setdiff(stopwords('en'),c("use","used","via","amp","big")))
corp <- tm_map(corp,removeWords,mystopwords)
tdm <- DocumentTermMatrix(corp, control = list(wordLenths = c(1,Inf)))
tdm
#inspect frequent terms 
freq.terms <- findFreqTerms(tdm, lowfreq = 15)
freq.terms
term.freq <- rowSums(as.matrix(tdm)
                     term.freq <- subset(term.freq, term.freq >= 15)
                     df <- data.frame(term = names(term.freq) , freq = term.freq)
                     tdm[1,1]
                     library(ggplot2)
                     ggplot(df,aes(x=term , y = freq)) + geom_bar(stat = "identity")+xlab("Terms") + ylab("Count") + coord_flip() + theme(axis.text = element_text(size = 7))
                     m <- as.matrix(tdm)
                     m
                     names(term.freq)
                     word.frrq <- sort(rowSums(m),decreasing = T)
                     word.frrq
                     pal <- brewer.pal(9,"BuGn")[-(1:4)]
                     library(wordcloud)
                     wordcloud(words = names(freq.terms), freq = word.frrq , min.freq = 3,random.order = F,colors = pal)
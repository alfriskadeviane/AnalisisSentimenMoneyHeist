library(SnowballC)
library(Rstem)
library(twitteR)
library(tm)
library(NLP)
library(SentimentAnalysis)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(sentiment)

#mengakses API Twitter
reqURL <- "http://api.twitter.com/oath/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
CUSTOMER_KEY <- "vPoDCU9994ZZUsEHFf5p8QrBV" 
CUSTOMER_SECRET <- "36r659jBumNUURjefZIBLc2Kz1kV3ozd22hOabFgWna8zNkvY8" 
ACCESS_TOKEN <- "313826433-eEv2aWHNeF0krqHkDc2BwWYhvgdLKPd80zI31vTX" 
ACCESS_secret <- "jv45289JTKP0lTkKjyZNShP7SCmtCvkeVVktt0C0Yk5gb" 
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)

#mengambil tweet
some_tweets <- searchTwitter('Money Heist', n=1000, lang="en")
some_txt <- sapply(some_tweets, function(x) x$getText()) 
write.csv(some_txt, file = 'dataTwitter.csv')
View(some_tweets)
View(some_txt)

#cleaning data
some_txt = sapply(some_tweets, function(x) x$getText())
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
some_txt = gsub("note", "", some_txt)
# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)
# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

write.csv(some_txt, file = 'dataCleaned.csv')

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"
# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
View(polarity)

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
write.csv(sent_df, file = 'dataSentimen.csv')
View(sent_df)
head(sent_df,20)
table(sent_df$emotion)
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  labs(title = "Sentiment Analysis of Money Heist",
       plot.title = element_text(size=12))
plotSentiments1 <- function(sentiment_dataframe, title) 
{
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) + 
    geom_bar(aes(y=..count.., fill=emotion)) + 
    scale_fill_brewer(palette="Dark2") + 
    ggtitle(title) + 
    theme(legend.position="right") + 
    ylab("Number of Tweets") + 
    xlab("Emotion Categories")
}

#plotting tweets emotions
plotSentiments1(sent_df, "Sentiment Analysis of Money Heist")


# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  labs(title = "Sentiment Analysis of Money Heist",
       plot.title = element_text(size=12))
plotSentiments2 <- function(sent_df, title)
{
  library(ggplot2)
  ggplot(sent_df, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position="right") +
    ylab("Number of Tweets") +
    xlab("Polarity Categories")
}

#plotting tweets polarity
plotSentiments2(sent_df, "Polarity Analysis of Hashtag Money Heist")

#liat jumlah tweets
View(sent_df)
table(sent_df$polarity)



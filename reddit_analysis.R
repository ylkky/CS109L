## Section 0: Preload libraries and functions
library(ggplot2)
library(e1071)
library(tm)
library(wordcloud)

## Generates a histogram representing either the highest scores for
## buckets of creation time or the post frequencies per bucket.
GenerateHistogram <- function(main.data, sub.id, score.graph=TRUE)
{
  hist.data <- main.data[main.data$subreddit_id == sub.id,]
  bucket.size <- (max(hist.data$score) - min(hist.data$score))/5
  if (score.graph)
  {
    ggplot(hist.data, aes(score, fill=as.factor(created_utc))) + geom_bar(position="dodge", binwidth=bucket.size)
  }
  else
  {
    qplot(created_utc, data=hist.data, geom="histogram")
  }
}

## Generate a Corpus
GenerateCorpus <- function(corpus_data)
{
  corpus <- Corpus(VectorSource(corpus_data$title))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  corpus <- tm_map(corpus, content_transformer(removeWords), stopwords('english'))
  return(corpus)
}

## Make a word cloud
GenerateWC <- function(wc.data)
{
  corpus <- GenerateCorpus(wc.data)
  dtm <- TermDocumentMatrix(corpus, control = list(minWordLength = 1));
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing=TRUE)
  d <- data.frame(word=names(v), freq=v)
  wordcloud(d$word, d$freq, min.freq=20)  
}

## Section 1: Load and combine data
thirty.rock <- read.csv(file="30rock.csv")
niners <- read.csv(file="49ers.csv")
ubuntu <- read.csv(file="ubuntu.csv")
sf.giants <- read.csv(file="sfgiants.csv")
all.data <- rbind(thirty.rock, niners, ubuntu, sf.giants)

## Section 2: Explore dataset
dim(all.data)
names(all.data)
summary(all.data)

## Section 3: Visualize popularity of a given subreddit w.r.t. time of day created (niners & ubuntu)
## Convert from UTC to hour of day timestamp
all.data$created_utc <-as.numeric(format(as.POSIXct(all.data$created_utc, origin='1970-01-01'), "%H"))
summary(all.data)
quantile(all.data$created_utc, c(.1, .3, .65))

all.data.mod <- all.data
all.data.mod$created_utc[all.data$created_utc >= 0 & all.data$created_utc <= 5] = "dawn"
all.data.mod$created_utc[all.data$created_utc >= 6 & all.data$created_utc <= 11] = "morning"
all.data.mod$created_utc[all.data$created_utc >= 12 & all.data$created_utc <= 17] = "afternoon"
all.data.mod$created_utc[all.data$created_utc >= 18 & all.data$created_utc <= 23] = "evening"
all.data.mod$subreddit_id = as.character(all.data.mod$subreddit_id)
# Niners
GenerateHistogram(all.data.mod, "t5_2rebv")
GenerateHistogram(all.data.mod, "t5_2rebv", FALSE)
# Ubuntu
GenerateHistogram(all.data.mod, "t5_2qh62")
GenerateHistogram(all.data.mod, "t5_2qh62", FALSE)

## Section 4: Generate Word Clouds
GenerateWC(thirty.rock)
GenerateWC(sf.giants)
GenerateWC(ubuntu)
GenerateWC(niners)

## Section 5: Run a classifier for 30 Rock and Niners (Poor Performance)
unmod.data = rbind(thirty.rock[sample(seq_len(nrow(thirty.rock)), size = 500),], niners[sample(seq_len(nrow(niners)), size = 500),])
unmod.data$created_utc <-as.numeric(format(as.POSIXct(unmod.data$created_utc, origin='1970-01-01'), "%H"))

corpus <- GenerateCorpus(unmod.data)
dtm <- DocumentTermMatrix(corpus)
reddit.ml <- data.frame(as.matrix(dtm))
reddit.ml = cbind(reddit.ml, unmod.data[c('score', 'author', 'num_comments', 'created_utc', 'subreddit_id')])
names(reddit.ml)

train.indices <- sample(seq_len(nrow(reddit.ml)), size = floor(0.9 * nrow(reddit.ml)))
reddit.ml.train <- reddit.ml[train.indices, ]
reddit.ml.test <- reddit.ml[-train.indices, ]

reddit.model <- naiveBayes(subreddit_id ~ ., data = reddit.ml.train)
reddit.prediction <- predict(reddit.model, reddit.ml.test)
mean(reddit.prediction == reddit.ml.test$subreddit_id)

## Section 6: Run a classifier for Iris (Excellent Performance)
iris.ml <- iris
summary(iris.ml)
train.indices <- sample(seq_len(nrow(iris.ml)), size = floor(0.8 * nrow(iris.ml)))
iris.ml.train <- iris.ml[train.indices, ]
iris.ml.test <- iris.ml[-train.indices, ]

iris.model <- naiveBayes(Species ~ ., data = iris.ml.train)
iris.prediction <- predict(iris.model, iris.ml.test)
mean(iris.prediction == iris.ml.test$Species)

qplot(Petal.Length, Petal.Width, colour = Species, data=iris.ml)
qplot(Petal.Length, Sepal.Width, colour = Species, data=iris.ml)
qplot(Sepal.Length, Petal.Width, colour = Species, data=iris.ml)
library(R6)
library(data.table)
library(tidytext)
library(tidyr)
library(SnowballC)
library(topicmodels)
library(tm)

library(dplyr)

Exploration <- R6Class("Exploration",
                       public = list(
                         train = NULL,
                         test = NULL,
                         
                         initialize = function() {
                           self$train <- fread("input/train.csv")
                           self$test <- fread("input/test.csv")
                         },
                         build = function() {
                           # data <- as_tibble(self$train$text)
                           raw <- self$train
                           data <- raw %>%
                             unnest_tokens(output = "word", token = "words", input = text) %>%
                             anti_join(stop_words) %>%
                             mutate(word = wordStem(word))
                           dtm <- data %>% count (id, word) %>% cast_dtm(document = id, term = word, value = n, weighting = tm::weightTf)
                           # lda <- LDA(dtm, k=4, method = "Gibbs", control=list(seed =1111))
                           dt<-as.data.table(as.matrix(dtm))
                           dt[, tweet_id := raw$id][, target:=raw$target]
                           dt
                         }
                       ),
                       private = list()
                       )



# remove stuff like http, t.co


raw <- fread("input/train.csv")
raw <- raw[1:100]
data <- raw %>%
  unnest_tokens(output = "word", token = "words", input = text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word))
dtm <- data %>% count (id, word) %>% cast_dtm(document = id, term = word, value = n, weighting = tm::weightTf)
dtm <-removeSparseTerms(dtm, sparse = .99)
# lda <- LDA(dtm, k=4, method = "Gibbs", control=list(seed =1111))
# dt<-as.data.table(as.matrix(dtm))
# dt[, tweet_id := raw$id]
# dt[, target:=raw$target]

dtm_as_df <- tidy(dtm) %>% spread(term, count)
dtm_as_df <- as.data.table(dtm_as_df)
dtm_as_df[, document := as.integer(document)]
joined <-inner_join(dtm_as_df, raw, by = c("document" = "id"))
joined[, c("text", "document", "id") :=  NULL]
joined[is.na(joined)]<-0

model <- glm(data = joined,  target ~ ., family = "binomial")

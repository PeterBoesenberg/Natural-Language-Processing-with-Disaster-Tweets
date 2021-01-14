library(R6)
library(data.table)
library(tidytext)
library(tidyr)
library(SnowballC)
library(topicmodels)
library(tm)
library(dplyr)


#' Prepare data with a document term matrix.
#' Attaches a document-term-matrix to the data.
#' Removes sparse data (words, which are used only by a couple of tweets)
#' => therefore some tweeets will be removed. Which is fine for building the model, but bad for predicting.
#' We handle it later.
#' 
#' @param raw data-table, directly from CSV 
#'
#' @return datatable with new columns (DTM)
prepare <- function(raw) {
  data <- raw %>%
    unnest_tokens(output = "word", token = "words", input = text) %>%
    anti_join(stop_words) %>%
    mutate(word = wordStem(word))
  dtm <- data %>% count (id, word) %>% cast_dtm(document = id, term = word, value = n, weighting = tm::weightTf)
  dtm <-removeSparseTerms(dtm, sparse = .995)
  print(dtm)
  
  dtm_as_df <- tidy(dtm) %>% spread(term, count)
  dtm_as_df <- as.data.table(dtm_as_df)
  dtm_as_df[, document := as.integer(document)]
  joined <-inner_join(dtm_as_df, raw, by = c("document" = "id"))
  
  joined[is.na(joined)]<-0
  joined
}

# read training data, prepare with document-term-matrix
train <- fread("input/train.csv")
train_prepared <- prepare(train)
# we wont be using those for modeling
train_prepared[, c("text", "document", "location") :=  NULL]

# read test data
test <- fread("input/test.csv")
test_prepared <- prepare(test)
# we wont be using those for predicting
test_prepared[, c("text", "location") :=  NULL]


# Get column names, which are used in training and test data.
# Columns=words, so we want only columns, which we have trained our model. 
# And we want only columns, which we want to use for prediction.
col_names_train <- names(train_prepared)
col_names_test <- names(test_prepared)
common_cols <- intersect(col_names_train, col_names_test)

# use all common columns + "target" for building our model
data_for_model <- train_prepared[, c(common_cols, "target"), with = FALSE]
model <- glm(data = data_for_model,  target ~ ., family = "binomial")

# use all common columns and "document", which is basically the tweet_id
data_for_predict <- test_prepared[, c(common_cols, "document"), with = FALSE]
data_for_predict[, prediction:=predict(model, data_for_predict, type="response")]

# prediction is a float, we have to convert it into a clear 1 or 0.
# We are using a simple round, but maybe a different threshold would make sense
result <- data_for_predict[, target := round(prediction)]

# Add all those tweets, which we removed in the prepare-function back in and just give them a target of zero.
result <- right_join(result, test, by = c("document" = "id"))
result[is.na(target), target := 0]

# rename column and reduce data to only id and target
setnames(result, "document", "id")
result <- result[, c("id", "target")]

fwrite(result, "result.csv")

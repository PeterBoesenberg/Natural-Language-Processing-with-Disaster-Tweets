library(R6)
library(data.table)
library(tidytext)
library(tidyr)
library(SnowballC)
library(topicmodels)
library(tm)
library(dplyr)


prepare <- function(raw) {
  data <- raw %>%
    unnest_tokens(output = "word", token = "words", input = text) %>%
    anti_join(stop_words) %>%
    mutate(word = wordStem(word))
  dtm <- data %>% count (id, word) %>% cast_dtm(document = id, term = word, value = n, weighting = tm::weightTf)
  dtm <-removeSparseTerms(dtm, sparse = .99)
  
  dtm_as_df <- tidy(dtm) %>% spread(term, count)
  dtm_as_df <- as.data.table(dtm_as_df)
  dtm_as_df[, document := as.integer(document)]
  joined <-inner_join(dtm_as_df, raw, by = c("document" = "id"))
  
  joined[is.na(joined)]<-0
  joined
}

train <- fread("input/train.csv")
train_prepared <- prepare(train)
train_prepared[, c("text", "document", "location") :=  NULL]

test <- fread("input/test.csv")
test_prepared <- prepare(test)
test_prepared[, c("text", "location") :=  NULL]


col_names_train <- names(train_prepared)
col_names_test <- names(test_prepared)

common_cols <- intersect(col_names_train, col_names_test)


data_for_model <- train_prepared[, c(common_cols, "target"), with=FALSE]
data_for_predict <- test_prepared[, c(common_cols, "document"), with=FALSE]

model <- glm(data = data_for_model,  target ~ ., family = "binomial")





data_for_predict[, prediction:=predict(model, data_for_predict, type="response")]



result <- data_for_predict[, c("document", "target") := list(id, round(prediction))]
result <- right_join(result, test, by = c("id" = "document"))
result[is.na(target), target := 0]
result <- data_for_predict[, target := round(prediction)][, id:=document][, c("id", "target")]

fwrite(result, "result.csv")

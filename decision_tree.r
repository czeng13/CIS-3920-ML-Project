library(dplyr)
library(stringr)
library(class)
library(tree)


rm(list = ls())

data=read.csv(file.choose(),header=T, stringsAsFactors=TRUE) 
names(data)
attach(data)
summary(data)

head(data)

dim(data)
str(data)


which(is.na(data))
sum(is.na(data))

rating <- as.numeric(rating)

percentiles_33_66 <- quantile(rating, c(0.33, 0.66))

breaks <- c(-Inf, 19, 21, Inf)

labels <- c("low", "medium", "high")

rating_category <- cut(rating, breaks = breaks, labels = labels, include.lowest = TRUE)
data$rating_category = rating_category

median_review_length <- median(str_count(data$review_content, "\\S+"))
print(median_review_length)


review_breaks <- c(-Inf,139, Inf)
review_labels <- c("short","long")
review_length = str_count(data$review_content, "\\S+")
review_category = cut(review_length, breaks = review_breaks, labels = review_labels, include.lowest = TRUE)

data$review_category = review_category

overall_categories <- vector("character", length(category))

for (i in 1:length(category)) {
  character_vector <- as.character(category[i])
  split_list <- strsplit(character_vector, "\\|")
  overall_categories[i] <- split_list[[1]][1]
}

data$overall_categories = overall_categories

data$overall_categories = as.factor(overall_categories)
data$discount_percentage = as.numeric(discount_percentage)
data$rating_count = as.numeric(rating_count)
data$actual_price = as.numeric(actual_price)

set.seed(1)
train <- sample(nrow(data), nrow(data) * 0.8)

tree.model <- tree(rating_category ~ discount_percentage + overall_categories + actual_price + review_category + rating_count, data = data, subset = train)
data.test=data[-train,]
rating_category.test=rating_category[-train]
cv.model <- cv.tree(tree.model, K = 10, FUN = prune.misclass)


best_tree <- cv.model$size[which.min(cv.model$dev)]


prune_model <- prune.tree(tree.model, best = best_tree)

dev.new()
plot(prune_model)
text(prune_model, pretty = 1)

prune_model.pred <- predict(prune_model, newdata = data.test, type = "class")

confusion_matrix <- table(prune_model.pred, rating_category.test)
print(confusion_matrix)

accuracy = (confusion_matrix[1, 1] + confusion_matrix[2, 2] + confusion_matrix[3, 3])  / sum(confusion_matrix)
accuracy

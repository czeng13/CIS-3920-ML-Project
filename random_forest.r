rm(list = ls())

library(randomForest)
library(stringr)
library(tree)
library(class)

data=read.csv(file.choose(),header=T, stringsAsFactors=TRUE) 
names(data)
attach(data)
summary(data)

head(data)

dim(data)
str(data)

# Simplifying category variable by only selecting the 1st element
overall_categories <- vector("character", length(category))

for (i in 1:length(category)) {
  character_vector <- as.character(category[i])
  split_list <- strsplit(character_vector, "\\|")
  overall_categories[i] <- split_list[[1]][1]
}

data$overall_categories = overall_categories


# creating rating_category variable that divides rating into high, medium, and low
rating <- as.numeric(rating)

percentiles_33_66 <- quantile(rating, c(0.33, 0.66))

breaks <- c(-Inf, 19, 21, Inf)

labels <- c("low", "medium", "high")

rating_category <- cut(rating, breaks = breaks, labels = labels, include.lowest = TRUE)
data$rating_category = rating_category

# creating review_category variable that differentiates between short and long reviews
median_review_length <- median(str_count(data$review_content, "\\S+"))
print(median_review_length)


review_breaks <- c(-Inf,139, Inf)
review_labels <- c("short","long")
review_length = str_count(data$review_content, "\\S+")
review_category = cut(review_length, breaks = review_breaks, labels = review_labels, include.lowest = TRUE)

data$review_category = review_category

data$discount_percentage = as.numeric(discount_percentage)
data$category = as.character(category)
data$rating_count = as.numeric(rating_count)
data$actual_price = as.numeric(actual_price)

data$overall_categories <- as.factor(data$overall_categories)
data$review_category <- as.factor(data$review_category)


data$review_category = as.numeric(data$review_category) - 1

data_encoded <- model.matrix(~ . - 1, data = data)

data$rating_category = as.numeric(rating_category)


set.seed(1)
train = sample(1:nrow(data), nrow(data)/2)
tree.boston=tree(rating_category~ discount_percentage + overall_categories + rating_count + actual_price + review_category,data,subset=train) 

cv.boston=cv.tree(tree.boston,K=10)
cv.boston

prune.boston=prune.tree(tree.boston,best=8)
boston.test=data[-train,"rating_category"]

tree.pred=predict(prune.boston,newdata=data[-train,])
mean((tree.pred-boston.test)^2)


set.seed(1)
bag.boston=randomForest(rating_category~ discount_percentage + overall_categories + rating_count + actual_price + review_category ,data=data,subset=train,mtry=13,importance=TRUE)
bag.boston


yhat.bag = predict(bag.boston,newdata=data[-train,])
print(yhat.bag)

mean((yhat.bag-boston.test)^2)


set.seed(1)
rf.boston=randomForest(rating_category~ discount_percentage + overall_categories + rating_count + actual_price + review_category,data=data,subset=train,mtry=5,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=data[-train,])
mean((yhat.rf-boston.test)^2)


importance(rf.boston)
dev.new(width = 10, height = 8)
varImpPlot(rf.boston)

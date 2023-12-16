rm(list = ls())
library(nnet)
library(class)
library(caret)
library(randomForest)
library(stringr)
library(tree)
library(class)
library(ggplot2)


data=read.csv(file.choose(),header=T, stringsAsFactors=TRUE) 
names(data)
attach(data)
summary(data)

head(data)

dim(data)
str(data)

which(is.na(data))
sum(is.na(data))

# dividing rating into categories
rating <- as.numeric(rating)

percentiles_33_66 <- quantile(rating, c(0.33, 0.66))

# View the calculated percentiles
print(percentiles_33_66)

breaks <- c(-Inf, 19, 21, Inf)

labels <- c("low", "medium", "high")

rating_category <- cut(rating, breaks = breaks, labels = labels, include.lowest = TRUE)
data$rating_category = rating_category

# separating long and short reviews
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

glm.fit1=glm(rating_category ~ discount_percentage + review_category + actual_price + overall_categories + rating_count,family="binomial",data=data)
summary(glm.fit1)
exp(coef(glm.fit1)) 

discount_percentage = as.numeric(discount_percentage)
overall_categories = as.numeric(overall_categories)
rating_count = as.numeric(rating_count)

standardized.discount_percentage=scale(discount_percentage)
standardized.overall_categories=scale(overall_categories)
standardized.rating_count=scale(rating_count)


Input.standard=cbind(standardized.discount_percentage,standardized.rating_count)
accuracy=matrix(0,10,5)

set.seed(1)
folds=sample(1:5,nrow(Input.standard),replace=TRUE)
for (j in 1:10)
{
  for(i in 1:5)
  {
    train.standard=Input.standard[folds!=i,]
    test.standard=Input.standard[folds==i,]
    train.truevalue=rating_category[folds!=i]
    test.truevalue=rating_category[folds==i]
    knn.pred=knn(train.standard,test.standard,train.truevalue,k=j)
    accuracy[j,i]=mean(knn.pred==test.truevalue)
  }
}

cv.accuracy=apply(accuracy,1,mean)
mean(cv.accuracy)
show(cv.accuracy)
accuracy_data <- data.frame(accuracy = cv.accuracy)


ggplot(accuracy_data, aes(x = 1:nrow(accuracy_data), y = accuracy)) +
  geom_point() +
  geom_line() +
  labs(x = "Fold", y = "Accuracy") +
  ggtitle("Cross-Validation Accuracies")

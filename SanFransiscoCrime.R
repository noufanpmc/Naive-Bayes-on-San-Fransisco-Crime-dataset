#################Machine Learning Evaluation###############
###################Dhrubojyoti Mukherjee###################
#########################Mohd Ahad#########################
#######################Noufan PMC##########################
#########################DF 1709###########################

#Dataset link:https://www.kaggle.com/c/sf-crime/download/train.csv.zip

##Data pre-processing
# Import data and rearrange
library(data.table)
dataset = data.table(read.csv('train.csv',
                            header = T))
library(ggplot2)
library(caTools)
set.seed(10)
split = sample.split(dataset$Category, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

train = training_set[,-1]
test = test_set[,-1]

train = train[,c(1,2,3,4,6,7,8,5)]
test = test[,c(1,2,3,4,6,7,8,5)]

# Feature Scaling
train[,c(6,7)] = scale(train[,c(6,7)])
test[,c(6,7)] = scale(test[,c(6,7)])

#naive bayes classifier(before tuning with 'laplace' parameter)
library(e1071)
classifier = naiveBayes(x = train[,-1],
                        y = train$Category,laplace = 0)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test[,-1])

# Making the Confusion Matrix
cm = table(test$Category, y_pred)
accuracy = (sum(diag(cm))/sum(cm))
accuracy#0.89739

#naive bayes classifier(after tuning with'laplace' parameter)
library(e1071)
classifier = naiveBayes(x = train[,-1],
                        y = train$Category,laplace = 1)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test[,-1])

# Making the Confusion Matrix
cm = table(test$Category, y_pred)
accuracy = (sum(diag(cm))/sum(cm))
accuracy# 0.98699

#Plotting
p = as.data.frame(y_pred)
q = ggplot(data = p, aes(x = y_pred)) +
  geom_bar(colour = "black", fill = "skyblue") +
  ggtitle("Plot of predicted crime category") +
  ylab('Count') 
q + theme(axis.text.x = element_text(angle = 90, hjust = 1))


##########################END########################
# Daniel Tuohy
# This code is working through the Kaggle Titanic tutorial by Trevor Stephens called 
# 'Getting started with R' Part 3 - Decision Trees. It can be found at:
# http://trevorstephens.com/post/72923766261/titanic-getting-started-with-r-part-3-decision

###### PART 3 - DECISION TREES ######

setwd("~/Documents/My College/Data Mining/CA2")

train <- read.csv('/Users/danieltuohy/Documents/My College/Data Mining/CA2/train.csv')

test <- read.csv('/Users/danieltuohy/Documents/My College/Data Mining/CA2/test.csv')

### PRE-PROCESSING #### - See previous code for explanation (e.g. DanielT DecisionTreev4.R)

test$Survived <- NA
combined_data_frames <- rbind(train,test)
combined_data_frames$Fare2 <- '30+'
# this creates a new variable and assigns the value of '30+'
combined_data_frames$Fare2[combined_data_frames$Fare < 30 & combined_data_frames$Fare >= 20] <- '20-30'
combined_data_frames$Fare2[combined_data_frames$Fare < 20 & combined_data_frames$Fare >= 10] <- '10-20'
combined_data_frames$Fare2[combined_data_frames$Fare < 10] <- '<10'
# these commands will break the remaining fares into different groups

# str (combined_data_frames)
#combined_data_frames
# to have a look at the new data frame

combined_data_frames$Survived <- as.factor(combined_data_frames$Survived)
# the survived attribute was changed to a factor
levels(combined_data_frames$Cabin)[1] = "missing"
levels(combined_data_frames$Embarked)[1] = "missing"
# fixing empty character level names 

training_new <- combined_data_frames[1:891,]
test_new <- combined_data_frames[892:1309,]

# -----Pre-processing now complete--------

library(rpart)
# rpart for ‘Recursive Partitioning and Regression Trees’ and uses the CART decision tree algorithm

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=training_new, method="class")
# you might try $fare2 attribute instead

plot(fit)
text(fit)
# examining the 'fit' model

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
# installing these libraries will allow better visualisations for the data

summary (fit)

fancyRpartPlot(fit)
# renders the decision tree model a bit nicer and with more information

Prediction <- predict(fit, test_new, type = "class")
# Here we have called rpart’s predict function and point it at the test_new dataframe

submit_CARTdecisiontree <- data.frame(PassengerId = test_new$PassengerId, Survived = Prediction)
write.csv(submit_CARTdecisiontree, file = "my1stCARTdtree.csv", row.names = FALSE)
# Kaggle submission made in the same way as before.

# THIS SUBMISSION GETS A SCORE OF 0.78469 WHICH IS NOT AN IMPROVEMENT ON THE C50 ALGORITHM SCORE OF 0.78947.
# THIS DECISION TREE (CART) uses more atributes than the C5.0 DECISION TREE......

###### MAKE SOME TWEAKS TO CART DECISION TREE TO SEE EFFECT ####
# Try use Fare 2 attribute - this might give a different result
# The model may be overfitted - try drop some attributes from the model

#### Using Fare 2 attribute instead #####
fit_fare2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked, data=training_new, method="class")

fancyRpartPlot(fit_fare2)
Predictionv2 <- predict(fit_fare2, test_new, type = "class")
submit_CARTdecisiontreev2 <- data.frame(PassengerId = test_new$PassengerId, Survived = Predictionv2)
write.csv(submit_CARTdecisiontreev2, file = "my2ndCARTdtree.csv", row.names = FALSE)

# This results in Fare2 not been used in the model
# THIS SUBMISSION GETS A SCORE OF 0.77990 WHICH IS NOT AN IMPROVEMENT on above dtree(CART) or previous dtree(C5.0)

#### Using Fare 2 attribute instead #####

# The summary(fit) shows the variable importance in the decision tree

#Variable importance
#Sex     Fare   Pclass    SibSp    Parch      Age Embarked 
#47       18       13        7        6        5        4 

# We can try to drop some of these variables to determine the effect (the model may be overfitted)
# Lets try drop the 'Embarked' attribute

fit_drop_embarked <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, data=training_new, method="class")

fancyRpartPlot(fit_drop_embarked)
Predictionv3 <- predict(fit_drop_embarked, test_new, type = "class")
submit_CARTdecisiontreev3 <- data.frame(PassengerId = test_new$PassengerId, Survived = Predictionv3)
write.csv(submit_CARTdecisiontreev3, file = "my3rdCARTdtree.csv", row.names = FALSE)

# THIS SUBMISSION GETS A SCORE OF 0.79426 - my best score yet!!!!!!

# ALL MODELS ABOVE USE DEFAULTS THAT STOP THE DTREE FROM GROWING TOO MASSIVE.
# YOU CAN OVERIDE THESE DEFAULTS BY USING THE COMMANDS BELOW - SEE TUTORIAL.
# DIDN'T TRY THIS BUT TUTORIAL SHOWS THAT THIS DOES NOT IMPROVE THE SCORE.

# fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
 #            method="class", control=rpart.control(minsplit=2, cp=0))
# fancyRpartPlot(fit)

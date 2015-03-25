# Daniel Tuohy - attempt two at Decision Tree
# I want to try get more variables considered in the decision tree
# try to create new fare variable

train <- read.csv('/Users/danieltuohy/Documents/My College/Data Mining/CA2/train.csv')

test <- read.csv('/Users/danieltuohy/Documents/My College/Data Mining/CA2/test.csv')

test$Survived <- NA

combined_data_frames <- rbind(train,test)
# the data frames were combined using the rbind() function to create a brand new data frame
# this was done so that certain attributes would have identical factor levels.

combined_data_frames$Fare2 <- '30+'
# this creates a new variable and assigns the value of '30+'
combined_data_frames$Fare2[combined_data_frames$Fare < 30 & combined_data_frames$Fare >= 20] <- '20-30'
combined_data_frames$Fare2[combined_data_frames$Fare < 20 & combined_data_frames$Fare >= 10] <- '10-20'
combined_data_frames$Fare2[combined_data_frames$Fare < 10] <- '<10'
# these commands will break the remaining fares into different groups

str (combined_data_frames)

combined_data_frames
# to have a look at the new data frame

combined_data_frames$Survived <- as.factor(combined_data_frames$Survived)
# the survived attribute was changed to a factor

levels(combined_data_frames$Cabin)[1] = "missing"
levels(combined_data_frames$Embarked)[1] = "missing"
# fixing empty character level names 

training_new <- combined_data_frames[1:891,]
test_new <- combined_data_frames[892:1309,]
# new data sets were created from the combined data frame, one for training and one for testing.

# -----Pre-processing now complete--------

str(training_new)
str(test_new)
# again have another look to check the data frames were created OK

# WE ARE USING 100% OF THE TRAINING DATA TO BUILD MODEL
# install.packages("C50")
# install C.50 algorithm if needed

library(C50)

decisiontree_model <- C5.0(training_new[-2], training_new$Survived)

# This command will build the decision tree model using the training_new data frame and the 'Survived' attribute as the class
# [-2] means do not include the 2nd column, in this case 'Survived' as this is the class

decisiontree_model

summary(decisiontree_model)
# This model STILL ONLY uses the sex attribute and ticket attribute. Summary shows model better at predicting class b or 1. 
# In other words the model is better at predicting those that did survive (1=yes) than predicting
# those that did not survive (0=no)
# I THINK THE DECISION TREE IS SPLITTING ON THE GENDER FIRST WHICH THEN EXCLUDES MORE SUBTLE 
# PREDICTORS SUCH AS THE NEW FARE VARIABLE THAT I CREATED

survived_predict <- predict(decisiontree_model, test_new)
# Now I am creating a variable called 'Survived' with the values of my prediction

test_new$Survived <- survived_predict
# Adding predictions back into the test dataframe. You could skip this step but I like to look at the 
# newly created results in R before submitting

test_c50submission <- test_new[c('PassengerId', 'Survived')]
# Create a new dataframe with only the variables 'PassengerId' and 'Survived'

write.csv(test_c50submission, file="c50decisiontreev4.csv", row.names=FALSE)
# write the new dataframe to a csv file and remove row names.
# this file can be submitted to Kaggle to get the score

decisiontree_model_boost10 <- C5.0(training_new[-2], training_new$Survived, trials =10)
# This command will build the decision tree model using the training_new data frame and the 'Survived' attribute as the class
# [-2] means do not include the 2nd column, in this case 'Survived' as this is the class
# We can run the model again but this time change 'trials' or the number of boosted iterations.
# This might improve the performance of the model on the training data
# See slide 40 for explanation of adaptive boosting

decisiontree_model_boost10
summary(decisiontree_model_boost10)
# This model BOOSTS THE ACCURACY AND USES MORE VARIABLES

survived_predict <- predict(decisiontree_model_boost10, test_new)
# Now I am creating a variable called 'Survived' with the values of my prediction

test_new$Survived <- survived_predict
# Adding predictions back into the test dataframe. You could skip this step but I like to look at the 
# newly created results in R before submitting

test_c50submission_boosted <- test_new[c('PassengerId', 'Survived')]
# Create a new dataframe with only the variables 'PassengerId' and 'Survived'

write.csv(test_c50submission_boosted, file="c50decisiontreev4.csv", row.names=FALSE)
# write the new dataframe to a csv file and remove row names.
# this file can be submitted to Kaggle to get the score

test_c50submission_boosted
# can check the new data frame


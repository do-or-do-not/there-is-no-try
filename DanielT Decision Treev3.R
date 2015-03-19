# Daniel Tuohy - attempt at Decision Tree
# See J Flynn file 'creating testable data in R' for more information on brief pre-processing below
# I am going to attempt to change more of the variables to 'as factors'

# install.packages("dplyr")

# library("dplyr")
# not needed

train <- read.csv('/Users/danieltuohy/Documents/My College/Data Mining/CA2/train.csv')

test <- read.csv('/Users/danieltuohy/Documents/My College/Data Mining/CA2/test.csv')
# this will be used slightly later

test$Survived <- NA

combined_data_frames <- rbind(train,test)
# the data frames were combined using the rbind() function to create a brand new data frame
# this was done so that certain attributes would have identical factor levels.

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

survival_model <- C5.0(training_new[-2], training_new$Survived)

# This command will build the decision tree model using the training_new data frame and the 'Survived' attribute as the class
# [-2] means do not include the 2nd column, in this case 'Survived' as this is the class

survival_model

summary(survival_model)

# This model uses only the sex attribute and ticket attribute. Summary shows model better at predicting class b or 1. 
# In other words the model is better at predicting those that did survive (1=yes) than predicting
# those that did not survive (0=no)

survived_predict <- predict(survival_model, test_new)

# Now I am creating a variable called 'Survived' with the values of my prediction

test_new$Survived <- survived_predict
# Adding predictions back into the test dataframe. You could skip this step but I like to look at the 
# newly created results in R before submitting


test_c50submission <- test_new[c('PassengerId', 'Survived')]
# Create a new dataframe with only the variables 'PassengerId' and 'Survived'


write.csv(test_c50submission, file="c50decisiontree.csv", row.names=FALSE)
# write the new dataframe to a csv file and remove row names.


# Daniel Tuohy
# This code is working through the tutorial by Trevor Stephens called
# Titanic-Getting started with R at:
# http://trevorstephens.com/post/72918760617/titanic-getting-started-with-r-part-1-booting

###### PART 1 - NONE SURVIVE - OUTCOME VARIABLE AS PREDICTOR ######

setwd("~/Documents/My College/Data Mining/CA2")

# train <- read.csv("~/Documents/My College/Data Mining/CA2/train.csv")
# View(train)

# test <- read.csv("~/Documents/My College/Data Mining/CA2/test.csv")
# View(test)

str(train)

table(train$Survived)
# isolate a single column of the dataframe

prop.table(table(train$Survived))
# How about a proportion? Well, we can send the output of one function into another

# We can see from the prop table that over 60% of people died.
# We are going to use this to build a very simple model and assume that most people 
# died in the test data. To make a first prediction, we will say that everyone died.

test$Survived <- rep(0, 418)

# Since there was no ‘Survived’ column in the dataframe, this command will create 
# a new column for us and the rep() function will repeat our ‘0’ prediction 418 times,
# the same number of rows we have in the test data.

submit_nosurvivors <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit_nosurvivors, file = "nosurvivors.csv", row.names = FALSE)
# This submission got a Kaggle score of 62%

######### PART 2 - GENDER AND CLASS VARIABLES AS PREDICTORS ##########

summary (train$Sex)

prop.table(table(train$Sex, train$Survived))
# This command by default takes each entry in the table and divides by number of rows

prop.table(table(train$Sex, train$Survived),1)
# typing the 1 gives us the row-wise proportion
# typing a 2 would give us the column wise proportion

#### WE ARE GOING TO MAKE ANOTHER PREDICTION - 'Woman and Children First'

test$Survived <- 0
# this command is the same as rep() function, we are adding in all 0's to this column

test$Survived[test$Sex == 'female'] <- 1
# we are now replacing the 0's with ones were the sex is equal to females
# we seen earlier that the majority of females survived.

# We can submit this to Kaggle as before, it gets a score of 0.762

submit_womanfirst <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit_womanfirst, file = "womanfirst.csv", row.names = FALSE)

#### Looking at other variables ####

prop.table(table(train$Age, train$Survived),1)
prop.table(table(train$Pclass, train$Survived),1)

# How can you set combinations of attributes equal to some value i.e. 1 or 0?





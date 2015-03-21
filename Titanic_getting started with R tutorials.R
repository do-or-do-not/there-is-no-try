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
# the double equals is a boolean test
# we are now replacing the 0's with ones were the sex is equal to females
# we seen earlier that the majority of females survived.

# We can submit this to Kaggle as before, it gets a score of 0.762

submit_womanfirst <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit_womanfirst, file = "womanfirst.csv", row.names = FALSE)

#### Looking at other variables ####

prop.table(table(train$Age, train$Survived),1)
prop.table(table(train$Pclass, train$Survived),1)

# How can you set combinations of attributes equal to some value i.e. 1 or 0?

summary(train$Age)

# We can see from this command that the average age is 29 and we have 177 missing values

train$Child <- 0
train$Child[train$Age < 5] <- 1

# proportion tables become less useful for continous variables as we could have only two 
# instances of each age. # we can create a new child variable
# the less than sign is another boolean test..NA's will also fail test and give a 0
# this is Ok for now because we can assume that all NA's are the average age of 29 to
# keep things simple.

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
# This command will subset the whole dataframe on 'Child' and 'Sex'
# This command will then sum the target variable 'Survived' for each subset
# It will give us the suvivors for both is 'Child' and not 'Child'.

aggregate(Survived ~ Child + Sex, data=train, FUN=length)
# aggregate(Pclass ~ Child + Sex, data=train, FUN=length)
# This command will subset the whole dataframe on 'Child' and 'Sex'
# This command will then get the length of the target variable 'Survived' or Survived vector
# for each subset. In other words it will count the number of people for each subset regardless
# if any of them are 1's or 0's.
# This command would have equally worked using another variable as the target variable such as Pclass

aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
# This command peforms two functions on the subsets which works out the proportion
# we have the number of survivors sum(x) divided by the total number of people length(x)

# We find out that 75% of female children (<18) survive & 39% of male children (<18) survive
# WE COULD PERHAPS LOOK AT YOUNGER AGES AS A BETTER DEFINTION of a child to see the effect.

# THIS MAKES A BIG DIFFERENCE TO THE PROPORTIONS
# We find out that 74% of female children (<5) survive & 65% of male children (<5) survive

### TRY ANOTHER SIMPLE MODEL - WOMAN FIRST AND CHILDREN #####

test$Survived <- 0
# this command is the same as rep() function, we are adding in all 0's to this column
# aggregate(Survived ~ Child + Sex, data=test, FUN=length)

test$Child <- 0
test$Child[test$Age < 5] <- 1
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Age < 5] <- 1
# this command should do the trick for assigning all females and children under the age of 5 as survived

test
# do this to check that the correct values were assigned

submit_womanandchildfirst <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit_womanandchildfirst, file = "womanandchildfirst.csv", row.names = FALSE)

# THIS PREDICTOR DID NOT IMPROVE MY SCORE. I PUT THIS DOWN TO THE SMALL NUMBER OF MALE CHILDREN
# UNDER THE AGE OF 5 IN THE TEST SET (I THINK IT IS 4)

#### Start to look at class variable #####

train$Fare2 <- '30+'
# this creates a new variable and assigns the value of '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
# these commands will break the remaining fares into different groups

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
# the aggregate() function will work out the proportion of survivors within each data subset









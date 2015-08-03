### Source: http://trevorstephens.com/post/72923766261/titanic-getting-started-with-r-part-3-decision
### Example: Titanic

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv("data/train.csv")

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

plot(fit)
text(fit)

fancyRpartPlot(fit)

# unleashes the depth that the tree can grow, 
# and opens up the minisplit which governs how many passengers must sit in a bucket before even looking for a split
# demonstrates overfitting
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
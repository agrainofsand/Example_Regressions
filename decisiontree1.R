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
manip <- function(){
  str(train)
  train$Survived
  table(train$Survived)
  prop.table(table(train$Survived))
  test$Survived <- rep(0, 418)
  sub_pred <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
  write.csv(sub_pred, file = "theyallperish.csv", row.names = FALSE)
  summary(train$Sex)
  prop.table(table(train$Sex, train$Survived))
  prop.table(table(train$Sex, train$Survived), 1)
  test$Survived <- 0
  test$Survived[test$Sex == 'female'] <- 1
  sub_pred <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
  write.csv(sub_pred, file = "theyallperish.csv", row.names = FALSE)
  summary(train$Age)
  train$Child <- 0
  train$Child[train$Age < 18] <- 1
  aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
  aggregate(Survived ~ Child + Sex, data = train, FUN = length)
  aggregate(Survived ~ Child + Sex, data = train, FUN = function(x){sum(x)/length(x)})
  train$Fare2 <- '30+'
  train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
  train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
  train$Fare2[train$Fare < 10] <- '<10'
  aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x){sum(x)/length(x)})
  test$Survived <- 0
  test$Survived[test$Sex == 'female'] <- 1
  test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
  sub_pred <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
  write.csv(sub_pred, file = "theyallperish.csv", row.names = FALSE)
  fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
  plot(fit)
  text(fit)
  rpart.plot(fit)
  Prediction <- predict(fit, test, type = "class")
  sub_pred <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
  write.csv(sub_pred, file = "myfirstdtree.csv", row.names = FALSE)
  fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               data=train,
               method="class", 
               control=rpart.control(minsplit=2, cp=0))
  test$Survived <- NA
  combi <- rbind(train, test)
  combi$Name <- as.character(combi$Name)
  combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  combi$Title <- sub(' ', '', combi$Title)
  combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
  combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
  combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
  combi$Title <- factor(combi$Title)
  combi$FamilySize <- combi$SibSp + combi$Parch + 1
  combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
  combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
  combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
  famIds <- data.frame(table(combi$FamilyID))
  famIds <- famIds[famIds$Freq <= 2,]
  combi$FamilyID[combi$FamilyID %in% famIds$Var1] <- 'Small'
  combi$FamilyID <- factor(combi$FamilyID)
  train <- combi[1:891, ]
  test <- combi[892:1309, ]
}
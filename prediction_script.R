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
}
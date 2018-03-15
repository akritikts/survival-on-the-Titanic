manip <- function(){
  str(train)
  train$Survived
  table(train$Survived)
  prop.table(table(train$Survived))
}
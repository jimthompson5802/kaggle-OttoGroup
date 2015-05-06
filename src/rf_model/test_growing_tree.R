library(caret)
library(randomForest)

set.seed(71)

df <- iris
iris.rf <- randomForest(df[,1:4], df[,5])
print(iris.rf)

iris.rf <- grow(iris.rf,100)


tr.ctrl <- trainControl(method="repeatedcv",number=5,repeats=1)

rf.mdl <- train(df[,1:4],df[,5],method="rf",trainControl=tr.ctrl)
print(rf.mdl$finalModel)

rf.mdl$finalModel <- grow(rf.mdl$finalModel,100)




data<-read.table('./lab3.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE)
View(data)
str(data)
data$klaster<-as.factor(data$klaster)
data$obrazov<-as.factor(data$obrazov)
data$sertifikat<-as.factor(data$sertifikat)
str(data)
install.packages("caTools")
library(caTools)
split <- sample.split(data$klaster, SplitRatio = 0.8)
Train <- subset(data, split == TRUE)
Test <- subset(data, split == FALSE)
View(Train)
View(Test)
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
dataTree <- rpart(klaster ~ ., data = Train, method = 'class', control=rpart.control(minbucket = 1))
#Вывод структуры дерева в текстовом виде
dataTree
#Визуализация дерева
fancyRpartPlot(dataTree, palettes=c('Greys', 'Oranges','Blues', 'Reds'))
PredictCART1<- predict(dataTree, newdata = Train, type="class")
PredictCART1
table(Train$klaster, PredictCART1)
PredictCART2 <- predict(dataTree, newdata = Test, type="class")
PredictCART2
table(Test$klaster, PredictCART2)
plotcp(dataTree)
install.packages("yardstick") 
library(yardstick)
accuracyTest <- accuracy_vec(data = Test, truth = Test$klaster, estimate = PredictCART2)
accuracyTest
recallTest <- recall_vec(data = Test, truth = Test$klaster, estimate = PredictCART2)
recallTest
AUCTest <- roc_auc_vec(data = Test, truth = Test$klaster, estimate=predict(dataTree,newdata=Test,type='prob'))
AUCTest
KappaTest <- kap_vec(data = Test, truth = Test$klaster, estimate = PredictCART2)
KappaTest
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
#number=30
fitControl <- trainControl(method="cv", number=30)
#Использование значения параметра сложности cp от 0.001 до 0.2
cartGrid <- expand.grid(.cp=(1:200)*0.001)
train(klaster ~ ., data = data, method="rpart", trControl=fitControl, tuneGrid=cartGrid)
dataTree2 <- rpart(klaster  ~ ., data = data, method = "class", control=rpart.control(cp = 0.2))
fancyRpartPlot(dataTree2, palettes=c("Greys", "Oranges","Blues", "Reds"))
PredictCART3 <- predict(dataTree2, newdata = data, type="class")
table(data$klaster, PredictCART3)
plotcp(dataTree2)
accuracyTest2 <- accuracy_vec(data = data, truth = data$klaster, estimate = PredictCART3)
accuracyTest2
recallTest2 <- recall_vec(data = data, truth = data$klaster, estimate = PredictCART3)
recallTest2
AUCTest2 <- roc_auc_vec(data = data, truth = data$klaster, estimate=predict(dataTree2,newdata=data,type='prob'))
AUCTest2
KappaTest2 <- kap_vec(data = data, truth = data$klaster, estimate = PredictCART3)
KappaTest2
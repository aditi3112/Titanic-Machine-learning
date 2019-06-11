

titanic.train<-read.csv(file = "train.csv" , stringsAsFactors = FALSE , header = TRUE)
titanic.test<-read.csv(file = "test.csv" , stringsAsFactors = FALSE , header = TRUE)

titanic.train$IsTrain<- TRUE
titanic.test$IsTrain<- FALSE

titanic.test$Survived<-NA

titanic.full<-rbind(titanic.test,titanic.train)

titanic.full[titanic.full$Embarked=='', "Embarked"]<-'S'


#categorical casting
titanic.full$Pclass<-as.factor(titanic.full$Parch)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

#plot to see outlier in fare
boxplot(titanic.full$Fare)
boxplot.stats(titanic.full$Fare)
# to remove outlier
titanic.full$Fare <= 65



age.median<-median(titanic.full$Age , na.rm = TRUE)

 titanic.full[is.na(titanic.full$Age),"Age"]<-age.median

 #clean
 #fare.median<-median(titanic.full$Fare , na.rm = TRUE)
 #titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.median

 upper.whisker <-boxplot.stats(titanic.full$Fare)$stats[5]
 outlier.filter <-titanic.full$Fare < upper.whisker
 
 # Rows without filter
 titanic.full[outlier.filter,]
 
 fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
 fare.model<-lm(
   formula = fare.equation,
   data = titanic.full[outlier.filter,]
 )
 
 fare.row<-titanic.full[
           is.na(titanic.full$Fare),
           c("Pclass" , "Sex" , "Age" , "SibSp" , "Parch" , "Embarked")
   ]
 
 fare.predictions<-predict(fare.model , newdata = fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions
 
 
 #Split
 titanic.train<-titanic.full[titanic.full$IsTrain==TRUE,] 
 titanic.test<-titanic.full[titanic.full$IsTrain==FALSE,] 
 
titanic.train$Survived<- as.factor(titanic.train$Survived)
str(titanic.full)
Survived.equation<-"Survived ~ Pclass +Sex + Age + SibSp + Parch + Fare + Embarked"
Survived.formula<-as.formula(Survived.equation)
install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula = Survived.formula , data = titanic.train , ntry = 500 , mtry = 3 ,nodesize = 0.01 * nrow(titanic.test))

features.equation <- "Pclass +Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <-predict(titanic.model , newdata = titanic.test)

PassengerId<-titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived<- Survived

write.csv(output.df , file = "Kag.csv", row.names = FALSE)



###################################################
library(randomForest)
house_model <- randomForest(SalePrice~.,
                            data = house.train)

importance    <- importance(house_model)
varImpPlot(house_model)



# Predict using the test set

prediction <- predict(house_model,house.test)

# Evaluation RMSE function

RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}
RMSE

RMSE1 <- RMSE(prediction, validate$SalePrice)
RMSE1
## [1] NA
RMSE1 <- round(RMSE1, digits = 5)
##Output file

prediction[which(is.na(prediction))] <- mean(prediction,na.rm=T)
submit <- data.frame(Id=house.test$Id,SalePrice=prediction)
write.csv(submit,file="House_Price_Pradeep.csv",row.names=F)

# Titanic: Machine Learning from Disaster
# Edited by Steven
# Not final ver.

#basic print info
setwd("~/Steven") #your path for R
str(train)
train <- read.csv("train.csv", stringsAsFactors=FALSE)
train <- read.csv("test.csv", stringsAsFactors=FALSE)
	#table(train$[column_name])
	#prop.table(table(train$[column_name]))   column_name : Survived, Pclass, Name..etc
test$Survived <- rep(0, 418)  #set all Survived to 0(all dead)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "first_try.csv", row.names = FALSE)


#Gendermodel try
table(train$Sex)
	#female   male
	#	314    577
prop.table(table(train$Sex, train$Survived),1) #not use paraneter 1 will print the format like f(0,1)|m(0,1)/total
 	#				0         1
	#female 0.2579618 0.7420382
	#male   0.8110919 0.1889081
	#we can know lots of females aboard survived so we try set all female to 1
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gender_try.csv", row.names = FALSE)

summary(train$Age) #NA is missing value, we will deal with later
	#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
	#0.42   20.12   28.00   29.70   38.00   80.00     177
	
	#Then we try create new variable and aggregate syntax
train$Child <- 0
train$Child[train$Age < 10] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
	#FUN parameter sum means who survived in the range we set
	#length means total people number in the range we set
	#Child    	Sex  Survived
	#1     0 female 0.7528958
	#2     1 female 0.6909091
	#3     0   male 0.1657033
	#4     1   male 0.3965517
	
	#Following we will use R library package to calculate parameter precisely
	#we don't need to set the value. 
	
	
#Use decision tree model
library(rpart) #should install if you don't have.  install.packages('rattle') , rpart.plot, RColorBrewer
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")	
	#pinrt graph right side
plot(fit)
text(fit)
	#we also can add another parameter		
	#fit <- rpart(Survived ~ Pclass + ..., data=train, method="class", control=rpart.control(minsplit=2, cp=0))
	#minsplit : branch num  , cp : Decide which parameter proportion less than cp is not important 
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)	
	
	
#Feature engineering
train$Name[1] 	#print : [1] Braund, Mr. Owen Harris
	#Now we want use name to find people who are group of family by flrst name
	#We also establish another table : combi
test$Survived <- NA
combi <- rbind(train, test) 	
combi$Name <- as.character(combi$Name)
	#Now we try to find the string within passenger name
	#strsplit(combi$Name[1], split='[,.]')[[1]][2]  #pinrt : [1] " Mr"
	#Of course we will find lots of string won’t give our model much to work
	#we replace it : combi$Title[combi$Title %in% c('.', '.', '.')] <- '...'
	#combi$Title <- factor(combi$Title)	
	
	#Another Variable : FamilyNUM
combi$FamilyNUM <- combi$SibSp + combi$Parch + 1	
combi$SPname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilyNUM), combi$SPname, sep="")	
	#Then we decide the number to devide
combi$FamilyNUM <- combi$SibSp + combi$Parch
combi$FamilyID[combi$FamilyNUM <= 2] <- 'Decide'
combi$FamilyID <- factor(combi$FamilyID)	

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilyNUM + FamilyID, data=train, method="class")
#fancyRpartPlot(fit)  print plot
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "featur.csv", row.names = FALSE)


#Randon forest method
library(randomForest)   # install.packages('randomForest') 'party'
library(party)

	#Deal with Age NA
table(combi$Age)   #print Age info
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilyNUM, data=combi[!is.na(combi$Age),], method="anova")
	#pclass3_missvalue_tmp
combi$p3mvt[is.na(combi$Age)] <- 5

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])				
table(combi$Embarked)				
which(combi$Embarked == '')  #print : [1]  62 830
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)				
summary(combi$Fare)				
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)				

	# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

	#Built Randon forest model
set.seed(415)

	#type following two line to debug..
train$Sex <- as.factor(train$Sex)
test$Sex <- as.factor(test$Sex)
	#fit <- randomForest(as.factor(Survived) ~ Pclass + .., data=train, importance=TRUE, ntree=2000)
	#varImpPlot(fit)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilyNUM + FamilyID,
               data = train, controls=cforest_unbiased(ntree=1000, mtry=3)) 	
Prediction <- predict(fit, test, OOB=TRUE, type = "response")	
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "random_forest.csv", row.names = FALSE)

#Improve for Pclass 1  #SibSp + Parch 
combi$Survived[combi$Pclass == 1 & combi$Sex == 'male' & combi$Age < 38 & combi$Age > 28 & combi$SibSp >= 1 & combi$Parch >= 1] <- 0
combi$Survived[combi$Pclass == 1 & combi$Sex == 'male' & combi$Age < 21 & combi$SibSp < 1 & combi$SibSp < 1] <- 1
combi$Survived[combi$Pclass == 1 & combi$Sex == 'male' & combi$Age <= 48 & combi$Age >= 28 & combi$SibSp < 1 & combi$SibSp < 1] <- 1
combi$Survived[combi$Pclass == 1 & combi$Age > 48 & combi$Parch >= 1] <- 0

#Improve for Pclass 3  #SibSp + Parch 
	#Age > 21 is correct  18 is tmp
combi$Survived[combi$Pclass == 3 & combi$Age > 18 & combi$Parch >= 1] <- 0
combi$Survived[combi$Pclass == 3 & combi$Age <= 12 & combi$Parch >= 0] <- 0
combi$Survived[combi$Pclass == 3 & combi$p3mvt == 5] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = combi$Survived)
write.csv(submit, file = "upload.csv", row.names = FALSE)

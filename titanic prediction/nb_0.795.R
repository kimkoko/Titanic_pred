train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

str(train)
str(test)

summary(train)
Survival <- ifelse(train$Survived==1, "Survived","Dead")


library("ggplot2")

ggplot(train,aes(x=Sex, fill=Survival)) + facet_wrap(~Pclass) + geom_bar(width=0.5) + ggtitle("Survival rate based on Sex and Pclass")
ggplot(train,aes(y=Age, x=Survival,color=Sex)) + geom_violin() + ggtitle("Survival rate based on Age and Sex")
ggplot(train,aes(x=Age,fill=Survival)) + geom_histogram(binwidth=5,color="black") + ggtitle("Survival rate based on Age")

mosaicplot(table(ifelse(train$Survived==1,"Survived","Dead"),train$Sex),color=TRUE,main="Survival rate based on Sex") 
mosaicplot(table(ifelse(train$Survived==1,"Survived","Dead"),train$Pcla),color=TRUE,main="Survival rate based on Pclass")

train$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE
test$Survived <- NA

combi_titanic <- rbind(train, test)
table(combi_titanic$IsTrainSet)

colSums(is.na(combi_titanic)|combi_titanic=='')


head(combi_titanic$Name)

combi_titanic$Title <- gsub("^.*, (.*?)\\..*$", "\\1", combi_titanic$Name)
table(combi_titanic$Sex, combi_titanic$Title)
combi_titanic$Title[combi_titanic$Title == 'Mlle' | combi_titanic$Title=='Ms'| combi_titanic$Title == 'Lady'] <- 'Miss'
combi_titanic$Title[combi_titanic$Title == 'Mme' | combi_titanic$Title == 'Major'
                   | combi_titanic$Title == 'Sir'|combi_titanic$Title == 'Master'|combi_titanic$Title == 'Rev'] <- 'Mrs'
combi_titanic$Title[combi_titanic$Title == 'Capt' |combi_titanic$Title == 'Col' |combi_titanic$Title == 'Don'
                   |combi_titanic$Title == 'Dona'|combi_titanic$Title == 'Dr' |combi_titanic$Title == 'Jonkheer'
                   | combi_titanic$Title =='the Countess'] <- 'Other'

combi_titanic$IsMale <- ifelse(combi_titanic$Sex=="male",1,0) # indicator for ML 
#male=1 female =0
combi_titanic$Sex <- NULL


combi_titanic$Pclass <- as.factor(combi_titanic$Pclass)
combi_titanic$Embarked <- as.factor(combi_titanic$Embarked)
combi_titanic$Age <- as.factor(combi_titanic$Age)
combi_titanic$Fare <- as.factor(combi_titanic$Fare)
combi_titanic$Title <- as.factor(combi_titanic$Title)
combi_titanic$IsMale <- as.factor(combi_titanic$IsMale)





train <- combi_titanic[combi_titanic$IsTrainSet==TRUE,]
test <- combi_titanic[combi_titanic$IsTrainSet==FALSE,]

train$Survived <- as.factor(train$Survived)

library(e1071)

model <- naiveBayes(Survived~., train)
str(model)
prediction <- predict(model, test)
str(prediction)
summary(prediction)
output <- data.frame(test$PassengerId, prediction)
str(output)
Sex <- ifelse(test$IsMale==1,"Male","Female")
Survived_p <- ifelse(output$prediction ==1,"Survived","Dead")
table(Sex, Survived_p)
colnames(output)<-cbind("PassengerId","Survived")

write.csv(output, file = 'candy.csv', row.names=F)

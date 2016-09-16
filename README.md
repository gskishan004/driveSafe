# driveSafe

this app is to help the drivers to send message to the caller

#r

tr_i <- sample(1:150,120)
tr_d <- iris[tr_i,]
ts_d <- iris[-tr_i,]

install.packages("RWeka")
library(RWeka)

model <- J48(formula = Species~., data = tr_d)

install.packages("partykit")
library(partykit)

plot(model ,type="simple")


pred <- predict(object=model, newdata=ts_d)
actual_class <- ts_d$Species
addmargins(table(actual_class,pred))

install.packages("C50")
library(C50)

model2 <- C5.0(formula = Species~., data = tr_d)
pred2 <- predict(object=model2, newdata=ts_d)
plot(model2 ,type="simple")

#..................................................


car_evaluation <- read.csv("C:/Users/ishan01.TRN/Desktop/car.csv")
head(car_evaluation)
car_evaluation$acceptability[car_evaluation$acceptability == "vgood"] = "acc"
car_evaluation$acceptability[car_evaluation$acceptability == "good"] = "acc"
class(car_evaluation$acceptability)

car_evaluation$acceptability # levels are still 4, 

# to remove unused levels
car_evaluation$acceptability <- as.factor(as.character(car_evaluation$acceptability))

table(car_evaluation$acceptability)

set.seed(1556)
training_index <- sample(1:1728, 1382)
training_data <- car_evaluation[training_index,]
testing_data <- car_evaluation[-training_index,]

head(training_data, 10)

install.packages("rpart")
library("rpart")
model <- rpart(formula = acceptability~., data = training_data)
install.packages("rpart.plot")
library("rpart.plot")
rpart.plot(model)


prediction <- predict(model, newdata = testing_data, type ="class")
actual_class <- testing_data$acceptability

addmargins(table(actual_class, prediction))

install.packages("ISLR")
library(ISLR)
data("Default")
Default

norm <- function(x){return (x-min(x))/(max(x)-min(x))}

norm_b <- lapply(Default[3],norm)
norm_i <- lapply(Default[4],norm)

def_norm <- data.frame(c(Default[,c(1,2)],norm_b,norm_i))

View(def_norm)

train_size <-0.75*nrow(def_norm)

tr_i <- sample(1:nrow(def_norm),train_size)

tr_d <- def_norm[tr_i,c(3,4)]
te_d <- def_norm[-tr_i,c(3,4)]

tr_c <- def_norm[tr_i,c(1)]
te_c <- def_norm[-tr_i,c(1)]

library(class)


mod1 <- knn(train = tr_d,test=te_d, cl= tr_c , k=3)
addmargins(table(te_c, mod1))
                                  
                                  

#.... 16 Sep ....

install.packages("e1071")
library(e1071)
data("iris")
new_iris <- iris
levels(new_iris$Species) <- c(levels(new_iris$Species),"non-setosa")
new_iris$Species[new_iris$Species!="setosa"] <- "non-setosa"
new_iris$Species <- factor(new_iris$Species)
set.seed(10)
train_size =floor(0.75*nrow(new_iris))
train_i <-sample(1:nrow(new_iris),train_size)
train_d <- new_iris[train_i,]
test_d <- new_iris[-train_i,]

m1 <- svm(formula=Species~Petal.Length+Petal.Width, data= train_d , kernel="linear")
m1 <- svm(formula=Species~Petal.Length+Petal.Width, data= train_d , kernel="radial")

summary(m1)

train_d
plot(m1,train_d)

addmargins(table(predict(m1,test_d),test_d$Species))

#.... letter recognition using svm.....
#... good site for ref: http://www.svm-tutorial.com/2014/10/support-vector-regression-r/

data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data")
head(data)

svm_model <- svm(T ~ ., data)
summary(svm_model)

addmargins(table(predict(svm_model,data),data$T))


#.....naive bayes ...

install.packages("klaR")
library(klaR)
model <- NaiveBayes(formula= Species ~., data=train_d)

plot(model)
model$apriori
pred <- as.data.frame(predict(model,test_d))
pred














































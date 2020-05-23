
#Question 3.1 (a)

install.packages("kknn")
library(kknn)
set.seed(1)

# Reading the data
data <- read.table("credit_card_data-headers.txt", stringsAsFactors = FALSE, header = TRUE)


kmax <- 20
accuracy <- rep(0,kmax)
pred<- rep(0,kmax)
fitnum<-rep(0,kmax)

# use train.kknn for leave-one-out cross-validation 
model <- train.kknn(R1~.,data,kmax=kmax,scale=TRUE)

for (k in 1:kmax) {
  fitnum[k] <- fitted(model)[k]

  #Round to 0 or 1, add to pred list
  pred <- as.integer(fitnum[[k]][1:nrow(data)]+0.5) 
  
  accuracy[k] <- sum(pred == data[,11]) / nrow(data)
  cat("For k = ", k, "accuracy = ", accuracy[k],"\n")
}

maxind<-which(accuracy==max(accuracy),arr.ind=TRUE)
cat("max accurancy is ",accuracy[maxind],"K valve is", maxind,".\n")



#######################
# Question3.1(b)

# 60% for training 20% for testing and 20% for validation 

library(ISLR)
attach(Smarket)
install.packages("kernlab,")
library(kernlab)

#split training  data and testing data

mask_train = sample(nrow(data), size = floor(nrow(data) * 0.6))
trainset = data[mask_train,] 
remaining = data[-mask_train, ]  
#split test and validation
test_smp_siz = floor(0.5*nrow(remaining)) 
mask_rem = sample(nrow(remaining), size = test_smp_siz)

testset =remaining[mask_rem,] 
validateset=remaining[-mask_rem,] 


#test SVM
accresult <-rep(0,10)

lam <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000,10000) 

for (i in 1:10) {
  # fit model using training set
    modelsvm<- ksvm(as.matrix(trainset[,1:10]), as.factor(trainset[,11]), type = "C-svc", 
                  kernel = "vanilladot", C = lam[i],scaled=TRUE) 
  
  #  test models using validation set
  pred <- predict(modelsvm,validateset[,1:10])
  
  accresult[i] <- sum(pred == data[,11]) / nrow(data)
  cat("For lambda = ", lam[i], "accuracy = ", accresult[i],"\n")
}

cat("Best C value is ",lam[which.max(accresult[1:10])],"\n")
cat("Best validation set correctness is ",max(accresult[1:10]),"\n")

#retrain the model using best lam
testacc <- 0
modelsvmbest<- ksvm(as.matrix(trainset[,1:10]), as.factor(trainset[,11]), type = "C-svc", 
                kernel = "vanilladot", C = lam[which.max(accresult[1:10])],scaled=TRUE) 
#test model using test set
testacc<-sum(predict(modelsvmbest,testset[,1:10]) == testset[,11]) / nrow(testset)

cat("Performance on test data is = ",testacc,"\n")



install.packages("kernlab,")
library(kernlab)
#Import data to R
data <- read.table("credit_card_data-headers.txt", 
                 header = TRUE)

###########################
#Question 2.2.1
#set range for C/lam
lam <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000,10000) 
#loop lam
accuracy<- rep(0,10)
for (i in 1:10){
#call model
model <-
  ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="vanilladot",C =lam[i],scaled=TRUE)
# calculate a1…am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
# calculate a0
a0 <- - model@b
# see what the model predicts
pred <- predict(model,data[,1:10])
# see what fraction of the model’s predictions match the actual classification
accuracy[i]<-sum(pred == data[,11]) / nrow(data)

cat("For lam = ", lam[i], "accuracy = ", accuracy[i],"\n")
}

#result print
cat("Best C value is ",lam[which.max(accuracy)],"\n")
cat("Best validation set correctness is ",max(accuracy),"\n")
cat("The coes for a0 - a10 are:", a0,a)
cat("\n ######################################")



##########################################
#Question 2.2.2 try another kernal polunomial

accuracy<- rep(0,10)
for (i in 1:10){
  #call model
  model <-
    ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="polydot",C =lam[i],scaled=TRUE)
  # calculate a1…am
  a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
  # calculate a0
  a0 <- - model@b
  # see what the model predicts
  pred <- predict(model,data[,1:10])
  # see what fraction of the model’s predictions match the actual classification
  accuracy[i]<-sum(pred == data[,11]) / nrow(data)
  
  cat("For lam = ", lam[i], "accuracy = ", accuracy[i],"\n")
}
#result print
cat("For polydot kernal, Best C value is ",lam[which.max(accuracy)],"\n")
cat("Best validation set correctness is ",max(accuracy),"\n")
cat("The coe for a0 - a10 are:", a0,a)
cat("\n ######################################")


##########################################
#Question 2.2.3

maxind <- 0
accuracy <-rep(0,20)

#loop for K = X
for (x in 1:20){
#Loop for each data point i, include i itself
pred<- 0 
for (i in 1:nrow(data)){
#call kknn function 
  knnmodel <- kknn(R1 ~ .,data[-i, ], data[i,], k =x, scale=TRUE)
  fitnum <-knnmodel$fitted.values
  #Round to 0 or 1, add to pred 
  pred <- c(pred,as.integer(fitnum+0.5) )
 #Calculate the accuracy for given K
}
  
  accuracy[x] <- sum(pred == data[,11]) / nrow(data)
  
  cat("For k = ", x, "accuracy = ", accuracy[x],"\n") 
}
maxind<-which(accuracy==max(accuracy),arr.ind=TRUE)
cat("max accurancy is ",accuracy[maxind],"K valve is", maxind,".\n")




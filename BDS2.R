
# Load the libraries and Boston data
library(MASS)
data(Boston)

#Split as per the random seed of MMDD
set.seed(0830)

#split the data into two equals
X_train1 <- sample(1:nrow(Boston),nrow(Boston)/2)

#lstat - to have only col - lstat for training data
X_train_lstat <- Boston[X_train1,"lstat"]
pu
#rm - to have only col - rm for training data
X_train_rm <- Boston[X_train1,"rm"]

#medv - to have only col - medv for training label
y_train <- Boston[X_train1,"medv"]

#cbind - column bind giving final X_train including both lstat and rm columns
X_train <- cbind(X_train_lstat,X_train_rm)

#test data after removing the training data - contains all columns present in Boston data
X_test1 <- Boston[-X_train1,]

#lstat - to have only col - lstat for test data
X_test_lstat <- X_test1[,13]

#rm - to have only col - rm for test data
X_test_rm <- X_test1[,6]

#final X_test containing only lstat and rm
X_test <- cbind(X_test_lstat,X_test_rm)

#test label containing only medv
y_test <- X_test1[,14]

#DS function that takes nput as y_train - resiudals with updated residuals each time and outputs a list with
# 1. DS - "lstat" or "rm", 2. "threshold", 3. "best less than mean", 4. "best greater than mean"
DecisionStump <- function(y_train) {
  # length variables to store different lengths of datasets
  len_lstat_train <- length(X_train_lstat)
  len_rm_train <- length(X_train_rm)
  len_lstat_test <- length(X_test_lstat)
  len_rm_test <- length(X_test_rm)
  
  # sequences of threshold for lstat starting from 1.8 till 37.9 with step size - 0.1
  s_lstat_train <- seq(1.8, 37.9, 0.1)
  best_s_lstat = Inf
  best_threshold_lstat = 0

  # sequences of threshold for rm starting from 3.6 till 8.7 with step size - 0.1
  s_rm_train <- seq(3.6, 8.7, 0.1)
  best_s_rm = Inf
  best_threshold_rm = 0

  # to find index in training data for all values that are more and less than the threshold of lstat
  for (j in s_lstat_train) {
    index_lstat_train_less = c()
    index_lstat_train_more = c()
    for (i in 1:len_lstat_train){
      if (X_train_lstat[i] < j){
        index_lstat_train_less <- c(index_lstat_train_less,i)
      }
      else {
        index_lstat_train_more <- c(index_lstat_train_more,i)
      }
    }
    
    # to get training label less than and more than values for the indexes found in above step for lstat    
    y_train_lstat_less <- y_train[index_lstat_train_less]
    y_train_lstat_more <- y_train[index_lstat_train_more]
    
    # to calculate the less than and greater than mean values for lstat
    mean_lstat_less <- mean(y_train_lstat_less)
    mean_lstat_more <- mean(y_train_lstat_more)
    
    # finding RSS for each iteration for lstat
    RSS_lstat <- sum((y_train_lstat_less - mean_lstat_less)^2) + sum((y_train_lstat_more - mean_lstat_more)^2) 
    
    # checking the best RSS and getting corresponding best threshold, best less than and best greater than mean for lstat
    if (RSS_lstat < best_s_lstat ){
      best_s_lstat = RSS_lstat
      best_threshold_lstat = j
      best_lstat_mean_less = mean_lstat_less
      best_lstat_mean_more = mean_lstat_more
    }
  }
  
  # to find index in training data for all values that are more and less than the threshold of rm
  for (j in s_rm_train) {
    index_rm_train_less = c()
    index_rm_train_more = c()
    for (i in 1:len_rm_train){
      if (X_train_rm[i] < j){
        index_rm_train_less <- c(index_rm_train_less,i)
      }
      else {
        index_rm_train_more <- c(index_rm_train_more,i)
      }
    }
    
    # to get training label less than and more than values for the indexes found in above step for rm
    y_train_rm_less <- y_train[index_rm_train_less]
    y_train_rm_more <- y_train[index_rm_train_more]
    
    # to calculate the less than and greater than mean values for rm
    mean_rm_less <- mean(y_train_rm_less)
    mean_rm_more <- mean(y_train_rm_more)
    
    # finding RSS for each iteration for rm
    RSS_rm <- sum((y_train_rm_less - mean_rm_less)^2) + sum((y_train_rm_more - mean_rm_more)^2)
    
    # checking the best RSS and getting corresponding best threshold, best less than and best greater than mean for rm
    if (RSS_rm < best_s_rm ){
      best_s_rm = RSS_rm
      best_threshold_rm = j
      best_rm_mean_less = mean_rm_less
      best_rm_mean_more = mean_rm_more
    }
  }
  
  #if else condition to get the best threshold, best less than and best greater than mean values, precisely the DS
  # hence fitted the training data
  best_s <- ifelse (best_s_rm < best_s_lstat, best_threshold_rm, best_threshold_lstat)
  best_mean_less <- ifelse(best_s_rm < best_s_lstat, best_rm_mean_less,best_lstat_mean_less)
  best_mean_more <- ifelse(best_s_rm < best_s_lstat, best_rm_mean_more,best_lstat_mean_more)
  best_attr <- ifelse(best_s_rm < best_s_lstat, "rm", "lstat")
  
  # return array back to BDS with the below list of items
  returnArray= c(best_attr,best_s,best_mean_less,best_mean_more)
  
}


# IMPLEMENTATION OF BOOSTING DECISION STUMP

# number of trees - 1000
B <- 1000

# intitalizing matrix DS and fhat
DS <- matrix(nrow=B,ncol=4,byrow = TRUE )
fhat <- matrix(nrow=B,ncol=nrow(X_train),byrow= TRUE)


# iterating over each tree
for(i in 1:B) {
  
  # calling the DS function to train the training data and get the DSecision Stump
  DS[i,]<- DecisionStump(y_train)
    for(j in 1:nrow(X_train)) {
      if(DS[i,1] == "lstat") {
        lstatS <- as.numeric(DS[i,2])
        lstat_mean_less <- as.numeric(DS[i,3])
        lstat_mean_more <- as.numeric(DS[i,4])
        if(X_train_lstat[j] < lstatS) {
          fhat[i,j] <- lstat_mean_less
        }
        else {
          fhat[i,j] <- lstat_mean_more
        }
      }
      else if(DS[i,1] == "rm") {
        rmS <- as.numeric(DS[i,2])
        rm_mean_less <- as.numeric(DS[i,3])
        rm_mean_more <- as.numeric(DS[i,4])
        if(X_train_rm[j] < rmS) {
          fhat[i,j] <- rm_mean_less
        }
        else {
          fhat[i,j] <- rm_mean_more
        }
      }
      
      # updating the residual to pass the updated residual to the DS in next iteration
      y_train[j] <- y_train[j]-0.01*fhat[i,j]
    }
}


# Plot and get the test MSE for different values of B 
# Use the decision stump that is coming from the training model on test data and get the test MSE
plot_mse <- c()
B_for_plot <- seq(100,1000,100) 
for (m in 1:length(B_for_plot)) {
  ghat <- matrix(nrow=B_for_plot[m],ncol=nrow(X_train),byrow= TRUE)
  for(i in 1:B_for_plot[m]) {
    for(j in 1:nrow(X_test)) {
      if(DS[i,1] == "lstat") {
        lstatS <- as.numeric(DS[i,2])
        lstat_mean_less <- as.numeric(DS[i,3])
        lstat_mean_more <- as.numeric(DS[i,4])
        if(X_test_lstat[j] < lstatS) {
          ghat[i,j] <- lstat_mean_less
        }
        else {
          ghat[i,j] <- lstat_mean_more
        }
      }
      else if(DS[i,1] == "rm") {
        rmS <- as.numeric(DS[i,2])
        rm_mean_less <- as.numeric(DS[i,3])
        rm_mean_more <- as.numeric(DS[i,4])
        if(X_test_rm[j] < rmS) {
          ghat[i,j] <- rm_mean_less
        }
        else {
          ghat[i,j] <- rm_mean_more
        }
      }
    }
  }

  diff <- 0
  for(i in 1:nrow(X_test)) {
    rule_of_prediction = sum(0.01*ghat[,i])
    diff <- diff+ (y_test[i] - (rule_of_prediction))^2
  }
  
  # MSE for test calculation
  
  plot_mse[m] <- (1/(nrow(X_test)))*diff
  print(c('The test MSE for',B_for_plot[m],'trees is:',plot_mse[m]))
}


plot(B_for_plot, plot_mse, type = "b",col="red",main="The plot of Test MSE against different values of B",xlab="B values"
     ,ylab="Test MSE")


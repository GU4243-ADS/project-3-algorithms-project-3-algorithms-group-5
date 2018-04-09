#function: eva_mae
#input: predictoin data, test data
#output: mean absolute error

eva_mae <- function(prediction,test_data){
  matrix_mae <- matrix(ncol = 2)
  # match prediction and test data 
  for (i in 1:nrow(test_data)){
    preds  <- prediction[i, ]
    tests <- test_data[i,]
    all <- cbind(preds,tests)
    all <- na.omit(all)
    all <- as.data.frame(all)
    
    #calculate MAE by function MAE(based on MLmetrics)
    mae <- MAE(all$preds,all$tests)
    
    labels <- cbind(i, mae)
    matrix_mae <- rbind(matrix_mae,labels)
  }
  
  matrix_mae <- na.omit(matrix_mae)
  evaluation = sum(matrix_mae[,2])/i
  
  return(evaluation)
}

#function: eva_rmse
#input: predictoin data, test data
#output: root-mean-square deviation

eva_rmse <- function(prediction, test_data){
  matrix_rmse <- matrix(ncol = 2)
  for (i in 1:nrow(test_data)){
    preds  <- prediction[i, ]
    tests <- test_data[i,]
    all <- cbind(preds,tests)
    all <- na.omit(all)
    all <- as.data.frame(all)
    
    #calculate RMSE by function RMSE(based on Metrics)
    RMSE <- rmse(all$tests, all$preds)
    
    labels <- cbind(i, RMSE)
    matrix_rmse <- rbind(matrix_rmse,labels)
  }
  
  matrix_rmse <- na.omit(matrix_rmse)
  evaluation = sum(matrix_rmse[,2])/i
  
  return(evaluation)
}

#function: rank_scoring
#input: predictoin data, test data, alpha( = 5)
#output: rank scoring
rank_scoring <- function(pred, test, alpha = 5){
  
  #create empty matrix Ra
  Ra <- matrix()
  RaMax <- matrix()
  
  #get max achievable utility
  to_check_test <- apply(test, 1, check_to_test)
  
  #we need get the index of j(ranked matrix)
  ranked_pred <- t(apply(pred, 1, ranked)) 
  
  # Create a loop to calculate Ra and maxRa seperately
  for(i in 1:nrow(test)){
    
    index <- ranked_pred[i,]
    denominator <-  2^((index - 1)/(alpha - 1))
    adjust <- ifelse(pred[i,] - 0 > 0 , pred[i,] - 0 , 0)
    #max(Va,j - d, 0) To removie unavailable items
    
    Ra[i] <- sum(adjust / denominator)
    RaMax[i] <- length(to_check_test[[i]])
  }
  
  #At first, I wanna only calculate the denominator into a matrix by a loop, but it fails..(I don't know why :() So I put all(Ra and MaxRa) into the loop)
  #matrix_d = matrix()
  #loop{....}
  #matrix_d <- matrix_d[-1]
  #adjust <- ifelse(pred - 0 > 0, pred - 0, 0)
  #utility <- adjust/matrix_d
  #r_a <- rowSums(utility)
  
  R <- 100*(sum(Ra)/sum(RaMax))
  return(R)
}

## Helper Function

#check_to_test
#Input: test matrix
#Output: A matrix
#this function would help us to get the sieze of the test size(in RS, we use length() to get the maximum achievable utility)
check_to_test <- function(test){
    which(test == 1)
}


#ranked
#Input: pred matrix
#Output: Ordered matrix
#In roder to get index of j in Va,j, we need get a ranked matrix
ranked <- function(pred){
  order(pred ,decreasing = TRUE)
}


#In final version, I give up this function
#ranked prediction by test
#input: predictoin data, test data
#output: ranked matrix
ranks <- function(pred, test){
  matrix_ranks <- matrix(NA)
  for (i in 1:ncol(test)){
    rank_pred <- pred[,i] %>% as.data.frame()
    rank_pred <- rank_pred %>% mutate(id = row.names(rank_pred)) %>% arrange(desc(.)) %>% mutate(rank = 1:nrow(rank_pred))
    rank_test_by_pred <- test[,i] %>% as.data.frame()
    rank_test_by_pred <- rank_test_by_pred %>% mutate(id = row.names(rank_test_by_pred))
    names(rank_test_by_pred) <- c("test","id")
    all <- merge(rank_pred, rank_test_by_pred) 
    all <- all %>% arrange(rank) %>% select(test)
    names(all) <- i
    matrix_ranks <- cbind(matrix_ranks,all)
  } 
  matrix_ranks <- matrix_ranks[-1]
  
  return(matrix_ranks)
}
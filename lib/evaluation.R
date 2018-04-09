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
  
  # select test equals to 1
  to_test <- function(test){
    which(test == 1)
  }
  # to rank prediction 
  ranked <- function(pred){
    order(pred ,decreasing = TRUE)
  }
  
  # apply it into matrix
  to_check_test <- apply(test, 1, to_test)
  ranked_pred <- t(apply(pred, 1, ranked)) 
  
  #matrix_d = matrix()
  #adjust <- ifelse(pred - 0 > 0, pred - 0, 0)
  
  r_a <- matrix()
  r_a_max <- matrix()
  
  # create a loop to calculate Ra and maxRa seperately
  for(i in 1:nrow(test)){
    
    index_by_pred <- ranked_pred[i,]
    denominator <-  2^((index_by_pred-1)/(5-1))
    adjust <- ifelse(pred[i,] - 0 > 0 , pred[i,] - 0 , 0)
    
    r_a[i] <- sum( adjust / denominator )
    r_a_max[i] <- length(to_check_test[[i]])
    
  }
  
  #matrix_d <- matrix_d[-1]
  #utility <- adjust/matrix_d
  #r_a <- rowSums(utility)
  
  R <- 100*(sum(r_a)/sum(r_a_max))
  return(R)
}


###Helper functoin
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
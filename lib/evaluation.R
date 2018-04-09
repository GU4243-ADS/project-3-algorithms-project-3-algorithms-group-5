

eva_mae <- function(prediction,test_data){
  matrix_mae <- matrix(ncol = 2)
  for (i in 1:nrow(test_data)){
    preds  <- prediction[i, ]
    tests <- test_data[i,]
    all <- cbind(preds,tests)
    all <- na.omit(all)
    all <- as.data.frame(all)
    
    mae <- MAE(all$preds,all$tests)
    
    labels <- cbind(i, mae)
    matrix_mae <- rbind(matrix_mae,labels)
  }
  
  matrix_mae <- na.omit(matrix_mae)
  evaluation = sum(matrix_mae[,2])/i
  
  return(evaluation)
}

eva_rmse <- function(prediction, test_data){
  matrix_rmse <- matrix(ncol = 2)
  for (i in 1:nrow(test_data)){
    preds  <- prediction[i, ]
    tests <- test_data[i,]
    all <- cbind(preds,tests)
    all <- na.omit(all)
    all <- as.data.frame(all)
    
    RMSE <- rmse(all$tests, all$preds)
    
    labels <- cbind(i, RMSE)
    matrix_rmse <- rbind(matrix_rmse,labels)
  }
  
  matrix_rmse <- na.omit(matrix_rmse)
  evaluation = sum(matrix_rmse[,2])/i
  
  return(evaluation)
}

###rank test metrics by pred
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

rank_scoring <- function(pred, test,a){
  sorted_test <- as.matrix(ranks(pred,test))
  adjs_test <- max(sorted_test, 0)
  
  matrix_d = matrix()
  for (j in (1:ncol(sorted_test))){
    denominator = 2^((j-1)/(a-1))
    matrix_d <- cbind(matrix_d, denominator)
  }
  
  matrix_d <- matrix_d[,-1]
  
  r_a <- sorted_test/matrix_d
  r_a_sum <- rowSums(r_a)
  
  r_a_max = t(apply(sorted_test, 1, sort,decreasing=T)) / matrix_d
  r_a_max_sum <- rowSums(r_a_max)
  
  r = 100*(sum(r_a_sum)/sum(r_a_max_sum))
  return(r)
}
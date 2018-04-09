

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
rank_scoring <- function(predicted, web_mini_test, alpha){
  
  visited_ind <- apply(web_mini_test, 1, function(rrr){return(which(rrr==1))})
  ord <- t(apply(predicted, 1, function(rrr){return(order(rrr,decreasing = T))})) #rank list of predicted
  # R_a_s: Expected utility of a ranked list for user a 
  R_a_s <- rep(NA, nrow(web_mini_test))
  R_a_max <- rep(NA, nrow(web_mini_test))
  
  for(a in 1:nrow(web_mini_test)){
    d<-mean(predicted[a,])
    j<-ord[a,] # rank of test case in predicted
    m<-ifelse((predicted[a,]-d)>0,(predicted[a,]-d),0)
    
    R_a_s[a] <- sum( m / 2^((j-1)/(alpha-1)) )
    R_a_max[a] <- length(visited_ind[[a]])
  }
  
  # Final rank score for an experiment
  R <- sum(R_a_s) / sum(R_a_max)*100
  return(R)
}

rank_s <- function(pred, test,a){
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
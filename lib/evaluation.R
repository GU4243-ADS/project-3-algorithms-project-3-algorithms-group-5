eva_mae <- function(prediction,test_data){
  matrix_mae <- matrix(ncol = 2)
  for (i in 1:nrow(test_data)){
    cols_to_test <- which(!is.na(test_data[i, ]))
    pred  <- prediction[i, ]
    
    mae <- MAE(cols_to_test,pred)
    
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
    cols_to_test <- which(!is.na(test_data[i, ]))
    pred  <- prediction[i, ]
    
    RMSE <- rmse(cols_to_test,pred)
    
    labels <- cbind(i, RMSE)
    matrix_rmse <- rbind(matrix_rmse,labels)
  }
  
  matrix_rmse <- na.omit(matrix_rmse)
  evaluation = sum(matrix_rmse[,2])/i
  
  return(evaluation)
}

RS <- function(pred, test){
  pred = MS_pred[,1]
  test = MS_UI[,1]
  pred <- as.data.frame(pred)
  test <- as.data.frame(test)
  pred$id <- row.names(pred)
  test$id <- row.names(test)
  rs <- merge(pred,test)
  total = 0
  for (i in 2:nrow(rs)){
    p <- rs[i,2] - rs[i,3]
    p <- max(p,0) 
    denominator <- (i - 1) * (5 - 1)# set a = 5
    p <- p / denominator
    total = total + p
    if (is.na(total)){
      print(i)
      break
    }
  }
  return(total)
}

eva_rs <- function(prediction, test_data){
  matrix_rs <- matrix(ncol = 2)
  for (i in 1:nrow(test_data)){
    cols_to_test <- which(!is.na(test_data[i, ]))
    pred  <- prediction[i, ]
    
    rs <- RS(pred, cols_to_test)
    
    labels <- cbind(i, rs)
    matrix_rs <- rbind(matrix_rs,labels)
  }
  return(matrix_rs)
}

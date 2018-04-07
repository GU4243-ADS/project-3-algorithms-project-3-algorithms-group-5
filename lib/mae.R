mae <- function(prediction,test_data){
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
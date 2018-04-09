MS_for_CF<- function(data, C, t=0.001){
  # Initialize parameters
  mu<- runif(C, 1, 10)
  mu<- mu/sum(mu) 
  gamma_array<- array(runif(ncol(data)*2*C, 1, 10), dim=c(ncol(data), 2, C),
                      dimnames = list(colnames(data),
                                      c("1", "2"),
                                      c(1:C)))
  for(c in 1:C){gamma_array[, , c]<- gamma_array[, , c]/rowSums(gamma_array[, , c])}
  nomi<- array(0, dim=c(ncol(data), 2, C),
               dimnames = list(colnames(data),
                               c("1", "2"),
                               c(1:C)))
  aic_assignment<- matrix(0, ncol=C, nrow=nrow(data))
  aic_assignment_old<- matrix(0, ncol=C, nrow=nrow(data))
  sum_log_gamma<-matrix(0, ncol=C, nrow=nrow(data))
  nominator<-matrix(0, ncol=C, nrow=nrow(data))
  indicator<-array(NA, dim=c(nrow(data), ncol(data), 2),
                   dimnames = list(rownames(data),
                                   colnames(data),
                                   c("1", "2")))
  deno_indicator<-matrix(NA, nrow=nrow(data),ncol=ncol(data), 
                         dimnames = list(rownames(data),
                                         colnames(data)))
  deno<- matrix(0, nrow=ncol(data), ncol=C,
                dimnames = list(colnames(data),
                                c(1:C)))
  ##some of them may in loop
  denominator<-rep(1,C)
  #print("Initialization is done!")
  
  # set convergence criterion
  iter <-1  
  convergence_t <-1
  ##
  while ( iter < 15 & convergence_t > t ) {
    aic_assignment_old<- aic_assignment
    
    # E-step
    ## Update aic_assignment
    log_gamma<-log(gamma_array)
    log_mu<-log(mu)
    
    for(c in 1:C){
      for (i in 1:nrow(data)) {
        for (k in 1:2) {
          ###nominator first
          index<-which(data[i,]==k)
          sum_log_gamma[i,c]<-sum_log_gamma[i,c]+sum(log_gamma[index,k,c])
          nominator[i,c]<-log_mu[c]+sum_log_gamma[i,c]
        }
        denominator[i]<-logSumExp(nominator[i,]) 
      }
    }
    for(c in 1:C){
      aic_assignment[,c]<-exp(nominator[,c]-denominator)
    }
    # https://hips.seas.harvard.edu/blog/2013/01/09/computing-log-sum-exp/ & LogSumExp in "matrixStats" package
    # Methods used to deal with log/exp overflow
    print(paste(iter, "E-step done!"))
    
    # M-step
    ## Update mu
    mu<- apply(aic_assignment, 2, mean)
    
    ## Update gamma_array
    # nominator first
    for(c in 1:C){
      for(k in 1:2){
        for(j in colnames(data)){
          indicator[,j,k]<-ifelse(data[,j]==k,1,0)
          nomi[j,k,c]<-sum(aic_assignment[,c]* indicator[,j,k],na.rm = T)
          deno_indicator[,j]<-ifelse(is.na(data[,j]),0,1)
          # denominator 
          deno[j,c]<-sum(aic_assignment[,c]*deno_indicator[,j],na.rm = T)
        }
      }
    }
    # in case denominator==0
    for (c in 1:C) {
      for (j in colnames(data)) {
        if(deno[j,c]==0){
          deno[j,c]<-0.0001
        }
        gamma_array[j,,c]<-nomi[j,,c]/deno[j,c]
      }
    }
    
    print(paste(iter, "M-step done!"))
    convergence_t<-norm(aic_assignment-aic_assignment_old, 'O')
    print(paste("The norm is", convergence_t)) 
    print(paste("Iteration", iter, "done!"))
    iter<-iter+1
  }  
  return(list(mu=mu, gamma_array=gamma_array, aic_assignment=aic_assignment,aic_assignment_old=aic_assignment_old))
}  

system.time(results<-MS_for_CF(MS_UI,9))
save(results,file = "MS_for_CF_9.RData")
MS_EM_pred_9 <- movie_EM_pred(results$gamma_array,results$aic_assignment)
save(MS_EM_pred_9,file = "MS_EM_pred_9.RData")


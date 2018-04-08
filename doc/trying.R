for (i in 1:length(ms_mu)){
  clusterlist[[i]]<-sample_ms_ui[ms.df2[,"ms_kmeans_cluster"]==i,]
  for (j in 1:nrow(clusterlist[[i]])){
    for (k in 1:ncol(sample_ms_ui)){
      
      ################ numerator first ###########################
      #MS only has 2 possiblities 0 & 1
      each_value<-clusterlist[[i]][j,k]
      #sum of colum which has the same value with selected
      j_pro_matrix[j,k]<-sum(clusterlist[[i]][,k] ==each_value)
      
      #cal each probality for user j in each column k for cluster i
      a_vector[j,k] <- j_pro_matrix[j,k]/nrow(clusterlist[[i]]) #sum(ms_kmeans_cluster==i)
      
      #contiue multiply 
      pi_gamma[j]<-prod(a_vector[j,]) 
      numerator[j,i]<-ms_mu[i]*pi_gamma[j]
      
      ############ start calculate denominator ###################
      #seems like the sum of numerator???
      
    }
  }
}


##evaluation 
##MAE



install.packages("matrixStats")
library(matrixStats)

EM_for_CF<- function(data, C, t=0.001){

  # data<-movie_UI
  # C<-3
  
  # Initialize parameters
  #mu<-rep((1/C),C)
  #gamma_array<- array(1/6, dim=c(ncol(data), 6, C),
   #                   dimnames = list(colnames(data),
    #                                  c("1", "2", "3","4","5","6"),
     #                                 c(1:C)))
  #uniformed initial value caused local maximum?? see in Pizza
  mu<- runif(C, 1, 10)
  mu<- mu/sum(mu) 
  gamma_array<- array(runif(ncol(data)*6*C, 1, 10), dim=c(ncol(data), 6, C),
                      dimnames = list(colnames(data),
                                      c("1", "2", "3","4","5","6"),
                                      c(1:C)))
  for(c in 1:C){gamma_array[, , c]<- gamma_array[, , c]/rowSums(gamma_array[, , c])}
  
  
  nomi<- array(0, dim=c(ncol(data), 6, C),
               dimnames = list(colnames(data),
                               c("1", "2", "3","4","5","6"),
                               c(1:C)))
  aic_assignment<- matrix(0, ncol=C, nrow=nrow(data))
  aic_assignment_old<- matrix(0, ncol=C, nrow=nrow(data))
  sum_log_gamma<-matrix(0, ncol=C, nrow=nrow(data))
  nominator<-matrix(0, ncol=C, nrow=nrow(data))
  indicator<-array(NA, dim=c(nrow(data), ncol(data), 6),
                   dimnames = list(rownames(data),
                                   colnames(data),
                                   c("1", "2", "3","4","5","6")))
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
  
  while (iter < 12 & convergence_t > t ) {
    aic_assignment_old<- aic_assignment
    
    # E-step
    ## Update aic_assignment
    log_gamma<-log(gamma_array)
    log_mu<-log(mu)
    
    for(c in 1:C){
      for (i in 1:nrow(data)) {
        for (k in 1:6) {
          ###nominator first
          index<-which(data[i,]==k)
          sum_log_gamma[i,c]<-sum_log_gamma[i,c]+sum(log_gamma[index,k,c])
          nominator[i,c]<-log_mu[c]+sum_log_gamma[i,c]
        }
        ###denominator
        # denominator333<- apply(nominator,2,sum) 
        # need to be tested...Maybe also need log again and overflow again??
        # a_max_xn<- max(sum_log_gamma+log(mu)) seems like the 2 links are replacable? 
        denominator[i]<-logSumExp(nominator[i,]) 
        #nominator[,c]? log_mu[c]+sum_log_gamma[i,c]?
        ######## Important ####### Be aware of the dimension of denominator!!! It is 5055*1,
        # C is in sum!!!! It is the col sum of numerator
        # The above [,c] or [i,c] is different!!!!
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
      for(k in 1:6){
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

system.time(results<-EM_for_CF(movie_UI,11))
save(results,file = "EM_11.RData")




##Pred value
movie_EM_pred<-function(gamma,aic){
  pred<-matrix(NA,nrow=dim(aic)[1],ncol=dim(gamma)[1])
  cluster_of_user<-apply(aic, 1, which.max)
  rate<-c(1:dim(gamma)[2])
  for (i in 1:dim(aic)[1]) {
    for (j in 1:dim(gamma)[1]) {
      pred[i,j]<-sum(gamma[j,,cluster_of_user[i]]*rate)
    }
  }
  return(movie_EM_pred=pred)
}

movie_EM_pred_11 <- movie_EM_pred(results$gamma_array,results$aic_assignment)
# movie_EM_pred_11[1:10,1:10]
save(movie_EM_pred_11,file = "movie_EM_pred_11.RData")


  
  



a<-matrix(3,3,6)
b<-matrix(3,6,2)

# Matrix multiply
system.time(mat_ans<-a%*%b) 
mat_ans<-a%*%b

# Loop
loop_ans<-matrix(0,3,2)
system.time(
  for (i in 1:nrow(a)) {
    for (j in 1:ncol(b)) {
      loop_ans[i,j]<-sum(a[i,]*b[,j])
    }
  }
)

loop_ans==mat_ans
  
  




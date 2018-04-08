##take a sample from movie UI to simplify the calculation
sample_movie_ui<-movie_UI[1:60,1:500]
sample_ms_ui<-MS_UI[1:60,]

#kmeans with k=3 for ms
#can't do kmenas in movie due to NA
C=3
ms_kmeans <- kmeans(sample_ms_ui, C)
ms_kmeans_cluster <- ms_kmeans$cluster
#seems like useless ms.df <- data_frame(x = rownames(sample_ms_ui), cluster = ms_kmeans_cluster)
#ms.df2 & ms.df keep one is enough, if so keep df cause it is smaller
ms.df2 <-cbind(sample_ms_ui,ms_kmeans_cluster)

#EM S1
ms_mu<-rep((1/C),C)
ms_gamma<-1/2

#EM S2
#aic matrix
aic_assignment<-matrix(0, nrow = nrow(sample_ms_ui), ncol = C)
numerator<-matrix(0, nrow = nrow(sample_ms_ui), ncol = C)
j_pro_matrix<-matrix(0, nrow = nrow(sample_ms_ui), ncol = ncol(sample_ms_ui))
a_vector<-matrix(0, nrow = nrow(sample_ms_ui), ncol = ncol(sample_ms_ui))
pi_gamma<-vector(mode="numeric",length=nrow(sample_ms_ui)) 

#continuely mutiply for gamma in different class/part of numerator
# j: user,   i: cluster,   k:column of ms(better delete in the future if possible...)

for (i in 1:C){
  for (j in 1:nrow(sample_ms_ui)){
    for (k in 1:ncol(sample_ms_ui)){
      ################ numerator first ###########################
      #MS only has 2 possiblities 0 & 1
      each_value<-sample_ms_ui[j,k]
      #sum of colum which has the same value with selected
      j_pro_matrix[j,k]<-sum(sample_ms_ui[,k] ==each_value
                             & ms.df2[,270]==i ) #270=ncol(sample_ms_ui)+1

      #cal each probality for user j in each column k for cluster i
      a_vector[j,k] <- j_pro_matrix[j,k]/sum(ms_kmeans_cluster==i)
      
      #contiue multiply 
      pi_gamma[j]<-prod(a_vector[j,]) 
      numerator[j,i]<-ms_mu[i]*pi_gamma[j]
      
      ############ start calculate denominator ###################
      #seems like the sum of numerator???
      #denominator<-rowSums(numerator)
      
      ################ aic matrix ################################
      aic_assignment[j,i]<-numerator[j,i]/sum(numerator[j,])
    }
  }
}
###almost no difference with k means ...

#EM S3
ms_mu<-apply(aic_assignment,2,mean)
ms_gamma<-
#didn;t use default gamma....in s2....

  
  
  
  
  
  
  
  
  















##take a sample from MS UI to simplify the calculation
sample_ms_ui<-MS_UI[1:60,]

#Set cluster number to C=3
C=3

#EM S1
ms_mu<-rep((1/C),C)
ms_gamma<-1/2  #for all j k c

#EM S2
#aic matrix
aic_assignment<-matrix(0, nrow = nrow(sample_ms_ui), ncol = C)
numerator<-matrix(0, nrow = nrow(sample_ms_ui), ncol = C)
j_pro_matrix<-matrix(0, nrow = nrow(sample_ms_ui), ncol = ncol(sample_ms_ui))
a_vector<-matrix(0, nrow = nrow(sample_ms_ui), ncol = ncol(sample_ms_ui))
pi_gamma<-vector(mode="numeric",length=nrow(sample_ms_ui)) 

#continuely mutiply for gamma in different class/part of numerator
# j: user,   i: cluster,   k:column of ms (better delete in the future if possible...)

for (i in 1:C){
  for (j in 1:nrow(sample_ms_ui)){
    for (k in 1:ncol(sample_ms_ui)){
      ################ numerator first ###########################
      #MS only has 2 possiblities 0 & 1
      each_value<-sample_ms_ui[j,k]
      #sum of colum which has the same value with selected
      j_pro_matrix[j,k]<-sum(sample_ms_ui[,k] ==each_value & ms.df2[,270]==i ) #270=ncol(sample_ms_ui)+1
      #cal each probality for user j in each column k for cluster i
      #### in the first step they all equal to the initial value we set.
      a_vector[j,k] <- ms_gamma
      #             <- j_pro_matrix[j,k]/sum(ms_kmeans_cluster==i)
      #contiue multiply 
      pi_gamma[j]<-prod(a_vector[j,]) 
      numerator[j,i]<-ms_mu[i]*pi_gamma[j]
      
      ############ start calculate denominator ###################
      #seems like the sum of numerator???
      #denominator<-rowSums(numerator)
      
      ################ aic matrix ################################
      aic_assignment[j,i]<-1/C
      #                  <-numerator[j,i]/sum(numerator[j,])
      #这里需要再算一下，，，不知道为啥原来的有错。。。
    }
  }
}

#EM S3
ms_mu<-apply(aic_assignment,2,mean)
#ms_gamma<-

 
#aic_assignment<-matrix(0, nrow = nrow(sample_ms_ui), ncol = C)
#numerator<-matrix(0, nrow = nrow(sample_ms_ui), ncol = C)
#j_pro_matrix<-matrix(0, nrow = nrow(sample_ms_ui), ncol = ncol(sample_ms_ui))
#a_vector<-matrix(0, nrow = nrow(sample_ms_ui), ncol = ncol(sample_ms_ui))
#pi_gamma<-vector(mode="numeric",length=nrow(sample_ms_ui)) 

#continuely mutiply for gamma in different class/part of numerator
# j: user,   i: cluster,   k:column of ms(better delete in the future if possible...)

for (i in 1:C){
  for (j in 1:nrow(sample_ms_ui)){
    for (k in 1:ncol(sample_ms_ui)){
      ################ numerator first ###########################
      #MS only has 2 possiblities 0 & 1
      each_value<-sample_ms_ui[j,k]
      #sum of colum which has the same value with selected
      j_pro_matrix[j,k]<-sum(sample_ms_ui[,k] ==each_value)*ms_mu[i] 
      
      #contiue sum 
      numerator[j,i]<-sum(j_pro_matrix[j,]) 
      
      ############ start calculate denominator ###################
      #seems like the sum of numerator???
      #denominator<-rowSums(numerator)
      
      ################ aic matrix ################################
      ms_gamma<-numerator[j,i]/sum(aic_assignment[j,])
    }
  }
}

  hist<-movie_UI
  K=3

multinomialEM <- function(hist, K, t){
  N <- dim(hist)[1]; # Sample size (num. of histograms)
  D <- dim(hist)[2]; # Dimension (num. of bins per histogr 
  change <- 1; # Measures change in centroids
  # between iterations 
  iteration_counter <- 1; # Current iteration num.
  # Create vector containing cluster proportions (initially # all are equal)
  cp <- rep(1, K)/K;
  # Init: Random-select class means (w/o replacement) 
  indices <- sample(1:N, K);
  # Randomly assign centroids
  theta <- hist[indices , ]

  while (change > t) {
    #
    # E-Step using loops: #
    Phi <- matrix(0, N, K)
    logphi<-matrix(0, N, K)
    # Change vector to matrix for matrix multiplication
    for(nu in 1:K) { 
      for(n in 1:N) {
        
        #index<-which(!is.na(theta[1,])&!is.na(hist[1,]))
        if (exp(sum(hist[n,]*log(theta[nu,]),na.rm = T))==Inf)
        {
          Phi[n,nu]<-cp[nu]*exp(709)
        }else{
          Phi[n,nu]<-cp[nu]*exp(sum(hist[n,]*log(theta[nu,]),na.rm = T)) }
        
        if (exp(sum(hist[n,]*log(theta[nu,]),na.rm = T))==0)
        {
          logphi[n,nu]<-log(cp[nu])+log(exp(sum(hist[n,]*log(theta[nu,]),na.rm = T)))
          Phi[n,nu]<-exp(logphi[n,nu])
        }else{
          Phi[n,nu]<-cp[nu]*exp(sum(hist[n,]*log(theta[nu,]),na.rm = T)) }
   
      }
    }
    A <- matrix(0, N, K) 
    for(n in 1:N) {
      A[n, ] <- Phi[n, ]/sum(Phi[n, ]) 
      }
    #
    # M-Step using loops: #
    # Record old centroids, so that we can compute
    # change at the end. 
    theta_old <- theta;
    # Recompute centroids:
    for(nu in 1:K) { 
      #nu<-1
      theta[nu, ] <- 0; 
      for(n in 1:N) {
      #theta[nu, ] <- theta[nu, ] + A[n, nu] * hist[n, ] 
      theta[nu, ] <- rowSums( cbind (theta[nu, ],A[n, nu] * hist[n, ]), na.rm=TRUE)
      #theta[1, ] <- rowSums( cbind (theta[1, ],A[2, 1] * hist[2, ]), na.rm=TRUE)
      #theta[1, ] <- theta[1, ] + A[2, 1] * hist[2, ]
      }
    theta[nu, ] <- theta[nu, ]/sum(theta[nu, ],na.rm = T) 
    }
    # Recompute cluster proportions:
    for (nu in 1:K) { 
      cp[nu] <- sum(A[,nu])
    }
    cp <- cp/sum(cp);
    # Compute change:
    iteration_counter <- iteration_counter + 1;
    
    if (sum(is.na(theta_old))>0 ){
      theta_old[is.na(theta_old)] <- 0
      change <- norm(theta_old-theta, type = "O")
      theta_old[theta_old==0] <- NA
    }else
      change <- norm(theta_old-theta, type = "O")
    #a<-a[complete.cases(theta_old)]
    #change <- norm(theta_old-theta, type = "O")
    #theta_old[theta_old==0] <- NA
    }
  
# I is the index (= cluster num.) of the maximum;
return(list(mu=cp, gamma_array=Phi, pi_mat=A,I = apply(A, 1, which.max),interation<-interation_counter ) ) 
} 
# end of multinomialEM function

system.time(result<-multinomialEM(movie_UI,3,0.01)) 

#a<-multinomialEM(sample_movie_ui,3,0.03)





while (change > t) {
  # E-Step: #
  L <- log(theta);
  #L[is.na(L)] <- 0
  #hist[is.na(hist)] <- 0
  
  #hist[1,index]
  #L[1,index]
  #sum(hist[1,index]*t(L)[index,1])
  #logic <- !is.na(hist) & !is.na(L)
  #index<-which(!is.na(L)&!is.na(hist))
  
  Phi <- exp(hist[index] %*% t(L[index])) * cp
  
  Phi[which(is.infinite(Phi))]<- exp(709)*1/k
  
  A <- matrix(rep(1/rowSums(Phi), K), N, K) * Phi
  # M-Step: #
  theta_old <- theta;
  theta<- t(A) %*% hist;
  theta <- matrix(rep(1/apply(theta ,1,sum),D),K,D)*theta 
  cp<- colSums(A)
  cp <- cp/sum(cp)
  #Compute change:
  iteration_counter <- iteration_counter + 1;
  change <- norm(theta_old-theta, type = "1"); 
}





EM_for_CF<- function(data, C, iterations=12){
  
  num_of_users<- nrow(sample_movie_ui)
  num_of_movies<- ncol(sample_movie_ui)
  list_of_users<- rownames(sample_movie_ui)
  list_of_movies<- colnames(sample_movie_ui)
  ##sample_movie_ui need to be changed to data at end

C=3
num_of_movies<-500
mu<-rep((1/C),C)

# movie 1/6 ms 1/2 
gamma_array<- array(1/6, dim=c(num_of_movies, 6, C))

#for(d in 1:C){
 # gamma_array[, , d]<- gamma_array[, , d]/rowSums(gamma_array[, , d])
#}

num_of_users<-60
pi_mat<- matrix(1, ncol=C, nrow=num_of_users)
pi_mat_old<- matrix(0, ncol=C, nrow=num_of_users)
print("Initialization is done!")

iterations=12
for(iter in 1:iterations){
  print(paste("The norm is", norm(pi_mat-pi_mat_old, 'O'))) 
  pi_mat_old<- pi_mat
  
  # E-step
  ## Update pi_mat
  for (i in 1:num_of_users){
    user <- list_of_users[i]
    log_fi_prod<- log(rep(1, C))
    for(c in 1:C){
      ###########
      data<-movie_test
      sub_train<- data[data$User==user,][c("Movie", "Score")]
      movies_index<- match(sub_train$Movie, list_of_movies)
      score<- sub_train$Score
      all_index<- cbind(movies_index, score)
      for(j in 1:dim(sub_train)[1]){
        log_fi_prod[c]<- log_fi_prod[c] + log(gamma_array[all_index[j, 1], all_index[j, 2], c])
      }
    }
    
    max_log_fi<- max(log_fi_prod+log(mu))
    
    for(c in 1:C){
      pi_mat[i, c]<- exp(log(mu[c]) + log_fi_prod[c] - (max_log_fi + log((sum(exp(log(mu) + log_fi_prod - max_log_fi)))))) 
      }
  }
  
  print(paste(iter, "E-step done!"))
  

  # M-step
  ## Update mu
  mu<- apply(pi_mat, 2, mean)
  
  ## Update gamma_array
  for(c in 1:C){
    for(score in 1:6){
      for(movie in list_of_movies){
        movie_index<- match(movie, list_of_movies)
        sub_train<- data[data$Movie==movie, ][c("Movie", "User", "Score")]
        users_index<- match(sub_train$User, list_of_users)
        if(sum(pi_mat[users_index, c])==0){
          gamma_array[movie_index, score, c]<- 0
        }
        else{
          gamma_array[movie_index, score, c]<- 
            sum(pi_mat[users_index, c] * (sub_train$Score==score)) / sum(pi_mat[users_index, c])
        }
      }
    }
  }
  print(paste(iter, "M-step done!"))
  print(paste("Iteration", iter, "done!"))
}  
 return(list(mu=mu, gamma_array=gamma_array, pi_mat=pi_mat))
 }  




















EM_for_CF<- function(data, C, iterations=10, t=0.001){
  
  num_of_users<- length(unique(data$User))
  num_of_movies<- length(unique(data$Movie))
  list_of_users<- unique(data$User)
  list_of_movies<- unique(data$Movie)
  
  # Initialize parameters
  mu<-rep((1/C),C)
  gamma_array<- array(1/6, dim=c(num_of_movies, 6, C))
  
  aic_assignment<- matrix(1, ncol=C, nrow=num_of_users)
  aic_assignment_old<- matrix(0, ncol=C, nrow=num_of_users)
  print("Initialization is done!")
  
  # set convergence criterion
  iter <-1  
  convergence_t <-1
  while (iter < 10 & convergence_t > t ) {
    
    convergence_t<-norm(aic_assignment-aic_assignment_old, 'O')
    print(paste("The norm is", convergence_t)) 
    aic_assignment_old<- aic_assignment
    
    # E-step
    ## Update aic_assignment
    for (i in 1:num_of_users){
      user <- list_of_users[i]
      log_fi_prod<- log(rep(1, C))
      for(c in 1:C){
        sub_train<- data[data$User==user,][c("Movie", "Score")]
        movies_index<- match(sub_train$Movie, list_of_movies)
        score<- sub_train$Score
        all_index<- cbind(movies_index, score)
        for(j in 1:dim(sub_train)[1]){
          log_fi_prod[c]<- log_fi_prod[c] + log(gamma_array[all_index[j, 1], all_index[j, 2], c])
        }
      }
      
      max_log_fi<- max(log_fi_prod+log(mu))
      
      # Avoid overflow/underflow 
      for(c in 1:C){
        aic_assignment[i, c]<- exp(log(mu[c]) + log_fi_prod[c] - (max_log_fi + log((sum(exp(log(mu) + log_fi_prod - max_log_fi))))))  
        }
    }
    
    print(paste(iter, "E-step done!"))
    
    # M-step
    ## Update mu
    mu<- apply(aic_assignment, 2, mean)
    
    ## Update gamma_array
    for(c in 1:C){
      for(score in 1:6){
        for(movie in list_of_movies){
          movie_index<- match(movie, list_of_movies)
          sub_train<- data[data$Movie==movie, ][c("Movie", "User", "Score")]
          users_index<- match(sub_train$User, list_of_users)
          if(sum(aic_assignment[users_index, c])==0){
            gamma_array[movie_index, score, c]<- 0
          }
          else{
            gamma_array[movie_index, score, c]<- sum(aic_assignment[users_index, c] * (sub_train$Score==score)) / sum(aic_assignment[users_index, c])
          }
        }
      }
    }
    print(paste(iter, "M-step done!"))
    print(paste("Iteration", iter, "done!"))
    iter<-iter+1
  }  
  return(list(mu=mu, gamma_array=gamma_array, aic_assignment=aic_assignment))
}  

system.time(results<-EM_for_CF(movie_train,3))



system.time(chushizhi<-EM_for_CF(movie_train,3))



EM_for_CF<- function(data, C, iterations=5){
  
  num_of_users<- length(unique(data$User))
  num_of_movies<- length(unique(data$Movie))
  list_of_users<- unique(data$User)
  list_of_movies<- unique(data$Movie)
  
  # Initialize parameters
  mu<-rep((1/C),C)
  gamma_array<- array(1/6, dim=c(num_of_movies, 6, C))
  for(d in 1:C){
    gamma_array[, , d]<- gamma_array[, , d]/rowSums(gamma_array[, , d])
    }
  
  pi_mat<- matrix(1, ncol=C, nrow=num_of_users)
  pi_mat_old<- matrix(0, ncol=C, nrow=num_of_users)
  print("Initialization is done!")
  
  for(iter in 1:iterations){
    print(paste("The norm is", norm(pi_mat-pi_mat_old, 'O'))) 
    pi_mat_old<- pi_mat
    
    # E-step
    ## Update pi_mat
    for (i in 1:num_of_users){
      user <- list_of_users[i]
      log_fi_prod<- log(rep(1, C))
      for(c in 1:C){
        sub_train<- data[data$User==user,][c("Movie", "Score")]
        movies_index<- match(sub_train$Movie, list_of_movies)
        score<- sub_train$Score
        all_index<- cbind(movies_index, score)
        for(j in 1:dim(sub_train)[1]){
          log_fi_prod[c]<- log_fi_prod[c] + log(gamma_array[all_index[j, 1], all_index[j, 2], c])
        }
      }
      
      max_log_fi<- max(log_fi_prod+log(mu))
      
      for(c in 1:C){
        pi_mat[i, c]<- exp(log(mu[c]) + log_fi_prod[c] - (max_log_fi + log((sum(exp(log(mu) + log_fi_prod - max_log_fi))))))       }
    }
    
    print(paste(iter, "E-step done!"))
    
    # M-step
    ## Update mu
    mu<- apply(pi_mat, 2, mean)
    
    ## Update gamma_array
    for(c in 1:C){
      for(score in 1:6){
        for(movie in list_of_movies){
          movie_index<- match(movie, list_of_movies)
          sub_train<- data[data$Movie==movie, ][c("Movie", "User", "Score")]
          users_index<- match(sub_train$User, list_of_users)
          if(sum(pi_mat[users_index, c])==0){
            gamma_array[movie_index, score, c]<- 0
          }
          else{
            gamma_array[movie_index, score, c]<- 
              sum(pi_mat[users_index, c] * (sub_train$Score==score)) / sum(pi_mat[users_index, c])
          }
        }
      }
    }
    print(paste(iter, "M-step done!"))
    print(paste("Iteration", iter, "done!"))
  }  
  return(list(mu=mu, gamma_array=gamma_array, pi_mat=pi_mat))
} 



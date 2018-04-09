###################
# MEMORY-BASED ALGO FUNCTIONS
###################


MS_data_transform <- function(MS) {
  
  ## Calculate UI matrix for Microsoft data
  ##
  ## input: data   - Microsoft data in original form
  ##
  ## output: UI matrix
  
  
  # Find sorted lists of users and vroots
  users  <- sort(unique(MS$V2[MS$V1 == "C"]))
  vroots <- sort(unique(MS$V2[MS$V1 == "V"]))
  
  nu <- length(users)
  nv <- length(vroots)
  
  # Initiate the UI matrix
  UI            <- matrix(0, nrow = nu, ncol = nv)
  row.names(UI) <- users
  colnames(UI)  <- vroots
  
  user_locs <- which(MS$V1 == "C")
  
  # Cycle through the users and place 1's for the visited vroots.
  for (i in 1:nu) {
    name     <- MS$V2[user_locs[i]]
    this_row <- which(row.names(UI) == name)
    
    # Find the vroots
    if (i == nu) {
      v_names <- MS$V2[(user_locs[i] + 1):nrow(MS)]
    } else {
      v_names <- MS$V2[(user_locs[i] + 1):(user_locs[i+1] - 1)]
    }  
    
    # Place the 1's
    UI[this_row, colnames(UI) %in% v_names] <- 1
  }
  return(UI)
}



movie_data_transform <- function(movie) {
  
  ## Calculate UI matrix for eachmovie data
  ##
  ## input: data   - movie data in original form
  ##
  ## output: UI matrix
  
  
  # Find sorted lists of users and vroots
  users  <- sort(unique(movie$User))
  movies <- sort(unique(movie$Movie))
  
  # Initiate the UI matrix
  UI            <- matrix(NA, nrow = length(users), ncol = length(movies))
  row.names(UI) <- users
  colnames(UI)  <- movies
  
  # We cycle through the users, finding the user's movies and ratings
  for (i in 1:length(users)) {
    user    <- users[i]
    movies  <- movie$Movie[movie$User == user]
    ratings <- movie$Score[movie$User == user]
    
    ord     <- order(movies)
    movies  <- movies[ord]
    ratings <- ratings[ord]
    
    # Note that this relies on the fact that things are ordered
    UI[i, colnames(UI) %in% movies] <- ratings
  }
  return(UI)
}  






calc_weight <- function(data, method = "pearson") {
  
  ## Calculate similarity weight matrix
  ##
  ## input: data   - movie data or MS data in user-item matrix form
  ##        method - 'pearson'
  ##
  ## output: similarity weight matrix
  
  
  # Iniate the similarity weight matrix
  data       <- as.matrix(data)
  weight_mat <- matrix(NA, nrow = nrow(data), ncol = nrow(data))
  
  
  
  weight_func <- function(rowA, rowB) {
    
    # weight_func takes as input two rows (thought of as rows of the data matrix) and 
    # calculates the similarity between the two rows according to 'method'
    
    joint_values <- !is.na(rowA) & !is.na(rowB)
    mod_I <- length(joint_values)
    if (sum(joint_values) == 0) {
      return(0)
    } else {
      if (method == 'pearson') {
        return(cor(rowA[joint_values], rowB[joint_values], method = 'pearson'))
      }
      if (method == 'spearman') {
        return(cor(rowA[joint_values], rowB[joint_values], method = 'spearman'))
      }
      if (method == 'cosine') {
        return(sum(rowA[joint_values] * rowB[joint_values])/sqrt(sum(rowA[joint_values]^2)*sum(rowB[joint_values]^2)))
      }
      if (method == 'msd') {
        ## this method assumes all entries in scale of 0-1. Use normalized matrices for this method.
        ## https://ac.els-cdn.com/S0950705113003560/1-s2.0-S0950705113003560-main.pdf?_tid=fc704a89-11d3-45b5-b332-0a713076d429&acdnat=1523137722_9ff0c342bc4fe598519d0e49dbf7cf67
        ## page. 158
        return(mean((rowA[joint_values] - rowB[joint_values]) ^ 2))
      }
      if (method == 'entropy') {
        ## https://pdfs.semanticscholar.org/61f4/11eb2d5cb47f376f518aa6d3d49d8df3c6f2.pdf
        ## Entropy calculation
        wde <- 0
        diff <- abs(rowA[joint_values] - rowB[joint_values])
        for (i in 1:length(diff)) {
          di <- diff[i]
          count_di <- 0
          for (j in 1:length(diff)){
            if (di == diff[j]) {
              count_di <- count_di + 1
            }
          }
          p_di <- count_di / mod_I
          wde <- wde - p_di*log(p_di)*di
        }
        wde <- wde / mod_I
        return(wde)
      }
      if (method == 'simrank') {
        N_a <- length(rowA[which(rowA != 0)])
        N_b <- length(rowB[which(rowB != 0)])
        
      }
    }
  }
  
  # Loops over the rows and calculate sall similarities using weight_func
  for(i in 1:nrow(data)) {
    weight_mat[i, ] <- apply(data, 1, weight_func, data[i, ])
    print(i)
  }
  return(round(weight_mat, 4))
}


normalize_entropy <- function(entropy_sim) {
  for (i in 1:nrow(entropy_sim)) {
    max_wde <- max(entropy_sim[i, ])
    min_wde <- min(entropy_sim[i, ])
    entropy_sim[i, ] <- (max_wde - entropy_sim[i, ]) / (max_wde - min_wde)
  }
}

calc_simrank_weight <- function(data) {
  
  C_1 <- 0.8
  C_2 <- 0.8
  # initialization
  data <- as.matrix(data)
  s_user <- matrix(0, nrow = nrow(data), ncol = nrow(data))
  s_item <- matrix(0, nrow = ncol(data), ncol = ncol(data))
  # assign diagonal to be 1
  diag(s_user) <- 1
  diag(s_item) <- 1
  
  # K = 5
  for (k in 1:5) {
    # update s_user
    for (i in 1:nrow(data)){
      for (j in 1:nrow(data)){
        if (i == j) {
          s_user[i,j] <- 1
        } else {
          i_list <- data[i,-1]
          j_list <- data[j,-1]
          i_ids <- which(i_list != 0)
          j_ids <- which(j_list != 0)
          N_i <- length(i_ids)
          N_j <- length(j_ids)
          sum <- 0
          for (m in 1:N_i) {
            for (n in 1:N_j) {
              sum = sum + s_item[i_ids[m], j_ids[n]]
            }
          }
          s_user[i, j] <- C_1 / (N_i * N_j) * sum
        }
      }
      print(i)
    }
    cat("iter ", k, " s_user complete")
    
    for (i in 1:ncol(data)){
      for (j in 1:ncol(data)){
        if (i == j) {
          s_item[i, j] <- 1
        } else {
          i_list <- data[,i]
          j_list <- data[,j]
          i_ids <- i_list[which(i_list != 0)]
          j_ids <- j_list[which(j_list != 0)]
          N_i <- length(i_ids)
          N_j <- length(j_ids)
          sum <- 0
          for (m in 1:N_i) {
            for (n in 1:N_j) {
              sum <- sum + s_user[i_ids[m], j_ids[n]]
            }
          }
          s_item[i, j] <- C_2 / (N_i * N_j) * sum
        }
      }
      print(i)
    }
    cat("iter ", k, " s_item complete")
  }
  
  return(round(s_user, 4))
}

calc_simrankpp_weight <- function(data) {
  
  C_1 <- 0.8
  C_2 <- 0.8
  # initialization
  data <- as.matrix(data)
  normalize <- function(x) {
    return(x/rowsum(x))
  }
  norm_data <- lapply(data, normalize)
  s_user <- matrix(0, nrow = nrow(data), ncol = nrow(data))
  s_item <- matrix(0, nrow = ncol(data), ncol = ncol(data))
  # assign diagonal to be 1
  for (i in 1:nrow(s_user)) {
    s_user[i,i] <- 1
  }
  for (i in 1:nrow(s_item)) {
    s_item[i,i] <- 1
  }
  
  # K = 5
  for (k in 1:5) {
    # update s_user
    for (i in 1:nrow(data)){
      for (j in 1:nrow(data)){
        if (i == j) {
          s_user[i, j] <- 1
        } else {
          i_list <- !is.na(data[i,])
          j_list <- !is.na(data[j,])
          mod_I <- length(i_list & j_list)
          spread_i <- exp(-var(norm_data[i, i_list]))
          spread_j <- exp(-var(norm_data[j, j_list]))
          s_user[i, j] <- C_1 * sum(s_item[i_list, j_list] * norm_data[i, i_list] * norm_data[j, j_list] * spread_i * spread_j) / (2 ^ mod_I)
        }
      }
    }
    
    for (i in 1:ncol(data)){
      for (j in 1:ncol(data)){
        if (i == j) {
          s_item[i, j] <- 1
        } else {
          i_list <- !is.na(data[,i])
          j_list <- !is.na(data[,j])
          mod_I <- length(i_list & j_list)
          spread_i <- exp(-var(norm_data[i, i_list]))
          spread_j <- exp(-var(norm_data[j, j_list]))
          s_item[i, j] <- C_2 * sum(s_user[i_list, j_list] * norm_data[i_list, i] * norm_data[j_list, j] * spread_i * spread_j) / (2 ^ mod_I)
        }
      }
    }
  }
  
  return(round(s_user, 4))
}

pred_matrix <- function(data, simweights) {
  
  ## Calculate prediction matrix
  ##
  ## input: data   - movie data or MS data in user-item matrix form
  ##        simweights - a matrix of similarity weights
  ##
  ## output: prediction matrix
  
  # Initiate the prediction matrix.
  pred_mat <- data
  
  # Change MS entries from 0 to NA
  pred_mat[pred_mat == 0] <- NA
  
  row_avgs <- apply(data, 1, mean, na.rm = TRUE)
  
  for(i in 1:nrow(data)) {
    
    # Find columns we need to predict for user i and sim weights for user i
    cols_to_predict <- which(is.na(pred_mat[i, ]))
    num_cols        <- length(cols_to_predict)
    neighb_weights  <- simweights[i, ]
    
    # Transform the UI matrix into a deviation matrix since we want to calculate
    # weighted averages of the deviations
    dev_mat     <- data - matrix(rep(row_avgs, ncol(data)), ncol = ncol(data))
    weight_mat  <- matrix(rep(neighb_weights, ncol(data)), ncol = ncol(data))
    
    weight_sub <- weight_mat[, cols_to_predict]
    dev_sub    <- dev_mat[ ,cols_to_predict]
    
    pred_mat[i, cols_to_predict] <- row_avgs[i] +  apply(dev_sub * weight_sub, 2, sum, na.rm = TRUE)/sum(neighb_weights, na.rm = TRUE)
    print(i)
  }
  
  return(pred_mat)
}

###################
# MODEL-BASED ALGO FUNCTIONS
###################

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
  ##
  while ( iter < 12 & convergence_t > t ) {
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
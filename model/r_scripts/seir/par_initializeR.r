par_initializeR <- function(data,model="Multinomial", init_pars, period_start, init_state_num, fix_pars, opt_num = 100,... ) {
 
  if(period_start[1]==1){
    n_period=length(period_start)
  }
  else{
    n_period=length(period_start)+1
  }
  
  negLL_func<- function(pars,period_start = parent.frame()$period_start, init_state_num = parent.frame()$init_state_num, 
                          ftime = parent.frame()$ftime, fix_pars = parent.frame()$fix_pars){
  
    pars=c(exp(c(pars[1:n_period])),invlogit(pars[(n_period+1):(2*n_period)]))
    ypred <- model_deterministic_simulateR(init_obs = init_state_num, period_start = period_start, 
                                           times = ftime, pars = pars, fix_pars = fix_pars)
  
    p_new_pred_n <- round(ypred[, 11])
    p_new_pred_prob <- ypred[, 12]
    rd_new_pred_n <- round(ypred[, 13])
    r_new_pred_prob <- ypred[, 14]
    d_new_pred_prob <- ypred[, 15]
    p <- c()
    if(model=="Poisson"){
      ypred <-  p_new_pred_n  *  p_new_pred_prob 
      p<-try(dpois(data$Confirmed, round(ypred),log=T),silent=T)
      if(any(is.nan(p))||any(p==-Inf)){
        logL <- -Inf
      }
      else{
        logL <- sum(p)
      }
    }
    else if(model=="Binomial"){
      p <- dbinom(data$Confirmed,round( p_new_pred_n), p_new_pred_prob, log = T)
      if(any(p == -Inf) || any(is.nan(p))){
        logL <- -Inf
      }else{
        logL <- sum(p)
      }
    }
    else if(model=="Multinomial"){
      for(i in 1:dim(ypred)[1]){
        p[i] = try(dbinom(data$Confirmed[i], round(p_new_pred_n[i]) , p_new_pred_prob[i], log = T),silent=T)
        
        obs_size = data$Confirmed[max(i - 1, 1)]
        pred_size = round(ypred[max(i - 1, 1),"P"])
        
        if((data$Recovered[i] + data$Deceased[i]) > pred_size){
          logL = -Inf
        }
        else{
          p[i] = p[i] + try(dmultinom(c(round(c(data$Recovered[i], data$Deceased[i])), pred_size - (data$Recovered[i] + data$Deceased[i])),
                                      pred_size, c(r_new_pred_prob[i], d_new_pred_prob[i], 1-r_new_pred_prob[i]-d_new_pred_prob[i]),
                                      log = T),silent=T)
        }
      }
      if(any(p ==-Inf) || any(is.nan(p))){
        logL <- -Inf
      }else{
        logL <- sum(p)
      }
    }
    
    return(-logL)
  }
  ftime <- 1:dim(data)[1]
  result_mat <- matrix(NA, opt_num, n_period * 2 + 2)
  colnames(result_mat) <- c("likelihood", "convergence", unlist(lapply(c("b","r") , function(y) lapply(1:n_period, function(x) paste(y,x, sep = "")))))
  result_mat[, 1] <- -Inf
  cat("Finding MLE", fill = T)
  i = 1
  while(i <= opt_num) {
    passed <- FALSE
    while (!passed) {
      if(is.null(init_pars)){
        initial_param<-c(runif(n_period,0.1,1),rep(0.2,times=n_period))
        }
     else{
       initial_param<-init_pars
     }
      mle_opt <- try(optim(par = c(log(c(initial_param[1:n_period])),logit(c(initial_param[(n_period+1):(2*n_period)]))), f =  negLL_func,init_state_num = init_state_num,period_start=period_start,fix_pars=fix_pars, 
                           ftime =  ftime,hessian = FALSE),silent=T)
      passed <- exists("mle_opt")
    } 
    
    if(class(mle_opt) == "try-error"){
      next
    }
    
    try(result_mat[i, 1] <- mle_opt$value,silent=T)
    try(result_mat[i, 2] <- mle_opt$convergence,silent=T)
    try(result_mat[i, (3:(n_period+2))] <- exp(mle_opt$par[1:n_period]),silent=T)
    try(result_mat[i,(n_period+3):(2*n_period+2)]<-invlogit(mle_opt$par[(n_period+1):(2*n_period)]),silent=T)
    cat(i, "MLE run finished!", fill = T)
    rm(mle_opt)
    
    i = i+1
  }
  
  result <- result_mat[which(result_mat[, 1] == min(result_mat[which(result_mat[,1]!=-Inf,1)]))[1], ]
  
  return(result)
}

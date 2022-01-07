mcmc_performR <- function(data, period_start, init_state_num, init_pars, fix_pars,
                          niter = 1e5, BurnIn = 5e3, model = "Multinomial", trace_num = 100,
                          step_pars = init_pars/1000, ... ){
  Prior_func <- function(pars){
    if(any(pars < 0) || any(pars[(length(pars)/2 + 1):length(pars)] > 1)){
      return (0)
    }else{
      return (1) ## Set a non-informative prior
    }
  }
  
  ftime <- 1:dim(data)[1]
  
  Log_Likelihood_calculateR <- function(pars, period_start = parent.frame()$period_start, init_state_num = parent.frame()$init_state_num, 
                                        ftime = parent.frame()$ftime, fix_pars = parent.frame()$fix_pars){
    ypred <- model_deterministic_simulateR(init_obs = init_state_num, period_start = parent.frame()$period_start, 
                                        times = ftime, pars = pars, fix_pars = parent.frame()$fix_pars)
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
      p <- try(dbinom(data$Confirmed,round( p_new_pred_n), p_new_pred_prob, log = T),silent = T)
      if(any(p == -Inf) || any(is.nan(p))){
        logL <- -Inf
      }else{
        logL <- sum(p)
      }
    }
    else if(model=="Multinomial"){
      for(i in 1:dim(ypred)[1]){
      p[i] = try(dbinom(data$Confirmed[i], round(p_new_pred_n[i]) , p_new_pred_prob[i], log = T),silent =T)
      obs_size = data$Confirmed[max(i - 1, 1)]
      pred_size = round(ypred[max(i - 1, 1),"P"])
      if((data$Recovered[i] + data$Deceased[i]) > pred_size){
        p[i] = -Inf
      }
      else{
        p[i] = p[i] + try(dmultinom(c(round(c(data$Recovered[i], data$Deceased[i])), pred_size - (data$Recovered[i] + data$Deceased[i])),
                                pred_size, c(r_new_pred_prob[i], d_new_pred_prob[i], 1-r_new_pred_prob[i]-d_new_pred_prob[i]),
                                log = T),silent=T)
      }
    }
    if(any(p == -Inf) || any(is.nan(p))){
      logL <- -Inf
    }else{
      logL <- sum(p)
    }
   }
    return(logL)
  }

  if(period_start[1] != 1){
    period_start = c(1, period_start)
  }
  if(length(period_start) != length(init_pars)/2) 
    stop("Number of period is not equal to number of initial parameters")
  
  ## build the matrix to store the results
  
  n_period = length(init_pars)/2 # number of periods
  pmat <- matrix(0, ((niter + BurnIn) / trace_num) + 1, 3*n_period) ## parameters + R0 for n_period periods
  colnames(pmat) <- unlist(lapply(c("b","r","R0") , function(y) lapply(1:n_period, function(x) paste(y,x, sep = ""))))
  pmat[1, 1:(2*n_period)] <- init_pars
  R0_est <- R0_calculateR(estpar = init_pars, fix_pars = fix_pars)
  pmat[1, ((2*n_period)+1):(3*n_period)] <- R0_est
  
  ## Start MCMC
  pars_now <- init_pars
  
  cat("MCMC:", fill = T) 
  for(i in 2:(niter + BurnIn)){
    pars_new <- rep(0, 2*n_period)
    for(j in 1:(2*n_period)){
      pars_new[j] <- rnorm(1, mean = pars_now[j], sd = step_pars[j])
    }
    A <- 0
    if(Prior_func(pars_new) > 0){ 
      ll_pars_new <- Log_Likelihood_calculateR(pars = pars_new)
      if(ll_pars_new != -Inf) {
        ll_pars_now <- Log_Likelihood_calculateR(pars = pars_now)
        A <-  exp(1)^(ll_pars_new - ll_pars_now) # Prior_func(pars_new) / Prior_func(pars_now) * 10^(ll_pars_new - ll_pars_now)
      }
    }
    
    if(runif(1) < A){
      pars_now <- pars_new
    }
    if(i %% trace_num == 0) {
      R0_est <- R0_calculateR(estpar = pars_now, fix_pars = fix_pars)
      pmat[(i / trace_num) + 1, 1:(2*n_period)] <- pars_now
      pmat[(i / trace_num) + 1, ((2*n_period)+1):(3*n_period)] <- R0_est
      
    }
    if(i%%(niter/10) == 0) cat("Iter", i, " A =", round(A, digits=4), " : ", round(pars_now, digits=4), fill = T)
    
  }
  mcmc_estimates = pmat[-c(1:(BurnIn / trace_num + 1)), ]
  return(mcmc_estimates)
  
}
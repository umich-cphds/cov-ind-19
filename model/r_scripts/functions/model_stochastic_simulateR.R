model_stochastic_simulateR <- function(init_obs_current, init_obs_daily, period_start, times, pars, fix_pars, T_predict, ...){
  stochastic_sampleR <- function(stage_pars, fix_pars, old_values) {
    ## stage pars
    b = stage_pars[1]
    r = stage_pars[2]
    ## fixed pars
    alpha_p = fix_pars[1]  
    alpha_u = fix_pars[2]
    beta_1 = fix_pars[3]
    beta_2 = fix_pars[4]
    delta_1 = fix_pars[5]
    delta_2 = fix_pars[6]
    lambda = fix_pars[7]
    mu = fix_pars[8]
    mu_c = fix_pars[9]
    De = fix_pars[10]
    Dr = fix_pars[11]
    f = fix_pars[12]
    N = fix_pars[13]
    ## old values
    S = old_values[1]
    E = old_values[2]
    U = old_values[3]
    P = old_values[4]
    F = old_values[5]
    RU = old_values[6]
    RR = old_values[7]
    DU = old_values[8]
    DR = old_values[9]
    ##
    
    pS_vec <- c( b * (alpha_p * P + alpha_u * U + F) / N , mu, 1 -  b * (alpha_p * P + alpha_u * U + F) / N - mu)
    sample_S <- rmultinom(1, size = S, prob = pS_vec)
    ##
    pE_vec <- c((1-r) / De, r * (1-f) / De, r*f/De, mu,1 - 1 / De - mu)
    sample_E <- rmultinom(1, size = E, prob = pE_vec)
    ##
    pU_vec <- c(1/(beta_1*Dr),delta_1*mu_c,mu,1-1/(beta_1*Dr)-delta_1*mu_c-mu)
    
    sample_U <- rmultinom(1, size = U, prob = pU_vec)
    ##
    pP_vec <- c(1 / Dr, mu_c,mu, 1 - 1 / Dr - mu-mu_c)
    sample_P <- rmultinom(1, size = P, prob = pP_vec)
    ##
    pF_vec <- c(beta_2 / Dr, mu_c/delta_2,mu,1 -beta_2 / Dr- mu_c/delta_2-mu)
    sample_F <- rmultinom(1, size = F, prob = pF_vec)
    ##
    pRU_vec <- c( mu,1-mu)
    sample_RU <- rmultinom(1, size = RU, prob = pRU_vec)
    ##
    pRR_vec <- c(mu,1-mu)
    sample_RR <- rmultinom(1, size = RR, prob = pRR_vec)
    ## new values
    S_new <- sample_S[3] + lambda*N
    E_new <- sample_E[5] + sample_S[1]
    U_new <- sample_U[4] + sample_E[1]
    P_new <- sample_P[4] + sample_E[2]
    F_new <- sample_F[4] + sample_E[3]
    RU_new <- sample_RU[2] + sample_U[1]+sample_F[1]
    RR_new <- sample_RR[2] + sample_P[1]
    DU_new <- DU + sample_U[2]+sample_F[2]
    DR_new <- DR + sample_P[2]
    est_P_new<- sample_E[2]
    est_RR_new<- sample_P[1]
    est_DR_new <- sample_P[2]
    est_F_new<-sample_E[3]
    est_U_new<-sample_E[1]
    return(c(S_new, E_new, U_new, P_new, F_new, RU_new, RR_new, DU_new, DR_new, 
             est_P_new,est_RR_new,est_DR_new,est_F_new,est_U_new))
  }
  n_period = length(period_start)
  
  which.period <- function(i, phase = period_start){ # function to determine which period i falls in
    sum(i >= phase)
  }

  for(i in 1:length(times)){
    stage_pars <- c(b = pars[which.period(i)], r = pars[n_period + which.period(i)])
    if(i == 1) {
      old_values <- init_obs_current
      results = matrix(c(init_obs_current,init_obs_daily,0,0), nrow = 1)
    } else {
      now_values <- stochastic_sampleR(stage_pars = stage_pars, fix_pars = fix_pars, old_values = old_values)
      results = rbind(results, now_values)
      old_values <- now_values[1:9]
    }
  }
  
  return(results)
}
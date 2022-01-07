R0_calculateR <- function(estpar, fix_pars){
  alpha_p = fix_pars["alpha_p"]
  alpha_u = fix_pars["alpha_u"]
  beta_1 = fix_pars["beta_1"]
  beta_2 = fix_pars["beta_2"]
  delta_1 = fix_pars["delta_1"]
  delta_2 = fix_pars["delta_2"]
  lambda = fix_pars["lambda"]
  mu = fix_pars["mu"]
  mu_c = fix_pars["mu_c"]
  De = fix_pars["De"]
  Dr = fix_pars["Dr"]
  f = fix_pars["f"]
  N = fix_pars["N"]
  
  n_period = length(estpar)/2
  R0 = rep(0, n_period)

  for(i in 1:n_period){
    b = estpar[i]                 # values of beta for i_th period
    r = estpar[n_period + i]      # values of r for i_th period
    
    R0[i] = (b / (mu * De + 1)) * (alpha_u * (1 - r) / (1 / (beta_1 * Dr) + delta_1 * mu_c + mu)  + 
                                   alpha_p * r * (1 - f) / (1 / Dr + mu_c + mu) + 
                                   r * f / (beta_2 / Dr + mu_c / delta_2 + mu))
  }
  return(R0)
}
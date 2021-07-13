tvt.eSIR <-
  function (Y,
            R,
            pi0 = NULL,
            change_time = NULL,
            omega0 = NULL,
            change_time_omega = NULL,
            exponential = FALSE,
            lambda0 = NULL,
            rayleigh = FALSE,
            sigma0 = NULL,
            begin_str = "01/13/2020",
            T_fin = 200,
            nchain = 4,
            nadapt = 1e4,
            M = 5e2,
            thn = 10,
            nburnin = 2e2,
            dic = FALSE,
            death_in_R = 0.02,
            beta0 = 0.2586,
            gamma0 = 0.0821,
            gamma_s0 = 1 / 240,
            R0 = beta0 / gamma0,
            gamma0_sd = 0.1,
            gamma_s0_sd = 0.1,
            R0_sd = 1,
            casename = "tvt.eSIR",
            file_add = character(0),
            add_death = FALSE,
            save_files = FALSE,
            save_mcmc = FALSE,
            save_plot_data = FALSE,
            eps = 1e-10) {
    beta0 <- R0 * gamma0
    len <- round(M / thn) * nchain #number of MCMC draws in total
    
    T_prime <- length(Y)
    if (T_prime != length(R))
      stop("Y and R should be matched.")
    Y <- pmax(Y, eps)
    R <- pmax(R, eps)
    if (add_death == T && death_in_R == 0.02) {
      message(
        "use the default death_in_R which is equal to 0.02 to plot the death curve in the removed process forecast plot"
      )
    }
    begin <- chron(dates. = begin_str)
    chron_ls <- chron(begin:(begin + T_fin))
    end <- chron(begin:(begin + T_fin))[T_fin]
    message(
      paste0(
        "The follow-up is from ",
        begin,
        " to ",
        end,
        " and the last observed date is ",
        chron_ls [T_prime],
        "."
      )
    )
    # current data up to this date
    gamma_var <- gamma0_sd ^ 2
    lognorm_gamma_parm <- lognorm.parm(gamma0, gamma_var)
    gamma_s_var <- gamma_s0_sd ^ 2
    lognorm_gamma_s_parm <- lognorm.parm(gamma_s0, gamma_s_var)
    R0_var <- R0_sd ^ 2
    lognorm_R0_parm <- lognorm.parm(R0, R0_var)
    
    if (exponential == FALSE &
        !is.null(change_time) & !is.null(pi0)) {
      #pi0<- c(1,0.9,0.5,0.1)
      message("Running for step-function pi(t)")
      if (length(change_time) != length(pi0) - 1) {
        stop("We need the length of vector change_time to be the length of pi0 minus 1. ")
      }
      change_time_chorn <-
        c(begin - 1, chron(dates. = change_time), end)
      pi <- rep(pi0, diff(change_time_chorn))
      
    } else if (exponential == TRUE & !is.null(lambda0)) {
      message("Running for exponential-function pi(t)")
      pi <- exp(-lambda0 * (0:(T_fin - 1)))
    } else {
      message("Running without pi(t)")
      
      pi <- rep(1, T_fin)
    }
    
    if (!is.null(change_time_omega) & !is.null(omega0)) {
      message("Running for step-function omega(t)")
      if (length(change_time_omega) != length(omega0) - 1) {
        stop("We need the length of vector change_time_omega to be the length of omega0 minus 1. ")
      }
      change_time_omega_chorn <-
        c(begin - 1, chron(dates. = change_time_omega), end)
      omega <- rep(omega0, diff(change_time_omega_chorn))
    } else if (rayleigh == TRUE & !is.null(sigma0)) {
      message("Running for Rayleigh-distributed omega(t)")
      omega <-
        prayleigh(q = 1:T_fin,
                  scale = sigma0,
                  lower.tail = TRUE)
    } else {
      message("Running without omega(t)")
      
      omega <- rep(0, T_fin)
    }
    
    ################ MCMC ##########
    model1.string <- paste0(
      "
  model{
     for(t in 2:(T_prime+1)){
       Km[t-1,1] <- -beta*pi[t-1]*theta[t-1,1]*theta[t-1,2]+omega[t-1]*gamma_s*theta[t-1,3]
       Km[t-1,9] <- gamma*theta[t-1,2]-omega[t-1]*gamma_s*theta[t-1,3]
       Km[t-1,5] <- -Km[t-1,1]-Km[t-1,9]

       Km[t-1,2] <- -beta*pi[t-1]*(theta[t-1,1]+0.5*Km[t-1,1])*(theta[t-1,2]+0.5*Km[t-1,5])+omega[t-1]*gamma_s*(theta[t-1,3]+0.5*Km[t-1,9])
       Km[t-1,10] <- gamma*(theta[t-1,2]+0.5*Km[t-1,5])-omega[t-1]*gamma_s*(theta[t-1,3]+0.5*Km[t-1,9])
       Km[t-1,6] <- -Km[t-1,2]-Km[t-1,10]

       Km[t-1,3] <- -beta*pi[t-1]*(theta[t-1,1]+0.5*Km[t-1,2])*(theta[t-1,2]+0.5*Km[t-1,6])+omega[t-1]*gamma_s*(theta[t-1,3]+0.5*Km[t-1,10])
       Km[t-1,11] <- gamma*(theta[t-1,2]+0.5*Km[t-1,6])-omega[t-1]*gamma_s*(theta[t-1,3]+0.5*Km[t-1,10])
       Km[t-1,7] <- -Km[t-1,3]-Km[t-1,11]

       Km[t-1,4] <- -beta*pi[t-1]*(theta[t-1,1]+Km[t-1,3])*(theta[t-1,2]+Km[t-1,7])+omega[t-1]*gamma_s*(theta[t-1,3]+Km[t-1,11])
       Km[t-1,12] <- gamma*(theta[t-1,2]+Km[t-1,7])-omega[t-1]*gamma_s*(theta[t-1,3]+Km[t-1,11])
       Km[t-1,8] <- -Km[t-1,4]-Km[t-1,12]

       alpha[t-1,1] <- theta[t-1,1]+(Km[t-1,1]+2*Km[t-1,2]+2*Km[t-1,3]+Km[t-1,4])/6
       alpha[t-1,2] <- theta[t-1,2]+(Km[t-1,5]+2*Km[t-1,6]+2*Km[t-1,7]+Km[t-1,8])/6
       alpha[t-1,3] <- theta[t-1,3]+(Km[t-1,9]+2*Km[t-1,10]+2*Km[t-1,11]+Km[t-1,12])/6

       theta[t,1:3] ~ ddirch(k*alpha[t-1,1:3])
       Y[t-1] ~ dbeta(lambdaY*theta[t,2],lambdaY*(1-theta[t,2]))
       R[t-1] ~ dbeta(lambdaR*theta[t,3],lambdaR*(1-theta[t,3]))
     }
    theta0[1:3]<-c(",
      1 - Y[1] - R[1],
      ",",
      Y[1],
      ",",
      R[1],
      ")
    theta[1,1:3] ~ ddirch(theta0[1:3])
    gamma ~  dlnorm(",
      lognorm_gamma_parm$mu,
      ",",
      1 / lognorm_gamma_parm$var,
      ")
    gamma_s ~  dlnorm(",
      lognorm_gamma_s_parm$mu,
      ",",
      1 / lognorm_gamma_s_parm$var,
      ")
    R0 ~ dlnorm(",
      lognorm_R0_parm$mu,
      ",",
      1 / lognorm_R0_parm$var,
      ")
    beta <- R0*gamma
    k ~  dgamma(2,0.0001)
    lambdaY ~ dgamma(2,0.0001)
    lambdaR ~ dgamma(2,0.0001)
  }
"
    )
    
    model.spec <- textConnection(model1.string)
    
    posterior <- jags.model(
      model.spec,
      data = list(
        'Y' = Y,
        'R' = R,
        'T_prime' = T_prime,
        'pi' = pi,
        'omega' = omega
      ),
      n.chains = nchain,
      n.adapt = nadapt
    )
    
    update(posterior, nburnin) #burn-in
    
    jags_sample <- jags.samples(
      posterior,
      c(
        'theta',
        'gamma',
        'gamma_s',
        'R0',
        'beta',
        'Y',
        'lambdaY',
        'lambdaR',
        'k'
      ),
      n.iter = M,
      thin = thn
    )
    
    {
      if (dic)
        dic_val <- dic.samples(posterior, n.iter = M, thin = thn)
      else
        dic_val <- NULL
    }
    
    if (save_files) {
      png(paste0(file_add, casename, "theta_p.png"),
          width = 700,
          height = 900)
      plot(as.mcmc.list(jags_sample$theta)[, (1:3) * (T_prime + 1)])
      # posterior true probabilities
      dev.off()
      
      png(paste0(file_add, casename, "R0_p.png"),
          width = 700,
          height = 350)
      R0_p <- unlist(as.mcmc.list(jags_sample$R0))
      plot(as.mcmc.list(jags_sample$R0))
      dev.off()
      
      png(paste0(file_add, casename, "gamma_p.png"),
          width = 700,
          height = 350)
      gamma_p <- unlist(as.mcmc.list(jags_sample$gamma))
      plot(as.mcmc.list(jags_sample$gamma))
      dev.off()
      
      png(
        paste0(file_add, casename, "gamma_s_p.png"),
        width = 700,
        height = 350
      )
      gamma_s_p <- unlist(as.mcmc.list(jags_sample$gamma_s))
      plot(as.mcmc.list(jags_sample$gamma_s))
      dev.off()
      
      png(paste0(file_add, casename, "beta_p.png"),
          width = 700,
          height = 350)
      beta_p <- unlist(as.mcmc.list(jags_sample$beta))
      plot(as.mcmc.list(jags_sample$beta))
      dev.off()
      
      png(
        paste0(file_add, casename, "lambdaY_p.png"),
        width = 700,
        height = 350
      )
      lambdaY_p <- unlist(as.mcmc.list(jags_sample$lambdaY))
      plot(as.mcmc.list(jags_sample$lambdaY))
      dev.off()
      
      png(
        paste0(file_add, casename, "lambdaR_p.png"),
        width = 700,
        height = 350
      )
      lambdaR_p <- unlist(as.mcmc.list(jags_sample$lambdaR))
      plot(as.mcmc.list(jags_sample$lambdaR))
      dev.off()
      
      png(paste0(file_add, casename, "k_p.png"),
          width = 700,
          height = 350)
      k_p <- unlist(as.mcmc.list(jags_sample$k))
      plot(as.mcmc.list(jags_sample$k))
      dev.off()
      
    } else{
      R0_p <- unlist(as.mcmc.list(jags_sample$R0))
      gamma_p <- unlist(as.mcmc.list(jags_sample$gamma))
      gamma_s_p <- unlist(as.mcmc.list(jags_sample$gamma_s))
      beta_p <- unlist(as.mcmc.list(jags_sample$beta))
      lambdaY_p <- unlist(as.mcmc.list(jags_sample$lambdaY))
      lambdaR_p <- unlist(as.mcmc.list(jags_sample$lambdaR))
      k_p <- unlist(as.mcmc.list(jags_sample$k))
    }
    
    
    theta_p <-
      array(Reduce(rbind, as.mcmc.list(jags_sample$theta)), dim = c(len, T_prime +
                                                                      1, 3))
    theta_p_mean <- apply(theta_p[, T_prime + 1, ], 2, mean)
    theta_p_ci <-
      as.vector(apply(theta_p[, T_prime + 1, ], 2, quantile, c(0.025, 0.5, 0.975)))
    
    R0_p_mean <- mean(R0_p)
    R0_p_ci <- quantile(R0_p, c(0.025, 0.5, 0.975))
    
    gamma_p_mean <- mean(gamma_p)
    gamma_p_ci <- quantile(gamma_p, c(0.025, 0.5, 0.975))
    
    gamma_s_p_mean <- mean(gamma_s_p)
    gamma_s_p_ci <- quantile(gamma_s_p, c(0.025, 0.5, 0.975))
    
    beta_p_mean <- mean(beta_p)
    beta_p_ci <- quantile(beta_p, c(0.025, 0.5, 0.975))
    
    lambdaY_p_mean <- mean(lambdaY_p)
    lambdaY_p_ci <- quantile(lambdaY_p, c(0.025, 0.5, 0.975))
    
    lambdaR_p_mean <- mean(lambdaR_p)
    lambdaR_p_ci <- quantile(lambdaR_p, c(0.025, 0.5, 0.975))
    
    k_p_mean <- mean(k_p)
    k_p_ci <- quantile(k_p, c(0.025, 0.5, 0.975))
    
    #### Forecast ####
    theta_pp <- array(0, dim = c(len, T_fin - T_prime, 3))
    Y_pp <- matrix(NA, nrow = len, ncol = T_fin - T_prime)
    R_pp <- matrix(NA, nrow = len, ncol = T_fin - T_prime)
    for (l in 1:len) {
      thetalt1 <- theta_p[l, T_prime + 1, 1]
      thetalt2 <- theta_p[l, T_prime + 1, 2]
      thetalt3 <- theta_p[l, T_prime + 1, 3]
      betal <- c(beta_p)[l]
      gammal <- c(gamma_p)[l]
      gamma_sl <- c(gamma_s_p)[l]
      kt <- c(k_p)[l]
      lambdaYl <- c(lambdaY_p)[l]
      lambdaRl <- c(lambdaR_p)[l]
      if (betal < 0 |
          gammal < 0 |
          gamma_sl < 0 | thetalt1 < 0 | thetalt2 < 0 | thetalt3 < 0)
        next
      for (t in 1:(T_fin - T_prime)) {
        Km <- NULL
        alpha_pp <- NULL
        Km[1] <-
          -betal * pi[t + T_prime] * thetalt1 * thetalt2 + omega[t + T_prime] * gamma_sl * thetalt3
        Km[9] <-
          gammal * thetalt2 - omega[t + T_prime] * gamma_sl * thetalt3
        Km[5] <- -Km[1] - Km[9]
        
        Km[2] <-
          -betal * pi[t + T_prime] * (thetalt1 + 0.5 * Km[1]) * (thetalt2 + 0.5 *
                                                                   Km[5]) + omega[t + T_prime] * gamma_sl * (thetalt3 + 0.5 * Km[9])
        Km[10] <-
          gammal * (thetalt2 + 0.5 * Km[5]) - omega[t + T_prime] * gamma_sl * (thetalt3 + 0.5 * Km[9])
        Km[6] <- -Km[2] - Km[10]
        
        Km[3] <-
          -betal * pi[t + T_prime] * (thetalt1 + 0.5 * Km[2]) * (thetalt2 + 0.5 *
                                                                   Km[6]) + omega[t + T_prime] * gamma_sl * (thetalt3 + 0.5 * Km[10])
        Km[11] <-
          gammal * (thetalt2 + 0.5 * Km[6]) - omega[t + T_prime] * gamma_sl * (thetalt3 + 0.5 * Km[10])
        Km[7] <- -Km[3] - Km[11]
        
        Km[4] <-
          -betal * pi[t + T_prime] * (thetalt1 + Km[3]) * (thetalt2 + Km[7]) + omega[t + T_prime] * gamma_sl * (thetalt3 + Km[11])
        Km[12] <-
          gammal * (thetalt2 + Km[7]) - omega[t + T_prime] * gamma_sl * (thetalt3 + Km[11])
        Km[8] <- -Km[4] - Km[12]
        
        alpha_pp[1] <-
          thetalt1 + (Km[1] + 2 * Km[2] + 2 * Km[3] + Km[4]) /
          6
        alpha_pp[2] <-
          thetalt2 + (Km[5] + 2 * Km[6] + 2 * Km[7] + Km[8]) /
          6
        alpha_pp[3] <-
          thetalt3 + (Km[9] + 2 * Km[10] + 2 * Km[11] + Km[12]) /
          6
        
        thetalt_tmp <- rdirichlet(1, kt * c(alpha_pp))
        thetalt1 <- theta_pp[l, t, 1] <- thetalt_tmp[1]
        thetalt2 <- theta_pp[l, t, 2] <- thetalt_tmp[2]
        thetalt3 <- theta_pp[l, t, 3] <- thetalt_tmp[3]
        
        Y_pp[l, t] <-
          rbeta(1, lambdaYl * thetalt2, lambdaYl * (1 - thetalt2))
        R_pp[l, t] <-
          rbeta(1, lambdaRl * thetalt3, lambdaRl * (1 - thetalt3))
      }
    }
    
    par(mfrow = c(1, 1))
    col2 = gg_color_hue(2)
    Y_band <-
      data.frame(t(apply(
        Y_pp,
        2,
        quantile,
        probs = c(0.025, 0.975),
        na.rm = T
      )))
    thetaI_band <-
      data.frame(t(apply(
        theta_p[, -1, 2],
        2,
        quantile,
        probs = c(0.025, 0.975),
        na.rm = T
      )))
    Y_mean <- c(colMeans(Y_pp, na.rm = T))
    thetaI_mean <-
      c(colMeans(theta_p[, -1, 2], na.rm = T), colMeans(theta_pp[, , 2], na.rm = T))
    thetaI_median <-
      c(apply(theta_p[, -1, 2], 2, median, na.rm = T),
        apply(theta_pp[, , 2], 2, median, na.rm = T))
    colnames(Y_band) <- c("lower", "upper")
    colnames(thetaI_band) <- c("lower", "upper")
    data_pre <- data.frame(time = 1:T_prime, Y)
    data_post <- data.frame(time = 1:T_prime, thetaI_band)
    data_fore <-
      data.frame(time = (T_prime + 1):T_fin, Y_band, Y_mean)
    
    data_comp <-
      data.frame(
        time = 1:T_fin,
        rbind(thetaI_band , Y_band),
        phase = c(rep('pre', nrow(thetaI_band)), rep('post', nrow(Y_band))),
        mean = thetaI_mean,
        median = thetaI_median
      )
    
    data_poly <-
      data.frame(
        y = c(
          thetaI_band$upper,
          rev(thetaI_band$lower),
          Y_band$upper,
          rev(Y_band$lower)
        ),
        x = c(1:T_prime, T_prime:1, (T_prime + 1):T_fin, T_fin:(T_prime + 1)),
        phase = c(rep('pre', T_prime * 2), rep('post', (T_fin - T_prime) * 2)),
        value = c(rep(col2[1], T_prime * 2), rep(col2[2], (T_fin - T_prime) * 2))
      )
    
    ## First-order derivative check
    thetaS_mat <- cbind(theta_p[, -1, 1], theta_pp[, , 1])
    thetaI_mat <- cbind(theta_p[, -1, 2], theta_pp[, , 2])
    thetaR_mat <- cbind(theta_p[, -1, 3], theta_pp[, , 3])
    #dthetaI_mat <- (thetaS_mat*thetaI_mat)*((c(beta_p))%o%(pi))-thetaI_mat*replicate(T_fin,c(gamma_p))
    #dthetaI_mat <- apply(thetaI_mat,1,diff) # this is to circumvent the difficulty of obtaining the differential equation among posterior theta's
    dthetaI_mat_post <-
      (theta_pp[, , 1] * theta_pp[, , 2]) * ((c(beta_p)) %o% (pi[(T_prime + 1):T_fin])) -
      theta_pp[, , 2] * replicate(T_fin - T_prime, c(gamma_p))
    dthetaI_mat_pre <-
      t(apply(theta_p[, , 2], 1, function(v) {
        diff(smooth(v))
      }))
    dthetaI_mat <- cbind(dthetaI_mat_pre, dthetaI_mat_post)
    
    dthetaI <- colMeans(dthetaI_mat, na.rm = T)
    dthetaI_tp1 <-
      (1:T_fin)[which.max(dthetaI)]# first second order derivative=0
    dthetaI_tp2 <-
      (dthetaI_tp1:T_fin)[which.min(dthetaI[dthetaI_tp1:T_fin] > 0)] # first order derivative=0
    dthetaI_tp1_rd <- max(round(dthetaI_tp1), 1)
    if (dthetaI_tp1_rd > T_prime) {
      thetatI_tp1_vec <- thetaI_mat[, dthetaI_tp1_rd]
      thetaI_tp1_mean <- mean(thetatI_tp1_vec, na.rm = T)
      thetaI_tp1_ci <-
        quantile(thetatI_tp1_vec, c(0.025, 0.5, 0.975), na.rm = T)
      Y_tp1_vec <- Y_pp[, dthetaI_tp1_rd - T_prime]
      Y_tp1_mean <- mean(Y_tp1_vec, na.rm = T)
      Y_tp1_ci <-
        quantile(Y_tp1_vec, c(0.025, 0.5, 0.975), na.rm = T)
      
      thetatR_tp1_vec <- thetaR_mat[, dthetaI_tp1_rd]
      thetaR_tp1_mean <- mean(thetatR_tp1_vec, na.rm = T)
      thetaR_tp1_ci <-
        quantile(thetatR_tp1_vec, c(0.025, 0.5, 0.975), na.rm = T)
      R_tp1_vec <- R_pp[, dthetaI_tp1_rd - T_prime]
      R_tp1_mean <- mean(R_tp1_vec, na.rm = T)
      R_tp1_ci <-
        quantile(R_tp1_vec, c(0.025, 0.5, 0.975), na.rm = T)
    } else{
      thetatI_tp1_vec <- thetaI_mat[, dthetaI_tp1_rd]
      thetaI_tp1_mean <- mean(thetatI_tp1_vec, na.rm = T)
      thetaI_tp1_ci <-
        quantile(thetatI_tp1_vec, c(0.025, 0.5, 0.975), na.rm = T)
      Y_tp1_vec <- NA
      Y_tp1_mean <- mean(Y_tp1_vec, na.rm = T)
      Y_tp1_ci <-
        quantile(Y_tp1_vec, c(0.025, 0.5, 0.975), na.rm = T)
      
      thetatR_tp1_vec <- thetaR_mat[, dthetaI_tp1_rd]
      thetaR_tp1_mean <- mean(thetatR_tp1_vec, na.rm = T)
      thetaR_tp1_ci <-
        quantile(thetatR_tp1_vec, c(0.025, 0.5, 0.975), na.rm = T)
      R_tp1_vec <- R_pp[, dthetaI_tp1_rd - T_prime]
      R_tp1_mean <- mean(R_tp1_vec, na.rm = T)
      R_tp1_ci <-
        quantile(R_tp1_vec, c(0.025, 0.5, 0.975), na.rm = T)
    }
    dthetaI_tp2_rd <- max(round(dthetaI_tp2), 1)
    if (dthetaI_tp1_rd == dthetaI_tp2_rd) {
      thetatI_tp2_vec <- NA
      thetaI_tp2_mean <- mean(thetatI_tp2_vec, na.rm = T)
      thetaI_tp2_ci <-
        quantile(thetatI_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
      Y_tp2_vec <- NA
      Y_tp2_mean <- mean(Y_tp2_vec, na.rm = T)
      Y_tp2_ci <-
        quantile(Y_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
      
      thetatR_tp2_vec <- NA
      thetaR_tp2_mean <- mean(thetatR_tp2_vec, na.rm = T)
      thetaR_tp2_ci <-
        quantile(thetatR_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
      R_tp2_vec <- NA
      R_tp2_mean <- mean(R_tp2_vec, na.rm = T)
      R_tp2_ci <-
        quantile(R_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
    } else if (dthetaI_tp2_rd > T_prime) {
      thetatI_tp2_vec <- thetaI_mat[, dthetaI_tp2_rd]
      thetaI_tp2_mean <- mean(thetatI_tp2_vec, na.rm = T)
      thetaI_tp2_ci <-
        quantile(thetatI_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
      Y_tp2_vec <- Y_pp[, dthetaI_tp2_rd - T_prime]
      Y_tp2_mean <- mean(Y_tp2_vec, na.rm = T)
      Y_tp2_ci <-
        quantile(Y_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
      
      thetatR_tp2_vec <- thetaR_mat[, dthetaI_tp2_rd]
      thetaR_tp2_mean <- mean(thetatR_tp2_vec, na.rm = T)
      thetaR_tp2_ci <-
        quantile(thetatR_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
      R_tp2_vec <- R_pp[, dthetaI_tp2_rd - T_prime]
      R_tp2_mean <- mean(R_tp2_vec, na.rm = T)
      R_tp2_ci <-
        quantile(R_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
    } else{
      thetatI_tp2_vec <- thetaI_mat[, dthetaI_tp2_rd]
      thetaI_tp2_mean <- mean(thetatI_tp2_vec, na.rm = T)
      thetaI_tp2_ci <-
        quantile(thetatI_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
      Y_tp2_vec <- NA
      Y_tp2_mean <- mean(Y_tp2_vec, na.rm = T)
      Y_tp2_ci <-
        quantile(Y_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
      
      thetatR_tp2_vec <- thetaR_mat[, dthetaI_tp2_rd]
      thetaR_tp2_mean <- mean(thetatR_tp2_vec, na.rm = T)
      thetaR_tp2_ci <-
        quantile(thetatR_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
      R_tp2_vec <- NA
      R_tp2_mean <- mean(R_tp2_vec, na.rm = T)
      R_tp2_ci <-
        quantile(R_tp2_vec, c(0.025, 0.5, 0.975), na.rm = T)
    }
    thetaR_max_vec <- thetaR_mat[, T_fin]
    thetaR_max_mean <- mean(thetaR_max_vec)
    thetaR_max_ci <-
      quantile(thetaR_max_vec, c(0.025, 0.5, 0.975), na.rm = T)
    
    cumInf_vec <- thetaR_mat[, T_fin] + thetaI_mat[, T_fin]
    cumInf_mean <- mean(cumInf_vec)
    cumInf_ci <-
      quantile(cumInf_vec, c(0.025, 0.5, 0.975), na.rm = T)
    
    
    #if(dthetaI_tp1<T_prime) {dthetaI_tp1=which.max(diff(colMeans(thetaI_mat)))
    #                        message("The turning point 1 was observed and obtained from prevalence! The CI of turning point 1 may not be valid!")}
    #if(dthetaI_tp2<T_prime) {dthetaI_tp2=which.min(diff(colMeans(thetaI_mat))>0)
    #                         message("The turning point 2 was observed and obtained from prevalence! The CI of turning point 2 may not be valid!")
    #}
    dthetaI_tp1_date <- chron_ls[dthetaI_tp1]
    dthetaI_tp2_date <- chron_ls[dthetaI_tp2]
    
    
    incidence_vec <- 1 - thetaS_mat[, T_fin]
    incidence_mean <-  mean(incidence_vec, na.rm = T)
    
    incidence_ci <-
      quantile(incidence_vec, c(0.025, 0.5, 0.975), na.rm = T)
    
    first_tp_vec <-
      (1:T_fin)[apply(dthetaI_mat, 1, which.max)]# first second order derivative=0
    
    second_tp_vec <- sapply(1:len, function(l) {
      (first_tp_vec[l]:T_fin)[which.min(dthetaI_mat[l, first_tp_vec[l]:T_fin] >
                                          0)]
    })
    
    end_p_vec <- sapply(1:len, function(l) {
      if (any(thetaI_mat[l, first_tp_vec[l]:T_fin] <= eps, na.rm = TRUE)) {
        (first_tp_vec[l]:T_fin)[which.max(thetaI_mat[l, first_tp_vec[l]:T_fin] <=
                                            eps)]
      } else{
        T_fin
      }
    })
    
    
    
    # first order derivative=0
    first_tp_mean <- mean(first_tp_vec, na.rm = T)
    second_tp_mean <- mean(second_tp_vec, na.rm = T)
    end_p_mean <- mean(end_p_vec, na.rm = T)
    
    first_tp_ci <-
      quantile(first_tp_vec, c(0.025, 0.5, 0.975), na.rm = T)
    second_tp_ci <-
      quantile(second_tp_vec, c(0.025, 0.5, 0.975), na.rm = T)
    end_p_ci <- quantile(end_p_vec, c(0.025, 0.5, 0.975), na.rm = T)
    
    first_tp_date_mean <- chron_ls[first_tp_mean]
    second_tp_date_mean <- chron_ls[second_tp_mean]
    end_p_date_mean <- chron_ls[end_p_mean]
    
    first_tp_date_ci <- chron_ls[first_tp_ci]
    second_tp_date_ci <- chron_ls[second_tp_ci]
    end_p_date_ci <- chron_ls[end_p_ci]
    
    names(first_tp_date_ci) <- c("2.5%", "50%", "97.5%")
    names(second_tp_date_ci) <- c("2.5%", "50%", "97.5%")
    names(end_p_date_ci) <- c("2.5%", "50%", "97.5%")
    
    if (save_files) {
      png(paste0(file_add, casename, "deriv.png"),
          width = 700,
          height = 350)
      plot(
        y = dthetaI,
        x = chron_ls,
        type = 'l',
        ylab = "1st order derivative",
        main = "Infection Prevalence"
      )
      abline(h = 0, col = 2)
      abline(v = chron_ls[T_prime], col = "blue")
      if (exponential == FALSE &
          !is.null(change_time) & !is.null(pi0)) {
        abline(v = change_time_chorn[-c(1, length(change_time_chorn))], col = "gray")
      }
      legend(
        "topright",
        legend = c("Last observation", "change point"),
        col = c("blue", "gray"),
        lty = 1,
        title = "",
        bty = "n"
      )
      dev.off()
    }
    
    ## Prepare the Spaghetti plot
    sample_dthetaI_mat <-
      cbind(dthetaI_mat[sample.int(len, 20, replace = F), ])
    colnames(sample_dthetaI_mat) <- c(as.character(chron_ls)[-1])
    sample_dthetaI_mat_long <- reshape2::melt(sample_dthetaI_mat)
    colnames(sample_dthetaI_mat_long) <- c("id", "date", "dthetaI")
    sample_dthetaI_mat_long$date <-
      (chron(as.character(sample_dthetaI_mat_long$date)))
    
    dthetaI_mean_data <- data.frame(dthetaI, date = chron_ls[-1])
    spaghetti_ht <- mean(range(sample_dthetaI_mat)) / 2
    spaghetti_plot <- ggplot() +
      theme_bw() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 16,
          face = "bold"
        ),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 14)
      ) +
      geom_line(data = sample_dthetaI_mat_long, aes(
        x = date,
        y = dthetaI,
        group = id,
        color = id
      )) +
      scale_color_gradientn(colours = rainbow(5, alpha = 0.5)) +
      labs(title = "spaghetti plot of infection prevalence", x = "time", y = "1st order derivative") +
      geom_line(data = dthetaI_mean_data, aes(x = date, y = dthetaI), color =
                  1) +
      scale_x_continuous(labels = as.character(chron_ls)[seq(1, T_fin, 30)],
                         breaks = as.numeric(chron_ls[-1][seq(1, T_fin, 30)])) +
      annotate(
        geom = "text",
        label = as.character(chron(chron_ls[T_prime]), format = "mon day"),
        x = as.numeric(chron_ls[T_prime]) + 12,
        y = spaghetti_ht,
        color = "blue"
      ) +
      annotate(
        geom = "text",
        label = as.character(chron(dthetaI_tp1_date, format = "mon day")),
        x = as.numeric(dthetaI_tp1_date) + 12,
        y = spaghetti_ht * 1.25,
        color = "darkgreen"
      ) +
      geom_vline(
        xintercept = as.numeric(chron_ls[T_prime]),
        color = "blue",
        show.legend = TRUE
      ) +
      geom_vline(
        xintercept = as.numeric(dthetaI_tp1_date),
        color = "darkgreen",
        show.legend = TRUE
      )
    #if(dthetaI_tp1>T_prime)
    spaghetti_plot <- spaghetti_plot +
      geom_rect(
        data = data.frame(
          xmin = as.numeric(first_tp_date_ci[1]),
          xmax = as.numeric(first_tp_date_ci[3]),
          ymin = -Inf,
          ymax = Inf,
          ci = "first tp"
        ),
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax
        ),
        fill = "darkgreen",
        alpha = 0.15
      )
    #if(dthetaI_tp2>T_prime)
    spaghetti_plot <- spaghetti_plot +
      geom_rect(
        data = data.frame(
          xmin = as.numeric(second_tp_date_ci[1]),
          xmax = as.numeric(second_tp_date_ci[3], ci = "second tp"),
          ymin = -Inf,
          ymax = Inf
        ),
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax
        ),
        fill = "purple",
        alpha = 0.15
      )
    if (dthetaI_tp2_date > dthetaI_tp1_date) {
      spaghetti_plot <-
        spaghetti_plot + geom_vline(
          xintercept = as.numeric(dthetaI_tp2_date),
          color = "purple",
          show.legend = TRUE
        ) +
        annotate(
          geom = "text",
          label = as.character(chron(dthetaI_tp2_date, format = "mon day")),
          x = as.numeric(dthetaI_tp2_date) + 12,
          y = spaghetti_ht * 1.5,
          color = "purple"
        )
    }
    
    if (save_files)
      ggsave(
        paste0(file_add, casename, "_spaghetti.png"),
        width = 12,
        height = 10
      )
    ###################
    y_text_ht <- max(rbind(thetaI_band , Y_band), na.rm = T) / 2
    plot1 <- ggplot(data = data_poly, aes(x = x, y = y)) +
      geom_polygon(alpha = 0.5, aes(fill = value, group = phase)) +
      labs(
        title = substitute(
          paste(
            casename,
            ": infection forecast with prior ",
            beta[0],
            "=",
            v1,
            ",",
            gamma[0],
            "=",
            v2,
            ",",
            gamma[s0],
            "=",
            v4,
            " and ",
            R[0],
            "=",
            v3
          ),
          list(
            casename = casename,
            v1 = format(beta0, digits = 3),
            v2 = format(gamma0, digits = 3),
            v4 = format(gamma_s0, digits = 3),
            v3 = format(R0, digits = 3)
          )
        ),
        subtitle = substitute(
          paste(
            "Posterior ",
            beta[p],
            "=",
            v1,
            ",",
            gamma[p],
            "=",
            v2,
            ",",
            gamma[sp],
            "=",
            v4,
            " and ",
            R[0],
            "=",
            v3
          ),
          list(
            v1 = format(beta_p_mean, digits = 3),
            v2 = format(gamma_p_mean, digits = 3),
            v4 = format(gamma_s_p_mean, digits = 3),
            v3 = format(R0_p_mean, digits = 3)
          )
        ),
        x = "time",
        y = "P(Infected)"
      ) +
      geom_line(data = data_comp, aes(x = time, y = median), color = "red") +
      geom_vline(xintercept = T_prime,
                 color = "blue",
                 show.legend = TRUE) +
      geom_vline(xintercept = dthetaI_tp1,
                 color = "darkgreen",
                 show.legend = TRUE) +
      geom_line(data = data_comp, aes(x = time, y = mean), color = "darkgray") +
      geom_point(data = data_pre, aes(x = time, y = Y)) + theme_bw() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 16,
          face = "bold"
        ),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)
      ) +
      scale_x_continuous(labels = as.character(chron_ls)[seq(1, T_fin, 30)],
                         breaks = seq(1, T_fin, 30)) +
      scale_fill_discrete(name = "Posterior",
                          labels = c(expression(paste(
                            y[t[0] + 1:T] ^ I, ' | ', y[1:t[0]] ^ I, ', ', y[1:t[0]] ^ R
                          )),
                          expression(paste(
                            theta[1:t[0]] ^ I, ' | ', y[1:t[0]] ^ I, ', ', y[1:t[0]] ^ R
                          )))) +
      annotate(
        geom = "text",
        label = as.character(chron(chron_ls[T_prime]), format = "mon day"),
        x = T_prime + 12,
        y = y_text_ht,
        color = "blue"
      ) +
      annotate(
        geom = "text",
        label = as.character(chron(dthetaI_tp1_date, format = "mon day")),
        x = dthetaI_tp1 + 12,
        y = y_text_ht * 1.25,
        color = "darkgreen"
      )
    if (dthetaI_tp2 > dthetaI_tp1) {
      plot1 <-
        plot1 + geom_vline(xintercept = dthetaI_tp2,
                           color = "purple",
                           show.legend = TRUE) + annotate(
                             geom = "text",
                             label = as.character(chron(dthetaI_tp2_date, format = "mon day")),
                             x = dthetaI_tp2 + 12,
                             y = y_text_ht * 1.5,
                             color = "purple"
                           )
    }
    # plot_list <- list(data_poly=data_poly,data_comp=data_comp,T_prime=T_prime,dthetaI_stationary2=dthetaI_stationary2,dthetaI_stationary1=dthetaI_stationary1,data_pre=data_pre,dthetaI_stationary2_date,dthetaI_stationary1_date,y_text_ht)
    
    if (save_files)
      ggsave(paste0(file_add, casename, "_forecast.png"),
             width = 12,
             height = 10)
    
    ### Removed
    R_band <-
      data.frame(t(apply(
        R_pp,
        2,
        quantile,
        probs = c(0.025, 0.975),
        na.rm = T
      )))
    thetaR_band <-
      data.frame(t(apply(
        theta_p[, -1, 3],
        2,
        quantile,
        probs = c(0.025, 0.975),
        na.rm = T
      )))
    R_mean <- c(colMeans(R_pp, na.rm = T))
    thetaR_mean <-
      c(colMeans(theta_p[, -1, 3], na.rm = T), colMeans(theta_pp[, , 3], na.rm = T))
    thetaR_med <-
      c(apply(theta_p[, -1, 3], 2, median, na.rm = T),
        apply(theta_pp[, , 3], 2, median, na.rm = T))
    colnames(R_band) <- c("lower", "upper")
    colnames(thetaR_band) <- c("lower", "upper")
    data_pre_R <- data.frame(time = 1:T_prime, R) # previous data
    data_post_R <-
      data.frame(time = 1:T_prime, thetaR_band) # posterior of theta^R
    data_fore_R <-
      data.frame(time = (T_prime + 1):T_fin, R_band, R_mean) # The forecast of R after T_prime
    
    data_comp_R <-
      data.frame(
        time = 1:T_fin,
        rbind(thetaR_band , R_band),
        phase = c(rep('pre', nrow(thetaR_band)), rep('post', nrow(R_band))),
        mean = thetaR_mean,
        median = thetaR_med,
        dead = thetaR_mean * death_in_R,
        dead_med = thetaR_med * death_in_R
      ) # the filled area--polygon
    
    data_poly_R <-
      data.frame(
        y = c(
          thetaR_band$upper,
          rev(thetaR_band$lower),
          R_band$upper,
          rev(R_band$lower)
        ),
        x = c(1:T_prime, T_prime:1, (T_prime + 1):T_fin, T_fin:(T_prime + 1)),
        phase = c(rep('pre', T_prime * 2), rep('post', (T_fin - T_prime) * 2)),
        value = c(rep(col2[1], T_prime * 2), rep(col2[2], (T_fin - T_prime) * 2))
      )
    
    r_text_ht <- max(rbind(thetaR_band , R_band), na.rm = T) / 2
    plot2 <- ggplot(data = data_poly_R, aes(x = x, y = y)) +
      geom_polygon(alpha = 0.5, aes(fill = value, group = phase)) +
      labs(
        title = substitute(
          paste(
            casename,
            ": removed forecast with prior ",
            beta[0],
            "=",
            v1,
            ",",
            gamma[0],
            "=",
            v2,
            ",",
            gamma[s0],
            "=",
            v4,
            " and ",
            R[0],
            "=",
            v3
          ),
          list(
            casename = casename,
            v1 = format(beta0, digits = 3),
            v2 = format(gamma0, digits = 3),
            v4 = format(gamma_s0, digits = 3),
            v3 = format(R0, digits = 3)
          )
        ),
        subtitle = substitute(
          paste(
            "posterior: ",
            beta[p],
            "=",
            v1,
            ",",
            gamma[p],
            "=",
            v2,
            ",",
            gamma[sp],
            "=",
            v4,
            " and ",
            R[0],
            "=",
            v3
          ),
          list(
            v1 = format(beta_p_mean, digits = 3),
            v2 = format(gamma_p_mean, digits = 3),
            v4 = format(gamma_s_p_mean, digits = 3),
            v3 = format(R0_p_mean, digits = 3)
          )
        ),
        x = "time",
        y = "P(Removed)"
      ) +
      geom_line(
        data = data_comp_R,
        aes(x = time, y = median),
        color = "red",
        linetype = 1
      ) +
      geom_vline(xintercept = T_prime, color = "blue") +
      geom_vline(xintercept = dthetaI_tp1, color = "darkgreen") +
      geom_line(data = data_comp_R, aes(x = time, y = mean), color = "darkgray") +
      geom_point(data = data_pre_R, aes(x = time, y = R)) + theme_bw() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 16,
          face = "bold"
        ),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)
      ) +
      scale_x_continuous(labels = as.character(chron_ls)[seq(1, T_fin, 30)],
                         breaks = seq(1, T_fin, 30)) +
      scale_fill_discrete(name = "Posterior",
                          labels = c(expression(paste(
                            y[t[0] + 1:T] ^ R, ' | ', y[1:t[0]] ^ I, ', ', y[1:t[0]] ^ R
                          )),
                          expression(paste(
                            theta[1:t[0]] ^ R, ' | ', y[1:t[0]] ^ I, ', ', y[1:t[0]] ^ R
                          )))) +
      annotate(
        geom = "text",
        label = as.character(chron(chron_ls[T_prime]), format = "mon day"),
        x = T_prime + 12,
        y = r_text_ht,
        color = "blue"
      ) + annotate(
        geom = "text",
        label = as.character(chron(dthetaI_tp1_date, format = "mon day")),
        x = dthetaI_tp1 + 12,
        y = r_text_ht * 1.25,
        color = "darkgreen"
      )
    
    if (dthetaI_tp2 > dthetaI_tp1) {
      plot2 <-
        plot2 + geom_vline(xintercept = dthetaI_tp2,
                           color = "purple",
                           show.legend = TRUE) + annotate(
                             geom = "text",
                             label = as.character(chron(dthetaI_tp2_date, format = "mon day")),
                             x = dthetaI_tp2 + 12,
                             y = r_text_ht * 1.5,
                             color = "purple"
                           )
    }
    if (add_death)
      plot2 <-
      plot2 + geom_line(
        data = data_comp_R,
        aes(x = time, y = dead),
        color = "black",
        linetype = 1
      ) + geom_line(
        data = data_comp_R,
        aes(x = time, y = dead_med),
        color = "black",
        linetype = 2
      )
    #plot2_list <- list(data_poly_R=data_poly_R,data_comp_R=data_comp_R,T_prime=T_prime,dthetaI_stationary2=dthetaI_stationary2,dthetaI_stationary1=dthetaI_stationary1,data_pre_R=data_pre_R,dthetaI_stationary2_date,dthetaI_stationary1_date)
    
    if (save_files)
      ggsave(
        paste0(file_add, casename, "_forecast2.png"),
        width = 12,
        height = 10
      )
    
    out_table1 <-
      data.frame(matrix(
        c(
          theta_p_mean,
          theta_p_ci,
          R0_p_mean,
          R0_p_ci,
          gamma_p_mean,
          gamma_p_ci,
          gamma_s_p_mean,
          gamma_s_p_ci,
          beta_p_mean,
          beta_p_ci,
          incidence_mean = incidence_mean,
          incidence_ci = incidence_ci,
          thetaI_tp1_mean = thetaI_tp1_mean,
          thetaI_tp1_ci = thetaI_tp1_ci,
          thetaR_tp1_mean = thetaR_tp1_mean,
          thetaR_tp1_ci = thetaR_tp1_ci,
          Y_tp1_mean = Y_tp1_mean,
          Y_tp1_ci = Y_tp1_ci,
          R_tp1_mean = R_tp1_mean,
          R_tp1_ci = R_tp1_ci,
          thetaI_tp2_mean = thetaI_tp2_mean,
          thetaI_tp2_ci = thetaI_tp2_ci,
          thetaR_tp2_mean = thetaR_tp2_mean,
          thetaR_tp2_ci = thetaR_tp2_ci,
          Y_tp2_mean = Y_tp2_mean,
          Y_tp2_ci = Y_tp2_ci,
          R_tp2_mean = R_tp2_mean,
          R_tp2_ci = R_tp2_ci,
          thetaR_max_mean,
          thetaR_max_ci,
          cumInf_mean = cumInf_mean,
          cumInf_ci = cumInf_ci
        ),
        nrow = 1
      ))
    
    out_table2 <-
      data.frame(matrix(
        c(
          dthetaI_tp1_date = as.character(dthetaI_tp1_date),
          first_tp_mean = as.character(first_tp_date_mean),
          first_tp_ci = as.character(first_tp_date_ci),
          dthetaI_tp2_date = as.character(dthetaI_tp2_date),
          second_tp_mean = as.character(second_tp_date_mean),
          second_tp_ci = as.character(second_tp_date_ci),
          end_p_date_mean = as.character(end_p_date_mean),
          end_p_date_ci = as.character(end_p_date_ci),
          begin_str = begin_str
        ),
        nrow = 1
      ))
    
    out_table <- cbind(out_table1, out_table2)
    #out_table<-matrix(c(theta_p_mean,theta_p_ci,R0_p_mean,R0_p_ci,gamma_p_mean,gamma_p_ci,beta_p_mean,beta_p_ci,k_p_mean,k_p_ci,lambdaY_p_mean,lambdaY_p_ci,lambdaR_p_mean,lambdaR_p_ci,as.character(first_order_change_date),as.character(second_order_change_date)),nrow=1)
    
    
    colnames(out_table) <-
      c(
        "thetaS_last_obs_p_mean",
        "thetaI_last_obs_p_mean",
        "thetaR_last_obs_p_mean",
        "thetaS_last_obs_p_ci_low",
        "thetaS_last_obs_p_ci_med",
        "thetaS_last_obs_p_ci_up",
        "thetaI_last_obs_p_ci_low",
        "thetaI_last_obs_p_ci_med",
        "thetaI_last_obs_p_ci_up",
        "thetaR_last_obs_p_ci_low",
        "thetaR_last_obs_p_ci_med",
        "thetaR_last_obs_p_ci_up",
        "R0_p_mean",
        "R0_p_ci_low",
        "R0_p_ci_med",
        "R0_p_ci_up",
        "gamma_p_mean",
        "gamma_p_ci_low",
        "gamma_p_ci_med",
        "gamma_p_ci_up",
        "gamma_s_p_mean",
        "gamma_s_p_ci_low",
        "gamma_s_p_ci_med",
        "gamma_s_p_ci_up",
        "beta_p_mean",
        "beta_p_ci_low",
        "beta_p_ci_med",
        "beta_p_ci_up",
        "incidence_mean",
        "incidence_ci_low",
        "incidence_ci_median",
        "incidence_ci_up",
        "thetaI_tp1_mean",
        "thetaI_tp1_ci_low",
        "thetaI_tp1_ci_med",
        "thetaI_tp1_ci_up",
        "thetaR_tp1_mean",
        "thetaR_tp1_ci_low",
        "thetaR_tp1_ci_med",
        "thetaR_tp1_ci_up",
        "Y_tp1_mean",
        "Y_tp1_ci_low",
        "Y_tp1_ci_med",
        "Y_tp1_ci_up",
        "R_tp1_mean",
        "R_tp1_ci_low",
        "R_tp1_ci_med",
        "R_tp1_ci_up",
        "thetaI_tp2_mean",
        "thetaI_tp2_ci_low",
        "thetaI_tp2_ci_med",
        "thetaI_tp2_ci_up",
        "thetaR_tp2_mean",
        "thetaR_tp2_ci_low",
        "thetaR_tp2_ci_med",
        "thetaR_tp2_ci_up",
        "Y_tp2_mean",
        "Y_tp2_ci_low",
        "Y_tp2_ci_med",
        "Y_tp2_ci_up",
        "R_tp2_mean",
        "R_tp2_ci_low",
        "R_tp2_ci_med",
        "R_tp2_ci_up",
        "thetaR_max_mean",
        "thetaR_max_ci_low",
        "thetaR_max_ci_med",
        "thetaR_max_ci_up",
        "cumInf_mean",
        "cumInf_ci_low",
        "cumInf_ci_med",
        "cumInf_ci_up",
        "dthetaI_tp1_date",
        "first_tp_mean",
        "first_tp_ci_low",
        "first_tp_ci_med",
        "first_tp_ci_up",
        "dthetaI_tp2_date",
        "second_tp_mean",
        "second_tp_ci_low",
        "second_tp_ci_med",
        "second_tp_ci_up",
        "end_p_mean",
        "end_p_ci_low",
        "end_p_ci_med",
        "end_p_ci_up",
        "begin_str"
      )
    
    #colnames(out_table)<-c("thetaS_p_mean","thetaI_p_mean","thetaR_p_mean","thetaS_p_ci_low","thetaS_p_ci_med","thetaS_p_ci_up","thetaI_p_ci_low","thetaI_p_ci_med","thetaI_p_ci_up","thetaR_p_ci_low","thetaR_p_ci_med","thetaR_p_ci_up","R0_p_mean","R0_p_ci_low","R0_p_ci_med","R0_p_ci_up","gamma_p_mean","gamma_p_ci_low","gamma_p_ci_med","gamma_p_ci_up","beta_p_mean","beta_p_ci_low","beta_p_ci_med","beta_p_ci_up","k_p_mean","k_p_ci_low","k_p_ci_med","k_p_ci_up","lambdaY_p_mean","lambdaY_p_ci_low","lambdaY_p_ci_med","lambdaY_p_ci_up","lambdaR_p_mean","lambdaR_p_ci_low","lambdaR_p_ci_med","lambdaR_p_ci_up","first_order_change_date","second_order_change_date")
    
    if (save_files)
      write.csv(out_table, file = paste0(file_add, casename, "_summary.csv"))
    if (save_mcmc) {
      save(theta_pp,
           Y_pp,
           R_pp,
           file = paste0(file_add, casename, "_forecast_MCMC.RData")) #@
      save(jags_sample,
           file = paste0(file_add, casename, "_rjags_MCMC.RData"))
    }
    
    if (save_plot_data) {
      other_plot <-
        list(
          T_prime = T_prime,
          T_fin = T_fin,
          chron_ls = chron_ls,
          dthetaI_tp1 = dthetaI_tp1,
          dthetaI_tp2 = dthetaI_tp2,
          dthetaI_tp1_date = dthetaI_tp1_date,
          dthetaI_tp2_date = dthetaI_tp2_date,
          beta_p_mean = beta_p_mean,
          gamma_p_mean = gamma_p_mean,
          gamma_s_p_mean = gamma_s_p_mean,
          R0_p_mean = R0_p_mean
        )
      spaghetti_plot_ls <-
        list(
          spaghetti_ht = spaghetti_ht,
          dthetaI_mean_data = dthetaI_mean_data,
          sample_dthetaI_mat_long = sample_dthetaI_mat_long,
          first_tp_date_ci = first_tp_date_ci,
          second_tp_date_ci = second_tp_date_ci
        )
      infection_plot_ls <-
        list(
          y_text_ht = y_text_ht,
          data_poly = data_poly,
          data_comp = data_comp,
          data_pre = data_pre
        )
      removed_plot_ls <-
        list(
          r_text_ht = r_text_ht,
          data_poly_R = data_poly_R,
          data_comp_R = data_comp_R,
          data_pre_R = data_pre_R
        )
      plot_data_ls <-
        list(
          casename = casename,
          other_plot = other_plot,
          spaghetti_plot_ls = spaghetti_plot_ls,
          infection_plot_ls = infection_plot_ls,
          removed_plot_ls = removed_plot_ls
        )
      save(plot_data_ls,
           file = paste0(file_add, casename, "_plot_data.RData"))
    }
    # Gelman-Rubin convergence diagnostics
    gelman_out_name <-
      c('R0',
        'gamma',
        'gamma_s',
        'beta',
        'theta',
        'lambdaY',
        'lambdaR',
        'k')
    gelman_diag_list <- lapply(gelman_out_name, function(name) {
      if (name == 'theta') {
        tryCatch(
          gelman.diag(as.mcmc.list(jags_sample[[name]])[, (1:3) * (T_prime + 1)],
                      autoburnin = FALSE),
          error = function(e)
            e
        )
      } else {
        tryCatch(
          gelman.diag(as.mcmc.list(jags_sample[[name]]),
                      autoburnin = FALSE),
          error = function(e)
            e
        )
      }
    })
    if (save_files) {
      sink(paste0(file_add, casename, "_Gelman_diag.txt"))
      print(gelman_diag_list)
      sink()
    }
    res <- list(
      casename = casename,
      incidence_mean = incidence_mean,
      incidence_ci = incidence_ci,
      out_table = out_table,
      plot_infection = plot1,
      plot_removed = plot2,
      spaghetti_plot = spaghetti_plot,
      first_tp_mean = as.character(first_tp_date_mean),
      first_tp_ci = as.character(first_tp_date_ci),
      second_tp_mean = as.character(second_tp_date_mean),
      second_tp_ci = as.character(second_tp_date_ci),
      dic_val = dic_val,
      gelman_diag_list = gelman_diag_list
    )
    return(res)
  }
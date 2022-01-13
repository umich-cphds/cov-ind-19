#' This function is used to estimate  the  different paramters of interest like the time varying transmission rates, proportion of reported cases and thebasic reproduction rates.
#'
#' @param N  (Mandatory) : The population size.
#' @param data (Mandatory) input :If the model is Multinomial then the data matrix should contain 3 columns namely Confirmed,Recovered and Death, while if the model is Poisson or Binomial, then the data should contain only one column namely Confirmed. Please see that the names of the colums are maintained.
#' @param data_init (Mandatory) This is the initial data values that should be provided by the user which is a six size length vector. The first entry should contain the Total Confirmed, Total Recovered ,Total Death , Daily Confirmed ,Daily Recovered and Daily Death for the Starting Date.If the starting total confirmed is 0 , please replace it by 1
#' @param init_pars = NULL( default) . Else the user can give a user input initial paramters which should consist of the initial values of the time varying beta , proportion of testing for the different periods.
#' @param niter = 1e5 (default)  No of iteration for the MCMC metropolis hasting algorithm.
#' @param BurnIn = 5e4 (default) This is the Burn-In Period for the MCMC algorithm
#' @param model = Multinomial(DEFAULT) This is the likelihood function that will be used. There are three options available namely Multinomial, Poisson and Binomial.
#' @param plot = TRUE(Default) This will give the box plot for the basic reproductuon number for the different periods.
#' @param auto.initialize = TRUE(default) This is the option for using a mle based initial parameter.
#' @param period_start The total time period is divided into small periods depending on the lockdowm measures imposed by the goverment. So this is a vector consisting of the start dates for the different time periods.
#' @param ... arguments passed to the function model_initializeR and model_plotR which is used for the initializing the parameters. The parameters are described below:
#'            \itemize{
#'                 \item{"step_pars"}{=init_pars/500 (default) . It is the variance of the proposal distribution for the Metropolis Hastings Algorithm which is assumed to be a Random Walk.
#'                 \item{"alpha_p"}{= 0.5 (default) It is defined as the ratio of rate of spread of infection by tested positive patients to that by false negatives. We have taken $\alpha_p < 1$ as patients who are tested positive are subjected to quarantine where the chance of spreading the disease is less than that of false negative patients who are mostly unaware of their infectious nature. So, false negative individuals continue to spread the disease freely at a higher rate than tested positive individuals. }
#'                 \item{"alpha_u"}{= 0.7 (default) It is defined as the  scaling factor for the rate of spread of infection by untested individuals. $\alpha_u$ is assumed to be < 1 as U mostly consists of asymptomatic or mildly symptomatic cases who are known to spread the disease at a much lower rate than those with higher levels of symptoms.}
#'                 \item{"beta_1 }{= 0.6 (default) . It is the scaling factor for rate of recovery  for untested individuals .  $\beta_1$ is  assumed to be less than 1.  The condition of Untested individuals is not so severe as they consists of mostly asymptomatic people. So, they are assumed to recover faster than the Current Positive Ones. }
#'                 \item{"beta_2"}{= 0.7 (default) . It is the inverse of the scaling factor for rate of recovery  for false negative individuals .  $\beta_2$ is assumed to be less than 1. It is assumed that the recovery rate is slower than the detected ones for the False Negative ones because they are not getting any hospital treatments. }
#'                 \item{"delta_1}{= 0.3 (default) It is the scaling factor for death rate for undetected individuals .  $\delta_1 $ is assumed to be less than 1. Similarly  for the Untested ones, the death rate is taken to be lesser because they are mostly asymptomatic. So, their probability of dying is much less.}
#'                 \item{"delta_2"}{= 0.7 (default). It is the inverse of  the scaling factor for death rate for false negative individuals . $\delta_2$ is assumed to be less than 1. Same as before, the death rate for False Negative ones are assumed to be higher than the Current detected Positive as they are not receiving proper treatment.}
#'                 \item{"lambda"}{=  1 / (66.26 * 365)(default) Natural birth  rate. The value given here as the default value is the world's common birth  rate.}
#'                 \item{"mu"}{=  1 / (66.26 * 365)(default) Natural  death rates. This is assumed to be equal with natural birth rate for the sake of simplicity.}
#'                 \item{"D_d"}{= 17.8 (default) Mean days till death for positive individuals.}
#'                 \item{"D_e"}{= 5.2 (default) Incubation period .}
#'                 \item{"Dr"}{= 17.8 (default) Mean days till recovery for positive individuals.}
#'                 \item{"f"}{=0.15 (default) False negative probability of RT-PCR test.}
#'                 \item{"mCFR"}{= NULL (It is calculated from the data  by default) It is defined as the ratio of the total reported deceased cases and the total removed cases  till that day.}
#'                 \item{"init.exposed.ratio "}{= 3 (default) This is the scaling factor for the calculation of the initial number of exposed people from the sum of the initial number of unreported, repoted people.}
#'                 \item{"init.confirmed.ratio"}{= 0.15 (default) This is the initial value of the probability of being tested.}
#'                 \item{"opt_num"}{= 100 (default) The number of times an user wants to run the mle optimization before deciding on the best initial parameter.}
#'                 \item{"trace_plot.common_axis"}{=FALSE(Default) This will give the trace plot for  the convergence of the mcmc estimated time varying parameters}
#'                 \item{"save plot"}{=TRUE (Default) It is the option for saving the plots in the directory folder.}
#'
#'                        }
#'
#'
#'
#' @return
#' @export
#'
#' @examples

model_estimateR <- function(data, data_init,N, init_pars = NULL, niter = 1e5, BurnIn = 5e3,
                            model = "Multinomial", plot = TRUE, period_start, auto.initialize = TRUE,
                            ... ){
  library(dplyr)

  if(model == "Multinomial"){
    if(! all(c("Confirmed", "Recovered", "Deceased" ) %in% colnames(data))){
      stop("Data does not contain required columns")
    }
  } else if (model == "Poisson" || model == "Binomial"){
    if(! all(c("Confirmed") %in% colnames(data))){
    stop("Data does not contain required columns")
    }
  } else
    stop("Incorrect model specified")

  if(length(data_init) != 6) stop("Incorrect initial values: length must be 6")

  if(any(data < 0) || data_init < 0) stop("Negative values in data")

  var_init = model_initializeR(data = data, data_init = data_init, init_pars = init_pars, model = model,
                               period_start = period_start, auto.initialize = auto.initialize, ...)

  fix_pars = var_init$fix_pars
  init_state_num = var_init$init_state_num
  init_pars = var_init$init_pars
  period_start = var_init$period_start

  pars_estimate = mcmc_performR(data = data, init_state_num =  init_state_num, init_pars = init_pars, model = model,
                                niter = niter, BurnIn = BurnIn, fix_pars = fix_pars, period_start = period_start, ...)

  ## Plot

  if(plot == TRUE){
    plots <- model_plotR(mcmc_pars = pars_estimate, n_period = length(period_start), data = data,
                         data_initial = data_initial, call = "estimateR", model = model, ...)
    return(list(mcmc_pars = pars_estimate, "plots" = plots))
  }

  return(list(mcmc_pars = pars_estimate))
}



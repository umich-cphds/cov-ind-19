generate_state_tab <- function(state, code)
{
  tabPanel(state,
    fluidRow(
      column(width = 1),
      column(width = 10,
      h2("Daily number of new COVID-19 cases, fatalities and recovered in", state),
      p(paste0("This figure provides the number of COVID-19 new cases (yellow),
         fatalities (red), and recovered cases (green) in ", state, ". You can
         hover your cursor over the bar to see the exact numerical counts.")
      ),
      plotlyOutput(paste0(code, "_x"), height = "600px"),
      hr(),
      h2("Impact of social distancing, travel ban, and 40 day lockdown in ", state),
      p("In the following Figures we consider various scenarios of intervention
        effects to assess the effect of the lockdown.", "These figures should
        not be overinterpreted as in reality we do not know how the lockdown
        will actually reduce the transmission probability in India and to what
        extent.", "We use the eSIR model (",a("Wang et al. 2020", .noWS =
        "outside", href = "https://www.medrxiv.org/content/10.1101/2020.02.29.20029421v1.full.pdf"),
        ") for all our projections and create hypothetical reductions in
        transmission probabilities capturing interventions like social
        distancing and lockdown. This in turn reduces the basic reproduction
        number over the period.", "It was announced that India would undergo
        a central lockdown from March 25 until May 3.", "The bar plots below
        the predicted cumulative short-term case counts represent three
        hypothetical scenarios: ",
        tags$ol(
          tags$li("No intervention"),
          tags$li("Social distancing and travel ban (without March 25 lockdown)"),
          tags$li("Lockdown until May 3 with a gradual, moderate resumption of daily activities")
        ),
        "Because we are using SIR models to generate the forecast, we
        explicitly delay changes in the basic reproduction number one week
        (Figure 4a) and two weeks (Figure 4b) to capture delayed onset of
        cases due to incubation period and timeliness of adherence.", "In
        general terms, the one-week delay models (Figures 4a, 5a, and 5b) can
        be thought of as 'quick adherence' to the lockdown measures, while
        the 'two-week' delay models (Figures 4b, 6a, and 6b) can be though of
        as 'slow adherence' to the lockdown measures.",
        HTML(paste0("All models assume a basic reproduction number of 2 under
          no intervention. The implied R", tags$sub("0"), " is 1.5 under Scenario 2.")),
        HTML(paste0("We further assume the R", tags$sub("0"), " drops to 0.8 under lockdown and then gradually rises back up to 1.5 after the lockdown ends over a three week period ('lockdown with moderate return').")),
          "You can hover of the bars for dates and counts. Also, please note the dotted line represents the upper confidence interval for the lockdown scenario (3), which is closest to the current intervention.",
          "Our codes are available on ", a("GitHub", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19") ," and so users can change the nature of interventions."),
        h2("Quick adherence (one-week delay)"),
        h3("Figure 4a (please note that the y-axis is in log base-10 scale; hover over the bars for count estimates and upper credible limits)"),
        plotlyOutput(paste0(code, "_p4"), height = "600px"),
     ),
     column(width = 1)
    )
  )
}

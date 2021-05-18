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
      h2("Short-term forecast for ", state),
    p("In the following Figures we consider various scenarios of intervention
      effects to assess the effect of the lockdown.", "These figures should
      not be overinterpreted as in reality we do not know how the lockdown
      will actually reduce the transmission probability in India and to what
      extent.", "We use the eSIR model (", a("Wang et al. 2020", .noWS =
      "outside", href = "https://www.medrxiv.org/content/10.1101/2020.02.29.20029421v1.full.pdf"),
      ") for all our projections and can create hypothetical reductions in
      transmission probabilities capturing interventions like social
      distancing and lockdown. This in turn reduces the basic reproduction
      number over the period.", "The bar plot below shows 
      the predicted cumulative short-term case counts under the assumption that there is no intervention.
      All models assume a basic reproduction number of 2 under no intervention."),
      HTML(paste0("You can hover of the bars for dates and counts. Also, please note the dotted line represents the upper confidence interval.",
        "Our codes are available on ", a("GitHub", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19") ," and so users can change the nature of interventions.")
    ),
        h3("Figure 4 (please note that the y-axis is in log base-10 scale; hover over the bars for count estimates and upper credible limits)"),
        plotlyOutput(paste0(code, "_p4"), height = "600px"),
        h3("SEIR"),
        plotlyOutput(paste0(code, "_pSEIR"), height = "600px"),
        hr(),
        h3('Time-varying R'),
        h4("Effective basic reproduction number"),
        plotlyOutput(paste0(code, "_ptvr"), height = "600px"),
      hr(),
    h2("Temporal testing pattern"),
    plotlyOutput(paste0(code, "_p15"), height = "600px"),
    hr()
      #h3("Doubling time"),
      #h4("Each point represents the estimated time for cases to double based on relative growth observed in the trailing 7 days."),
      #plotlyOutput(paste0(code, "_pdbl"), height = '600px'),
      #hr()
     ),
     column(width = 1)
    )
  )
}

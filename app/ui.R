library(shiny)
library(plotly)
library(tidyverse)

top_matter <- wellPanel(fluidRow(
  h3("COV-IND-19 Study Group"),
  fluidRow(
                    column(width = 2, shiny::img(src = "group_logo.png", height = 200, width = 200)),
                    column(width = 10,
                    p("Welcome to the COV-IND-19 shiny app. We aim to provide a resource to describe the COVID-19 outbreak in India to date as well as prediction models under various hypothetical scenarios.
      The figure and forecasting models update as new data becomes available (i.e., at least daily). You may download PNG files of each figure by clicking on the camera icon when you are hovering within each plot.
        Please cite our medium article and this website in any publication that you use this resource for."),
                           p("The COV-IND-19 study group is comprised of: Maxwell Salvatore, Alexander Rix, Michael Kleinsasser, Daniel Barker, Lili Wang, Rupam Bhattacharyya, Soumik Purkayastha, Debashree Ray,
            Shariq Mohammed, Aritra Halder, Debraj Bose, Peter Song, Mousumi Banerjee, Veera Baladandayuthapani, and Parikshit Ghosh. Led by PI ", a("Bhramar Mukherjee", .noWS = "outside", href = "http://www-personal.umich.edu/~bhramar/"), "."),
                           p("Please direct inquiries to ",
                             a("Maxwell Salvatore", .noWS = "outside", href="mailto:mmsalva@umich.edu"),", ",
                             a("Alexander Rix", .noWS = "outside", href="mailto:alexrix@umich.edu"),", ",
                             a("Michael Kleinsasser", .noWS = "outside", href="mailto:mkleinsa@umich.edu"),", and ",
                             a("Bhramar Mukherjee", .noWS = "outside", href="mailto:bhramar@umich.edu")),
                    column(5,
                           h4("References"),
                           p("Read the report: ", a("COV-IND-19 Report", .noWS = "outside", href = "https://bit.ly/COV-IND-19_Report"), " (this is a direct download link, check your downloads folder)"),
                           p("Read the original March 21 article: ", a("Medium article", .noWS = "outside", href = "https://medium.com/@covind_19/predictions-and-role-of-interventions-for-covid-19-outbreak-in-india-52903e2544e6")),
                           p("Read the accompanying April 3 article: ", a("Medium article", .noWS = "outside",  href = "https://medium.com/@covind_19/historic-lockdown-prediction-models-to-study-lockdown-effects-and-the-role-of-data-in-the-crisis-a0afeeec5a6")),
                           p("Source code: ", a("COV-IND-19 GitHub", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19"))
                    ),
                    column(5,
                           h4("Sources"),
                           p("Non-India country-level data source: ", a("JHU CSSE COVID-19 GitHub", .noWS = "outside", href = "https://github.com/CSSEGISandData/COVID-19")),
                           p("India data source: ", a("covid19india.org", .noWS = "outside", href = "https://www.covid19india.org")),
                           p("Map data source: ", a("COVID-19 in India Kaggle", .noWS = "outside", href = "https://www.kaggle.com/sudalairajkumar/covid19-in-india")),
                           p("R modeling package: ", a("eSIR R package", .noWS = "outside", href = "https://github.com/lilywang1988/eSIR"))
                    ),
                    )
                  )
  )
  )


shinyUI(
  navbarPage("COVID-19 Outbreak in India",
             tabPanel("National Observed",
                      fluidRow(
                        top_matter
                        ),
                      fluidRow(
                        h3("Please be patient as we are in the process of updating data sources and forecasts. This message will be removed when update is complete."),
                        h4("(Please wait a few seconds for the figures to load)"),
                        h4(textOutput("latest")),
                        h2(""),
                        h2("Daily number of new COVID-19 cases, fatalities and recovered in India"),
                        p("This figure provides the number of COVID-19 new cases (yellow), fatalities (red), and recovered cases (green) in India.
          You can hover your cursor over the bar to see the exact numerical counts."),
                      plotlyOutput("India_p1", height = "600px"),
                      hr(),
                      h2("Total number of COVID-19 cases and deaths"),
                      p("The first figure represents COVID-19 case counts where the x-axis starts on the day when each country passed 100 cases. 
The second figure represents COVID-19 fatalities where the x-axis starts on the day when each country exceeded 3 fatalities. 
These axes allow comparison of counts at similar stages of the outbreak. 
You can click on countries in the legend to add or remove them and you cann hover your cursor over the lines to see the exact numerical counts."),
                      plotlyOutput("India_p2", height = "800px"),
                      hr(),
                      h2("Doubling graphs"),
                      p("The following figures visualize the case and death data to depict how long it takes each country to double its case or death count."),
                      h3("Cases"),
                      plotlyOutput("India_p3a", height = "600px"),
                      h3("Deaths"),
                      plotlyOutput("India_p3b", height = "600px"),
                      hr(),
                      h2("Cumulative case counts by state/union territory"),
                      p("The map displays the case counts by state/union territory in India over the last few days.",
                        "The darker areas of the map indicate a greater number of cases."),
                      fluidRow(
                        column(width = 3),
                        column(width = 9, imageOutput("map", height = "650px")),
                      ),
                      hr(),
                      h2("Cumulative COVID-19 case count by state/union territory"),
                      plotOutput("India_p7b", height = "600px"),
                      h2("Cumulative COVID-19 death count by state/union territory"),
                      plotOutput("India_p7d", height = "600px"),
                      hr(),
                      h2("We are in the process of updating our forecasting models. Please check back soon. Thank you for your patience.")
                      )
                      ),
             
             tabPanel("National Forecast",
                      fluidRow(
                        
                        h2("Short-term impact of social distancing, travel ban, and lockdown"),
                                            p("In the following Figures we consider various scenarios of intervention effects to assess the effect of the lockdown.",
                                              "These figures should not be overinterpreted as in reality we do not know how the lockdown will actually reduce the transmission probability in India and to what extent.",
                                              "We use the eSIR model (",a("Wang et al. 2020", .noWS = "outside", href = "https://www.medrxiv.org/content/10.1101/2020.02.29.20029421v1.full.pdf"), ") for all our projections and create hypothetical reductions in transmission probabilities capturing interventions like social distancing and lockdown.
                        This in turn reduces the basic reproduction number over the period.",
                                              "It was announced that India would undergo a central lockdown from March 25 until April 14.",
                                              "The bar plots below the predicted cumulative short-term case counts represent three hypothetical scenarios: ",
                                              tags$ol(
                                                tags$li("No intervention"),
                                                tags$li("Social distancing and travel ban (without March 25 lockdown)"),
                                                tags$li("Lockdown until April 14 with a gradual, moderate resumption of daily activities")
                                              ),
                                              "Because we are using SIR models to generate the forecast, we explicitly delay changes in the basic reproduction number one week (Figure 4a) and two weeks (Figure 4b) to capture delayed onset of cases due to incubation period and timeliness of adherence.",
                                              "In general terms, the one-week delay models (Figures 4a, 5a, and 5b) can be thought of as 'quick adherence' to the lockdown measures, while the 'two-week' delay models (Figures 4b, 6a, and 6b) can be though of as 'slow adherence' to the lockdown measures.",
                                              HTML(paste0("All models assume a basic reproduction number of 2 under no intervention. The implied R", tags$sub("0"), " is 1.5 under Scenario 2.")),
                                              HTML(paste0("We further assume the R", tags$sub("0"), " drops to 0.8 under lockdown and then gradually rises back up to 1.5 after the lockdown ends over a three week period ('lockdown with moderate return').")),
                                              "You can hover of the bars for dates and counts. Also, please note the dotted line represents the upper confidence interval for the lockdown scenario (3), which is closest to the current intervention.",
                                              "Our codes are available on ", a("GitHub", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19") ," and so users can change the nature of interventions."),
                                            h2("Quick adherence (one-week delay)"),
                                            h3("Figure 4a (please note that the y-axis is in log base-10 scale; hover over the bars for count estimates and upper credible limits)"),
                                            plotlyOutput("India_p4a", height = "600px"),
                                            h2("Slow adherence (two-week delay)"),
                                            h3("Figure 4b (please note that the y-axis is in log base-10 scale; hover over the bars for count estimates and upper credible limits)"),
                                            plotlyOutput("India_p4b", height = "600px"),
                                            hr(),
                                            h2("Longer term forecasts post-lockdown"),
                                            p("We present four hypothetical scenarios:",
                                              tags$ul(
                                                tags$li(HTML(paste0("Perpetual social distancing and travel ban (no lockdown; represented in yellow): R", tags$sub("0"), " remains 1.5 over the entire interval."))),
                                                tags$li(HTML(paste0("Post-lockdown activities return to normal activities prior to any intervention ('normal (pre-intervention)'; light blue): R", tags$sub("0"), " returns to 2 three weeks after the lockdown ends."))),
                                                tags$li(HTML(paste0("Post-lockdown activities gradually return to a moderate level ('moderate return'; blue): R", tags$sub("0"), " returns to 1.5 three weeks after the lockdown ends."))),
                                                tags$li(HTML(paste0("Post-lockdown activities return to a subdued level ('cautious return'; dark blue): R", tags$sub("0"), " returns to 1.2 three weeks after the lockdown ends.")))
                                              ),
                                              HTML(paste0("As in Figures 4a and 4b, Figures 5a and 5b represent an explicit one- and two-week delay in changes to R", tags$sub("0"), ", respectively."))
                                            ),
                                            h2("Quick adherence (one-week delay)"),
                                            h3("Figure 5a"),
                                            plotlyOutput("India_p5a", height = "600px"),
                                            h3("Figure 5b"),
                                            plotlyOutput("India_p5b", height = "600px"),
                                            hr(),
                                            h2("Slow adherence (two-week delay)"),
                                            h3("Figure 6a"),
                                            plotlyOutput("India_p6a", height = "600px"),
                                            h3("Figure 6b"),
                                            plotlyOutput("India_p6b", height = "600px"),
                                            hr()
                        
                        )
                      )
             
  )


)

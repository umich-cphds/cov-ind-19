#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(reshape2)
library(tmap)
library(sf)
library(leaflet)
library(magick)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("COVID-19 Outbreak in India"),

  # # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("COV-IND-19 Study Group"),
      # HTML('<center><img src="group_logo.png" width="200"></center>'),
      shiny::img(src = "group_logo.png", height = 200, width = 200),
      br(),
      br(),
      br(),
      p("Welcome to the COV-IND-19 shiny app. We aim to provide a resource to describe the COVID-19 outbreak in India to date as well as prediction models under various hypothetical scenarios.
      The figure and forecasting models update as new data becomes available (i.e., at least daily). You may download figures for reference.
        Please cite our medium article and this website in any publication that you use this resource for."),
      p("Read the original article: ", a("Medium article", .noWS = "outside", href = "https://medium.com/@covind_19/predictions-and-role-of-interventions-for-covid-19-outbreak-in-india-52903e2544e6")),
      p("Read the report: ", a("COV-IND-19 Report", .noWS = "outside", href = "https://bit.ly/COV-IND-19_Report"), " (this is a direct download link, check your downloads folder)"),
      p("Country-level data source: ", a("JHU CSSE COVID-19 GitHub", .noWS = "outside", href = "https://github.com/CSSEGISandData/COVID-19")),
      p("State-level data source: ", a("COVID-19 in India Kaggle", .noWS = "outside", href = "https://www.kaggle.com/sudalairajkumar/covid19-in-india")),
      p("R modeling package: ", a("eSIR R package", .noWS = "outside", href = "https://github.com/lilywang1988/eSIR")),
      p("Source code: ", a("COV-IND-19 GitHub", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19")),
      p("Please direct inquiries to ",
        a("Maxwell Salvatore", .noWS = "outside", href="mailto:mmsalva@umich.edu"),", ",
        a("Alexander Rix", .noWS = "outside", href="mailto:alexrix@umich.edu"),", ",
        a("Michael Kleinsasser", .noWS = "outside", href="mailto:mkleinsa@umich.edu"),", and ",
        a("Bhramar Mukherjee", .noWS = "outside", href="mailto:bhramar@umich.edu")),
      p("The COV-IND-19 study group is comprised of: Maxwell Salvatore, Alexander Rix, Michael Kleinsasser, Daniel Barker, Lili Wang, Rupam Bhattacharyya, Soumik Purkayastha, Debashree Ray,
            Shariq Mohammed, Aritra Halder, Debraj Bose, Peter Song, Mousumi Banerjee, Veera Baladandayuthapani, and Parikshit Ghosh. Led by PI ", a("Bhramar Mukherjee", .noWS = "outside", href = "http://www-personal.umich.edu/~bhramar/"), ".")
    ),



    # Show a plot of the generated distribution
    mainPanel(
      h4("(Please wait a few seconds for the figures to load)"),
      h1(textOutput("latest")),
      h2(""),
      h2("Daily number of COVID-19 new cases, fatalities and recovered cases in India since March 1"),
      p("This figure provides the number of COVID-19 new cases (yellow), fatalities (red), and recovered cases (green) in India.
          You can hover your cursor over the bar to see the exact numerical counts."),
      plotlyOutput("plot1", height = "600px"),
      hr(),
      h2("Cumulative number of COVID-19 cases in India compared to other countries affected by the pandemic"),
      p("The x-axis starts on the day when each country exceeded 100 cases in order to allow comparison of case counts at similar stages of the outbreak.
        Use your cursor to click on countries in the legend to remove them from the plot."),
      plotlyOutput("plot2", height = "600px"),
      hr(),
      h2("Cumulative number of COVID-19 cases in India alone"),
      p("This figure displays the cumulative number of COVID-19 cases in India since the country reached 100 total cases (March 14)."),
      plotlyOutput("plot3", height = "600px"),
      hr(),
      h2("Cumulative case counts by state/union territory"),
      p("The map displays the case counts by state/union territory in India over the last few days.",
        "The darker areas of the map indicate a greater number of cases."),
      fluidRow(
        column(width = 3),
        column(width = 9, imageOutput("map", height = "650px")),
      ),
      hr(),
      h1("Predictive modeling of case counts in India under hypothetical intervention scenarios"),
      h2("Short-term impact of social distancing, travel ban, and lockdown"),
      p("In the following Figures we consider various scenarios of intervention effects to assess the effect of the lockdown.",
            "These figures should not be overinterpreted as in reality we do not know how the lockdown will actually reduce the transmission probability in India and to what extent.",
            "We use the eSIR model (",a("Wang et al. 2020", .noWS = "outside", href = "https://www.medrxiv.org/content/10.1101/2020.02.29.20029421v1.full.pdf"), ") for all our projections and create hypothetical reductions in transmission probabilities capturing interventions like social distancing and lockdown.
        This in turn reduces the basic reproduction number over the period.",
        "It was announced that India would undergo a central lockdown from March 25 until April 15.",
        "The bar plots below the predicted cumulative short-term case counts represent three scenarios: ",
        tags$ol(
          tags$li("No intervention"),
          tags$li("Social distancing and travel ban (without March 25 lockdown)"),
          tags$li("Lockdown until April 15 with a gradual, moderate resumption of daily activities")
        ),
        "Because we are using SIR models to generate the forecast, we explicitly delay changes in the basic reproduction number one week (Figure 4a) and two weeks (Figure 4b) to capture delayed onset of cases due to incubation period and timeliness of adherence.",
        "In general terms, the one-week delay models (Figures 4a, 5a, and 5b) can be thought of as 'quick adherence' to the lockdown measures, while the 'two-week' delay models (Figures 4b, 6a, and 6b) can be though of as 'slow adherence' to the lockdown measures.",
        HTML(paste0("All models assume a basic reproduction number of 2 under no intervention. The implied R", tags$sub("0"), " is 1.5 under Scenario 2.")),
        HTML(paste0("We further assume the R", tags$sub("0"), " drops to 0.8 under lockdown and then gradually rises back up to 1.5 after the lockdown ends over a three week period ('lockdown with moderate release').")),
        "You can hover of the bars for dates and natural log counts. To obtain the estimate, exponentiate the the log value seen. Also, please note the dotted line represents the upper confidence interval for the lockdown scenario (3), which is closest to the current intervention.",
        "Our codes are available on GitHub and so users can change the nature of interventions."),
      h3("Figure 4a"),
      plotlyOutput("plot4a_full", height = "600px"),
      h3("Figure 4b"),
      plotlyOutput("plot4b_full", height = "600px"),
      hr(),
      h2("Longer term forecasts post-lockdown"),
      p("We present four models: 1) Perpetual social distancing and travel ban (no lockdown; represented in yellow), 2) post-lockdown activities return to normal activities prior to any intervention ('pre-lockdown'; light blue), 3) post-lockdown activities gradually return to a moderate level ('moderate activity'; blue), 4) post-lockdown activities return to a subdued level ('hesitant'; dark blue). ",
              HTML(paste0("As in Figures 4a and 4b, Figures 5a and 5b represent an explicit one- and two-week delay in changes to R", tags$sub("0"), ", respectively.")),
              tags$ul(
              tags$li(HTML(paste0("In Scenario 1, the R", tags$sub("0"), " remains 1.5 over the entire interval."))),
              tags$li(HTML(paste0("In Scenario 2, the R", tags$sub("0"), " returns to 2 three weeks after the lockdown ends."))),
              tags$li(HTML(paste0("In Scenario 3, the R", tags$sub("0"), " returns to 1.5 three weeks after the lockdown ends."))),
              tags$li(HTML(paste0("In Scenario 4, the R", tags$sub("0"), " returns to 1.2 three weeks after the lockdown ends.")))
              ),
       ),
      h2("Quick adherence (one-week delay)"),
      h3("Figure 5a"),
      plotlyOutput("plot5a", height = "600px"),
      h3("Figure 5b"),
      plotlyOutput("plot5b", height = "600px"),
      hr(),
      h2("Slow adherence (two-week delay)"),
      h3("Figure 6a"),
      plotlyOutput("plot6a", height = "600px"),
      h3("Figure 6b"),
      plotlyOutput("plot6b", height = "600px"),
      hr(),
      h3("Contributors to the package"),
      width = 8,
    )
  )
)
)

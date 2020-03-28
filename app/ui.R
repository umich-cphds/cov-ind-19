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
             p("Read the article: ", a("Medium article", target = "_blank", href = "https://medium.com/@covind_19/predictions-and-role-of-interventions-for-covid-19-outbreak-in-india-52903e2544e6")),
             p("Read the report: ", a("COV-IND-19 Report", target = "_blank", href = "https://bit.ly/COV-IND-19_Report"), "(this is a direct download link, check your downloads folder)"),
             p("Date source: ", a("JHU CSSE COVID-19 GitHub", target = "_blank", href = "https://github.com/CSSEGISandData/COVID-19")),
             p("R modeling package: ", a("eSIR R package", target = "_blank", href = "https://github.com/lilywang1988/eSIR")),
             p("Source code: ", a("COV-IND-19 GitHub", target = "_blank", href = "https://github.com/umich-cphds/cov-ind-19")),
             p("Please direct any questions or inquiries to ", a("Bhramar Mukherjee", target="_blank", href="mailto:bhramar@umich.edu"))
        ),



    # Show a plot of the generated distribution
    mainPanel(
        h2("Daily number of COVID-19 new cases, fatalities and recovered cases in India from March 1 to current date"),
        p("This figure provides the number of COVID-19 new cases (yellow), fatalities (red), and recovered cases (green) in India. 
          You can hover your cursor over the bar to see the exact numerical counts."),
        plotlyOutput("plot1", height = "600px"),
        downloadButton("download_plot1", label = "Download Figure 1"),
        hr(),
        h2("Cumulative number of COVID-19 cases in India compared to other countries affected by the pandemic"),
        p("The x-axis starts on the day when each country exceeded 100 cases in order to allow comparison of case counts at similar stages of the outbreak. Use your cursor to click on countries in the legend to remove them from the plot."),
        plotlyOutput("plot2", height = "600px"),
        downloadButton("download_plot2", label = "Download Figure 2"),
        hr(),
        h2("Cumulative number of COVID-19 cases in India alone"),
        p("This figure displays the cumulative number of COVID-19 cases in India since the country reached 100 total cases."),
        plotlyOutput("plot3", height = "600px"),
        downloadButton("download_plot3", label = "Download Figure 3"),
        hr(),
        h2("Cumulative case counts by state/union/territory"),
        p("The map displays the case counts by state/union territory in India over the last few days.",
          "The darker areas of the map indicate a greater number of cases."),
        fluidRow(
            column(width = 3), 
            column(width = 9, imageOutput("map", height = "650px")),
        ), 
        hr(),
        h2(HTML(paste0("Predicted cases in India until June 15 (assuming R", tags$sub("0"), " = 2)"))),
        p("This figure plots the observed number of cases up until today and then forecasts the number of cases until June 15.",
          "The bars represent our best guess and the dashed line represents the upper credible limit of predicted cases for India.",
          "This graph is assuming a basic reproduction number (or R0) of 2."),
        plotOutput("plot4a_full", height = "600px"),
        downloadButton("download_plot4a", label = "Download Figure 4a"),
        hr(),
        h2(HTML(paste0("Predicted cases in India until June 15 (assuming R", tags$sub("0"), " = 2)"))),
        p("This figure plots the observed number of cases up until today and then forecasts the number of cases until June 15.",
          "The bars represent our best guess and the dashed line represents the upper credible limit of predicted cases for India.",
          "This graph is assuming a basic reproduction number (or R0) of 2."),
        plotOutput("plot4b_full", height = "600px"),
        downloadButton("download_plot4b", label = "Download Figure 4b"),
        hr(),
        h2(HTML(paste0("Predicted cases in India until June 15 (assuming R", tags$sub("0"), " = 2.5)"))),
        p("This figure plots the observed number of cases up until today and then forecasts the number of cases until June 15.",
          "The bars represent our best guess and the dashed line represents the upper credible limit of predicted cases for India.",
          "This graph is assuming a basic reproduction number (or R0) of 2.5."),
        plotOutput("plot5a_full", height = "600px"),
        downloadButton("download_plot5a", label = "Download Figure 5a"),
        hr(),
        h2(HTML(paste0("Predicted cases in India until June 15 (assuming R", tags$sub("0"), " = 2.5)"))),
        p("This figure plots the observed number of cases up until today and then forecasts the number of cases until June 15.",
          "The bars represent our best guess and the dashed line represents the upper credible limit of predicted cases for India.",
          "This graph is assuming a basic reproduction number (or R0) of 2.5."),
        plotOutput("plot5b_full", height = "600px"),
        downloadButton("download_plot5b", label = "Download Figure 5b"),
        hr(),
        h3("Acknowledgments"),
        p("The COV-IND-19 study group is comprised of: Debashree Ray, Rupam Bhattacharyya, Lili Wang, Maxwell Salvatore,
          Shariq Mohammed, Aritra Halder, Yiwang Zhou, Peter Song, Soumik Purkayastha, Mike Kleinsasser, Daniel Barker,
          Debraj Bose, Mousumi Banerjee, Veera Baladandayuthapani, Parikshit Ghosh, and Bhramar Mukherjee."),
        # hr(),
        # h2("Figure 3c"),
        # imageOutput("plot3c_metro"),
        # hr(),
        # h2("Figure 3d"),
        # imageOutput("plot3d_metro"),
        width = 8,
    )
)
)
)

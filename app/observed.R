observed <- tabPanel("National Observed",
  fluidRow(column(width = 12, top_matter)),
  fluidRow(column(width = 1),
    column(width = 10,
      h4("(Please wait a few seconds for the figures to load)"),
      h4(textOutput("latest")),
      h2(""),
      h2("Daily number of new COVID-19 cases, fatalities and recovered in India"),
      p("This figure provides the number of COVID-19 new cases (yellow),
        fatalities (red), and recovered cases (green) in India. You can
        hover your cursor over the bar to see the exact numerical counts."
      ),
      plotlyOutput("India_p1", height = "600px"),
      hr(),
      h2("Total number of COVID-19 cases and deaths"),
      p("The first figure represents COVID-19 case counts where the x-axis
        starts on the day when each country passed 100 cases. The second
        figure represents COVID-19 fatalities where the x-axis starts on the
        day when each country exceeded 3 fatalities. These axes allow
        comparison of counts at similar stages of the outbreak. You can click
        on countries in the legend to add or remove them and you can hover
        your cursor over the lines to see the exact numerical counts."
      ),
      plotlyOutput("India_p2", height = "800px"),
      hr(),
      h2("Cumulative case counts by state/union territory"),
      p("The map displays the case counts by state/union territory in India
        over the last few days.", "The darker areas of the map indicate a
        greater number of cases."
      ),
      HTML(paste0("<center><img src=", img.file, "></center>")),
      hr(),
      h2("Cumulative COVID-19 case count by state/union territory"),
      plotOutput("India_p7b", height = "600px"),
      downloadLink('downloadFacet_cases', 'Download'),
      h2("Cumulative COVID-19 death count by state/union territory"),
      plotOutput("India_p7d", height = "600px"),
      downloadLink('downloadFacet_deaths', 'Download'),
      hr(),
      #h2("Doubling graphs"),
      #p("The following figures visualize the case and death data to depict
      #  how long it takes each country to double its case or death count."),
      #h3("Cases"),
      #plotlyOutput("India_p3a", height = "600px"),
      #h3("Deaths"),
      #plotlyOutput("India_p3b", height = "600px"),
      #hr()
    ),
    column(width = 1)
  )
)

top_matter <- wellPanel(
  fluidRow(
    h3("COV-IND-19 Study Group"),
    fluidRow(
      column(width = 2, shiny::img(src = "group_logo.png", height = 200, width = 200)),
      column(width = 10,
        p("Welcome to the COV-IND-19 shiny app. We aim to provide a resource to
          describe the COVID-19 outbreak in India to date as well as prediction
          models under various hypothetical scenarios. The figure and forecasting
          models update as new data becomes available (i.e., at least daily). You
          may download PNG files of each figure by clicking on the camera icon
          when you are hovering within each plot. Please cite our medium article
          and this website in any publication that you use this resource for."),
        p("The COV-IND-19 study group is comprised of: Maxwell Salvatore,
          Alexander Rix, Michael Kleinsasser, Daniel Barker, Lili Wang, Rupam
          Bhattacharyya, Soumik Purkayastha, Debashree Ray, Shariq Mohammed,
          Aritra Halder, Debraj Bose, Peter Song, Mousumi Banerjee, Veera
          Baladandayuthapani, and Parikshit Ghosh. Led by PI ",
          a("Bhramar Mukherjee", .noWS = "outside", href = "http://www-personal.umich.edu/~bhramar/"), "."
        ),
        p("Please direct inquiries to ",
          a("Maxwell Salvatore", .noWS = "outside", href="mailto:mmsalva@umich.edu"),", ",
          a("Alexander Rix", .noWS = "outside", href="mailto:alexrix@umich.edu"),", ",
          a("Michael Kleinsasser", .noWS = "outside", href="mailto:mkleinsa@umich.edu"),", and ",
          a("Bhramar Mukherjee", .noWS = "outside", href="mailto:bhramar@umich.edu")
        ),
        column(5,
          h4("References"),
          p("Read the study: ", a("Ray et al. 2020", .noWS = "outside", href = "https://www.medrxiv.org/content/10.1101/2020.04.15.20067256v1")),
          p("Read the report: ", a("COV-IND-19 Report", .noWS = "outside", href = "https://bit.ly/COV-IND-19_Report"),
            " (this is a direct download link, check your downloads folder)"),
          p("Read our Medium trilogy: ", a("pre-lockdown (March 21)", .noWS = "outside",
            href = "https://medium.com/@covind_19/predictions-and-role-of-interventions-for-covid-19-outbreak-in-india-52903e2544e6"),", ", a("studying lockdown (April 3)", .noWS = "outside",
            href = "https://medium.com/@covind_19/historic-lockdown-prediction-models-to-study-lockdown-effects-and-the-role-of-data-in-the-crisis-a0afeeec5a6"), ", and ", a("unlocking the lockdown (April 24)", .noWS = "outside",
            href = "https://medium.com/@covind_19/unlocking-the-40-day-national-lockdown-in-india-there-is-no-magic-key-de4e43177cb4")),
          p("Source code: ", a("COV-IND-19 GitHub", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19"))
        ),
        column(5,
          h4("Sources"),
          p("Non-India country-level data source: ", a("JHU CSSE COVID-19 GitHub", .noWS = "outside",
            href = "https://github.com/CSSEGISandData/COVID-19")),
          p("India National and State / Union Territory data source: ",
            a("covid19india.org", .noWS = "outside", href = "https://www.covid19india.org")),
          p("R modeling package: ", a("eSIR R package", .noWS = "outside",
            href = "https://github.com/lilywang1988/eSIR"))
        ),
      )
    )
  )
)

top_matter <- wellPanel(
                         fluidRow(
                           h3("COV-IND-19 Study Group"),
                           fluidRow(
                             column(width = 2, shiny::img(src = "group_logo.png", height = 200, width = 200)),
                             column(width = 1),
                             column(width = 9,
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
                                    fluidRow(
                                      column(5, 
                                             h4("Sources"),
                                             p("Non-India country-level data source: ", a("JHU CSSE COVID-19 GitHub", .noWS = "outside",
                                                                                          href = "https://github.com/CSSEGISandData/COVID-19")),
                                             p("India National and State / Union Territory data source: ",
                                               a("covid19india.org", .noWS = "outside", href = "https://www.covid19india.org")),
                                             p("R modeling package: ", a("eSIR R package", .noWS = "outside",
                                                                         href = "https://github.com/lilywang1988/eSIR"))
                                      ),
                                      column(5,
                                             h4("How to cite COVIND19.org:"),
                                             p("Ray, D., Salvatore, M., Bhattacharyya, R., Wang, L., Du, J., Mohammed, S., … Mukherjee, B. (2020). Predictions, Role of Interventions and Effects of a Historic National Lockdown in India’s Response to the the COVID-19 Pandemic: Data Science Call to Arms. ", em("Harvard Data Science Review", .noWS = "outside"), ". ",
                                               a("https://doi.org/10.1162/99608f92.60e08ed5", .noWS = "outside", href = "https://doi.org/10.1162/99608f92.60e08ed5"))
                                      )
                                    ),
                                    p("Sub-app containing the other states: ", a("COVID-19 India", .noWS = "outside", href = "https://umich-biostatistics.shinyapps.io/covid19b/"))
                                    
                             )
                           )
                         )
                       )
                       


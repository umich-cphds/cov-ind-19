
references <- 
  tabPanel("References",
           
           wellPanel(
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
           ),
           wellPanel(
             fluidRow(
               column(10,
                      h4("References"),
                      p("On the resurgence of COVID-19 in India: ", a("Salvatore et al. 2021", .noWS = "outside", href = "https://www.medrxiv.org/content/10.1101/2021.06.23.21259405v1")),
                      p("Estimating COVID-19 related mortality in India: ", a("Zimmermann et al. 2021", .noWS = "outside", href = "https://www.preprints.org/manuscript/202105.0617/v1")),
                      p("Incorporating false-negative tests into COVID-19 models: ", a("Bhattacharyya et al. 2021", .noWS = "outside", href = "https://www.nature.com/articles/s41598-021-89127-1")),
                      p("Read our forecasting and lockdown effects study: ", a("Ray et al. 2020", .noWS = "outside", href = "https://hdsr.mitpress.mit.edu/pub/r1qq01kw/release/2")),
                      p("Read our nation vs state-level trends paper: ", a("Salvatore et al. 2020", .noWS = "outside", href = "https://bmjopen.bmj.com/content/10/12/e041778")),
                      p("Comparing 5 COVID-19 models in India: ", a("Purkayastha et al. 2020", .noWS = "outside", href = "https://bmcinfectdis.biomedcentral.com/articles/10.1186/s12879-021-06077-9")),
                      p("Extending SEIR model for imperfect testing: ", a("Bhaduri et al. 2020", .noWS = "outside", href = "https://doi.org/10.1101/2020.09.24.20200238"),
                        p("Read the report: ", a("COV-IND-19 Report", .noWS = "outside", href = "https://bit.ly/COV-IND-19_Report"),
                          " (direct download link, check downloads folder)"),
                        p(a("View webinar slides", .noWS = "outside", href = "https://sph.umich.edu/precision-health-data-science/research/pdf/COVIND19_UConn_May22.pdf"), " (direct download link, check downloads folder)"),
                        p("Read our Medium trilogy: ", a("pre-lockdown (March 21)", .noWS = "outside",
                                                         href = "https://medium.com/@covind_19/predictions-and-role-of-interventions-for-covid-19-outbreak-in-india-52903e2544e6"),", ", a("studying lockdown (April 3)", .noWS = "outside",
                                                                                                                                                                                           href = "https://medium.com/@covind_19/historic-lockdown-prediction-models-to-study-lockdown-effects-and-the-role-of-data-in-the-crisis-a0afeeec5a6"), ", and ", a("unlocking the lockdown (April 24)", .noWS = "outside",
                                                                                                                                                                                                                                                                                                                                                             href = "https://medium.com/@covind_19/unlocking-the-40-day-national-lockdown-in-india-there-is-no-magic-key-de4e43177cb4")),
                        p("Directory of ", a("all Medium articles", .noWS = "outside", href = "https://bhramarm.medium.com")),
                        p("COV-IND-19 app ", a("source code repository", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19"), " and ", a("data repository", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19-data")),
                        p("Source code: ", a("COV-IND-19 GitHub", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19")),
                        p("R package: ", a("SEIRfansy", .noWS = "outside", href = "https://github.com/umich-biostatistics/SEIRfansy"))
                      ))
             )
           )
           
  )
  
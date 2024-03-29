library(gt)

metrics <- tabPanel("Metrics",
                    fluidRow(column(width = 1),
                             column(width = 10,
                                    h4("(Please wait a few seconds for the figures to load)"),
                                    hr(),
                                    gt_output("India_gt"),
                                    downloadButton("download_gt_point", 'Download point in time metrics table'),
                                    downloadButton("download_gt_cumulative", 'Download cumulative metrics table'),
                                    hr(),
                                    plotOutput("India_pforest_cfr1", height = "500px", width = "800px"),
                                    hr(),
                                    #plotOutput("India_pforest_dbl", height = "500px", width = "800px"),
                                    #hr(),
                                    plotOutput("India_pforest_r_est", height = "500px", width = "800px"),
                                    hr(),
                                    # plotOutput("India_pforest_tp", height = "500px", width = "800px"),
                                    # hr(),
                                    h4("Download and save this dashboard for future reference using
                                       the download button."),
                                    downloadButton('download_dashboard', 'Download dashboard'),
                                    hr()
                             ),
                             column(width = 1)
                    )
)

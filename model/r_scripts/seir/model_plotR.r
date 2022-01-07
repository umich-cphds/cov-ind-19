model_plotR <- function(Result = NULL, mcmc_pars = NULL, T_predict = NULL, T_simu = NULL, n_period, data,
                        data_test = NULL, data_initial, call, save_plots = TRUE, trace_plot.common_axis = FALSE, model = "Multinomial", ...){
  
  if(call == "estimateR"){
    ## Trace plot
    pars_estimate = mcmc_pars
    pars_estimate_df = data.frame(value = as.vector(pars_estimate),
                                  variable_name = rep(c("beta", "r", "R0"), each = nrow(pars_estimate)*ncol(pars_estimate)/3),
                                  variable_index = rep(rep(as.character(1:(ncol(pars_estimate)/3)), each = nrow(pars_estimate)), times = 3),
                                  iteration = rep(1:nrow(pars_estimate), times = ncol(pars_estimate)))

    library(ggplot2)

    trace_plot_theme <-   theme_minimal() +
      theme(
        plot.title         = element_text(size = 18, face = "bold"),
        plot.subtitle      = element_text(size = 16, color = "#36454f"),
        plot.caption       = element_text(hjust = 0, size = 16, lineheight = 1.1),
        axis.text.x        = element_text(size = 16, color = "#36454f", angle = 45),
        axis.text.y        = element_text(size = 16, color = "#36454f"),
        axis.title         = element_text(size = 16, face = "italic"),
        strip.text.x       = element_text(size = 16, color = "white"),
        strip.text.y       = element_text(size = 16, color = "white"),
        legend.title       = element_blank() ,
        legend.text        = element_text(size = 16) ,
        panel.grid.major.x = element_line(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.x = element_line(),
        panel.grid.minor.y = element_line(),
        legend.position = "none",
        strip.background = element_rect(fill = "grey", color = "grey")
      )
    trace_plot_colors <- c("#3CAEA3", "#0472cf", "#173F5F")
    trace_plot <- ggplot(pars_estimate_df, aes(x = iteration, y=value)) +
      geom_line(aes(col = variable_name) ) +
      facet_wrap(variable_name ~ variable_index,  scales='free_y', labeller = label_parsed,
                 dir = "h", ncol = ncol(pars_estimate)/3, nrow = 3) +
      trace_plot_theme +
      scale_color_manual(values = trace_plot_colors) +
      scale_x_continuous(breaks = seq(0,nrow(pars_estimate), length = 3))

    trace_plot <- ggplot(pars_estimate_df %>% mutate(group = paste(variable_name,variable_index, sep="")),
                         aes(x = iteration, y=value)) +
      geom_line(aes(col = variable_name) ) +
      facet_wrap(~ group,  scales='free_y', labeller = label_parsed,
                 dir = "h", ncol = ncol(pars_estimate)/3, nrow = 3) +
      trace_plot_theme +
      scale_color_manual(values = trace_plot_colors) +
      scale_x_continuous(breaks = round(seq(0,nrow(pars_estimate), length = 3)))
    if(trace_plot.common_axis == TRUE)
      trace_plot = trace_plot + facet_grid(variable_name ~ variable_index, scale = "free",labeller = label_parsed)

    if(save_plots == TRUE)
      ggsave("trace_plot.png", trace_plot, width = 2*ncol(mcmc_pars), height = 15, dpi = 300, limitsize = FALSE)
    
    ## Plot - Reproduction number

    R0_pred = mcmc_pars[ , 2*n_period + 1:n_period]
    data_R0 <- data.frame(value = as.vector(as.matrix(R0_pred)),
                          phase = rep(unlist(lapply(1:n_period, function(x) paste0("Period ", x))), each = nrow(R0_pred)))

    fun_color_range <- colorRampPalette(c( "#00A972", "#007FA9"))
    library(DescTools)
    my_colors <- fun_color_range(n_period)

    R0_theme <-   theme_minimal() +
      theme(
        plot.title         = element_text(size = 18, face = "bold"),
        plot.subtitle      = element_text(size = 14, color = "#36454f"),
        plot.caption       = element_text(hjust = 0, size = 10, lineheight = 1.1),
        axis.text          = element_text(size = 16, color = "#36454f"),
        axis.title         = element_text(size = 16, face = "italic"),
        legend.title       = element_blank() ,
        legend.text        = element_text(size = 16) ,
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom"
      )

    library("patchwork")
    boxplot_R0 <- ggplot(data_R0, aes(x = phase, y=value, fill = phase, color = phase)) +
      geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
      stat_summary(aes(fill = phase), fun =mean, geom="point", shape=18, size=4,show.legend = FALSE) +
      R0_theme +
      labs(title="Basic reproduction number : 95% CI",x="", y = "value") +
      theme(legend.position = "none") +
      scale_color_manual(values = MixColor(my_colors, "#000000", 0.75)) +
      scale_fill_manual(values = my_colors) +
      plot_annotation(caption = "* The diamond shapes indicate mean of the estimates",
                      theme = theme(plot.caption = element_text(size = 14)))

    if(save_plots == TRUE)
      ggsave("boxplot_R0.png", boxplot_R0, width = 8, height = 10, dpi = 300)

    print(trace_plot)
    print(boxplot_R0)
    return(list("trace_plot" = trace_plot, "boxplot_R0" = boxplot_R0))

  }

  if(call == "predictR"){
    ## Panel plot - check fit

    prediction = Result$prediction
    prediction_mean = rowMeans(prediction)
    P_current_pred_mean = prediction_mean[3 * T_simu + 1:T_simu]
    RR_current_pred_mean = prediction_mean[6 * T_simu + 1:T_simu]
    DR_current_pred_mean = prediction_mean[8 * T_simu + 1:T_simu]
    P_total_pred_mean = P_current_pred_mean + RR_current_pred_mean + DR_current_pred_mean

    prediction_2.5 = apply(prediction, 1, function(x) quantile(x, probs = 0.025))
    P_current_pred_2.5 = prediction_2.5[3 * T_simu + 1:T_simu]
    RR_current_pred_2.5 = prediction_2.5[6 * T_simu + 1:T_simu]
    DR_current_pred_2.5 = prediction_2.5[8 * T_simu + 1:T_simu]
    P_total_pred_2.5 = P_current_pred_2.5 + RR_current_pred_2.5 + DR_current_pred_2.5

    prediction_97.5 = apply(prediction, 1, function(x) quantile(x, probs = 0.975))
    P_current_pred_97.5 = prediction_97.5[3 * T_simu + 1:T_simu]
    RR_current_pred_97.5 = prediction_97.5[6 * T_simu + 1:T_simu]
    DR_current_pred_97.5 = prediction_97.5[8 * T_simu + 1:T_simu]
    P_total_pred_97.5 = P_current_pred_97.5 + RR_current_pred_97.5 + DR_current_pred_97.5

    T_train = nrow(data)
    T_test = ifelse(!is.null(data_test), nrow(data_test), 0)


    P_total_obs = data_initial[1] + cumsum(c(0, data$Confirmed[2:T_train], data_test$Confirmed))
    if(model == "Multinomial"){
      RR_current_obs = data_initial[2] + cumsum(c(0, data$Recovered[2:T_train], data_test$Recovered))
      DR_current_obs = data_initial[3] + cumsum(c(0, data$Deceased[2:T_train], data_test$Deceased))
      P_current_obs = P_total_obs - RR_current_obs - DR_current_obs
    }
    data_type = c(rep("data_train", T_train), rep("data_test", T_test))

    date_pred = 1:T_simu
    date_obs = 1:(T_train + T_test)

    panel_theme <-   theme_minimal() +
      theme(
        plot.title         = element_text(size = 18, face = "bold"),
        plot.subtitle      = element_text(size = 14, color = "#36454f"),
        plot.caption       = element_text(hjust = 0, size = 10, lineheight = 1.1),
        axis.text          = element_text(size = 16, color = "#36454f"),
        axis.title         = element_text(size = 16, face = "italic"),
        legend.title       = element_blank() ,
        legend.text        = element_text(size = 16) ,
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom"
      )

    if(model == "Multinomial"){
      data_plot_current_case = data.frame("values_mean" = c(P_current_pred_mean, P_current_obs),
                                        "values_2.5" = c(P_current_pred_2.5, P_current_obs),
                                        "values_97.5" = c(P_current_pred_97.5, P_current_obs),
                                        "pred_data" = c(rep("predicted", times = T_simu), rep("data", times = (T_train + T_test))),
                                        "date" = c(date_pred, date_obs),
                                        "test_train_pred" = c(rep("95% CI train", T_train), rep("95% CI test", T_test), rep("95% CI prediction", T_predict - T_test), rep("95% CI train", T_train), rep("95% CI test", T_test)),
                                        "test_train_pred_data" = c(rep("train", T_train), rep("test", T_test), rep("prediction", T_predict - T_test), rep("data", T_train + T_test)))

    plot_current_case <- ggplot(data = data_plot_current_case, aes(x = date)) +
      geom_ribbon(aes(ymin = values_2.5, ymax = values_97.5, fill = test_train_pred),alpha = 0.3, subset(data_plot_current_case, pred_data %in% c("predicted"))) +
      geom_line(aes(y = values_mean, col = test_train_pred_data), size = 1.2) +
      panel_theme +
      labs(title="Current cases",x="days from onset", y = "cases") +
      scale_y_continuous(labels = scales::comma ) +
      scale_fill_manual(values = c("#3CAEA3", "#0472cf", "#173F5F")) +
      scale_color_manual(values = c("#C32900", "#3CAEA3", "#0472cf", "#173F5F"))
    }

    data_plot_total_case = data.frame("values_mean" = c(P_total_pred_mean, P_total_obs),
                                      "values_2.5" = c(P_total_pred_2.5, P_total_obs),
                                      "values_97.5" = c(P_total_pred_97.5, P_total_obs),
                                      "pred_data" = c(rep("predicted", times = T_simu), rep("data", times = (T_train + T_test))),
                                      "date" = c(date_pred, date_obs),
                                      "test_train_pred" = c(rep("95% CI train", T_train), rep("95% CI test", T_test), rep("95% CI prediction", T_predict - T_test), rep("95% CI train", T_train), rep("95% CI test", T_test)),
                                      "test_train_pred_data" = c(rep("train", T_train), rep("test", T_test), rep("prediction", T_predict - T_test), rep("data", T_train + T_test)))
    plot_total_case <- ggplot(data = data_plot_total_case, aes(x = date)) +
      geom_ribbon(aes(ymin = values_2.5, ymax = values_97.5, fill = test_train_pred),alpha = 0.3, subset(data_plot_total_case, pred_data %in% c("predicted"))) +
      geom_line(aes(y = values_mean, col = test_train_pred_data), size = 1.2) +
      panel_theme +
      labs(title="Total cases",x="days from onset", y = "cases") +
      scale_y_continuous(labels = scales::comma ) +
      scale_fill_manual(values = c("#3CAEA3", "#0472cf", "#173F5F")) +
      scale_color_manual(values = c("#C32900", "#3CAEA3", "#0472cf", "#173F5F"))

    if(model == "Multinomial"){
      data_plot_recovery = data.frame("values_mean" = c(RR_current_pred_mean, RR_current_obs),
                                    "values_2.5" = c(RR_current_pred_2.5, RR_current_obs),
                                    "values_97.5" = c(RR_current_pred_97.5, RR_current_obs),
                                    "pred_data" = c(rep("predicted", times = T_simu), rep("data", times = (T_train + T_test))),
                                    "date" = c(date_pred, date_obs),
                                    "test_train_pred" = c(rep("95% CI train", T_train), rep("95% CI test", T_test), rep("95% CI prediction", T_predict - T_test), rep("95% CI train", T_train), rep("95% CI test", T_test)),
                                    "test_train_pred_data" = c(rep("train", T_train), rep("test", T_test), rep("prediction", T_predict - T_test), rep("data", T_train + T_test)))

    plot_recovery <- ggplot(data = data_plot_recovery, aes(x = date)) +
      geom_ribbon(aes(ymin = values_2.5, ymax = values_97.5, fill = test_train_pred),alpha = 0.3, subset(data_plot_recovery, pred_data %in% c("predicted"))) +
      geom_line(aes(y = values_mean, col = test_train_pred_data), size = 1.2) +
      panel_theme +
      labs(title="Total recoveries",x="days from onset", y = "recoveries") +
      scale_y_continuous(labels = scales::comma ) +
      scale_fill_manual(values = c("#3CAEA3", "#0472cf", "#173F5F")) +
      scale_color_manual(values = c("#C32900", "#3CAEA3", "#0472cf", "#173F5F"))

    data_plot_death = data.frame("values_mean" = c(DR_current_pred_mean, DR_current_obs),
                                 "values_2.5" = c(DR_current_pred_2.5, DR_current_obs),
                                 "values_97.5" = c(DR_current_pred_97.5, DR_current_obs),
                                 "pred_data" = c(rep("predicted", times = T_simu), rep("data", times = (T_train + T_test))),
                                 "date" = c(date_pred, date_obs),
                                 "test_train_pred" = c(rep("95% CI train", T_train), rep("95% CI test", T_test), rep("95% CI prediction", T_predict - T_test), rep("95% CI train", T_train), rep("95% CI test", T_test)),
                                 "test_train_pred_data" = c(rep("train", T_train), rep("test", T_test), rep("prediction", T_predict - T_test), rep("data", T_train + T_test)))

    plot_death <- ggplot(data = data_plot_death, aes(x = date)) +
      geom_ribbon(aes(ymin = values_2.5, ymax = values_97.5, fill = test_train_pred),alpha = 0.3, subset(data_plot_death, pred_data %in% c("predicted"))) +
      geom_line(aes(y = values_mean, col = test_train_pred_data), size = 1.2) +
      panel_theme +
      labs(title="Total deaths",x="days from onset", y = "deaths") +
      scale_y_continuous(labels = scales::comma ) +
      scale_fill_manual(values = c("#3CAEA3", "#0472cf", "#173F5F")) +
      scale_color_manual(values = c("#C32900", "#3CAEA3", "#0472cf", "#173F5F"))


    library(ggpubr)
    panel_plot_fit <- ggarrange(plot_current_case + theme(legend.position = "none"),
                                plot_total_case + theme(legend.position = "none"),
                                plot_recovery + theme(legend.position = "none"),
                                plot_death + theme(legend.position = "none"),
                                nrow = 2, ncol = 2,
                                labels = c("A", "B", "C", "D"), common.legend = TRUE, legend="bottom")
    } else{
      panel_plot_fit <- plot_total_case
    }

    if(save_plots == TRUE)
      ggsave("panel_plot_fit.png", panel_plot_fit, width = 12, height = 12, units = "in", dpi = 300)

    ## Plot - Undetected, Detected and False Negatives

    prediction_mean = rowMeans(prediction)
    U_current_pred = prediction_mean[2 * T_simu + 1:T_simu]
    P_current_pred = prediction_mean[3 * T_simu + 1:T_simu]
    F_current_pred = prediction_mean[4 * T_simu + 1:T_simu]

    data_detected_undetected_plot = data.frame("date" = rep(1:T_simu, times = 3),
                                               "count" = c( U_current_pred, F_current_pred, P_current_pred),
                                               "type" = factor(rep(c("Untested", "False Negative", "Confirmed"), each = T_simu), levels = c("Untested", "False Negative", "Confirmed")))

    detected_undetected_theme <-   theme_minimal() +
      theme(
        plot.title         = element_text(size = 18, face = "bold"),
        plot.subtitle      = element_text(size = 14, color = "#36454f"),
        plot.caption       = element_text(hjust = 0, size = 10, lineheight = 1.1),
        axis.text          = element_text(size = 16, color = "#36454f"),
        axis.title         = element_text(size = 16, face = "italic"),
        legend.title       = element_blank() ,
        legend.text        = element_text(size = 16) ,
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom"
      )

    plot_detected_undetected <- ggplot(data_detected_undetected_plot, aes(x = date, y = count)) +
      geom_col(aes(fill = type), alpha = 1) +
      detected_undetected_theme +
      scale_fill_manual(values = c("#173F5F", "#0472cf", "#3CAEA3")) +
      labs(title="Current Cases - Untested, Detected and False Negatives", x = "days from onset", y = "cases") +
      scale_y_continuous(labels = scales::comma)

    if(save_plots == TRUE)
      ggsave("plot_detected_undetected.png", plot_detected_undetected, width = 12, height = 10, units = "in", dpi = 300)

    print(panel_plot_fit)
    print(plot_detected_undetected)

    return(list("panel_plot_fit" = panel_plot_fit, "plot_detected_undetected" = plot_detected_undetected))


  }

}

# libraries -----------
library(tidyverse)
library(here)
library(ggtext)
library(extrafont)
library(glue)
library(EpiEstim)
library(httr)
library(janitor)
library(scales)
library(gridExtra)
library(grid)
library(vroom)

plot_fig_forest = function() {
  
  # ggplot theme ------------
  covind19_base <- theme_minimal() +
    theme(
      # text               = element_text(family = "Helvetica Neue"),
      plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
      plot.subtitle      = element_text(size = 14, color = "#36454f"),
      plot.caption       = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1),
      axis.text          = element_text(size = 10, color = "#36454f"),
      axis.title         = element_text(size = 12, face = "italic"),
      legend.position    = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  cfr1 = read_csv(paste0(data_repo, today, '/cfr_t7_avg.csv'), col_types = cols())
  everything = read_csv(paste0(data_repo, today, '/everything.csv'), col_types = cols())
  r0 = read_csv(paste0(data_repo, today, '/r0_t7_avg.csv'), col_types = cols())
  
  fplot_colors <- c(
    "alarm" = "#eb4034",
    "eh"    = "gray40",
    "good"  = "#138808",
    "india" = "black"
  )
  
  cfr_danger <- 0.06
  cfr_safe   <- 0.03
  
  cfr_title    <- "Case fatality rate for COVID-19 in India by state/union territory"
  cfr_subtitle <- glue("as of {format(as.Date(today), '%B %e')}")
  cfr_x_lab    <- "State/Union territory"
  cfr_y_lab    <- "CFR"
  cfr_caption  <- glue("**\uA9 COV-IND-19 Study Group**<br>",
                       "**Source:** covid19india.org<br>",
                       "**Note:**<br>",
                       " - Estimate and 95% confidence interval are provided in each plot by state.<br>",
                       " - Colored red if estimate is above 0.03 and green if below 0.01.<br>",
                       " - Estimation is based on all cases confirmed till May 18.<br>",
                       " - CFR stands for case-fatality rate.")
  
  cfr1_for <- cfr1 %>%
      mutate(
        fplot = ifelse(place == "National estimate", "india", ifelse(cfr > cfr_danger, "alarm", ifelse(cfr < cfr_safe, "good", "eh"))) # change 
      ) %>%
      mutate(
        shape = ifelse(fplot == "india", "india", "not_india"),
        size  = ifelse(shape == "india", .5, .2)
      ) %>%
      ggplot(aes(x = fct_relevel(reorder(place, cfr), "National estimate"), y = cfr, shape = shape)) +
      geom_hline(yintercept = cfr_safe, color = "gray40", linetype = 2) +
      geom_hline(yintercept = cfr_danger, color = "gray40", linetype = 2) +
      geom_pointrange(aes(ymin = lower, ymax = upper, color = fplot), size = 0.4) +
      scale_color_manual(values = fplot_colors) +
      scale_shape_manual(values = c("not_india" = 16, "india" = 18)) +
      labs(
        title    = cfr_title,
        subtitle = cfr_subtitle,
        x        = cfr_x_lab,
        y        = cfr_y_lab,
        caption  = cfr_caption
      ) +
      coord_flip(
        ylim = c(0, 0.1)
      ) +
      covind19_base
  
  state_t7_avg <- everything %>%
    group_by(place) %>%
    slice((n()-6):n()) %>%
    summarise(
      lower = min(dbl, na.rm = T),
      upper = max(dbl, na.rm = T),
      dbl   = mean(dbl, na.rm = T)
    ) %>%
    drop_na(dbl) %>%
    ungroup() %>%
    mutate(place = recode(place, "India" = "National estimate"))
  
  dbl_danger <- 21
  dbl_safe   <- 28
  
  dbl_for <- state_t7_avg %>%
      mutate(
        fplot = ifelse(dbl < dbl_danger, "alarm", ifelse(dbl > dbl_safe, "good", "eh"))
      ) %>%
      mutate(
        shape = ifelse(fplot == "india", "india", "not_india"),
        size  = ifelse(shape == "india", .5, .2),
        fplot = case_when(place == "National estimate" ~ "india", TRUE ~ fplot)
      ) %>%
      ggplot(aes(x = fct_relevel(reorder(place, desc(dbl)), "National estimate"), y = dbl, shape = shape)) +
      geom_hline(yintercept = dbl_danger, color = "gray40", linetype = 2) +
      geom_hline(yintercept = dbl_safe, color = "gray40", linetype = 2) +
      geom_pointrange(aes(ymin = lower, ymax = upper, color = fplot), size = 0.4) +
      scale_color_manual(values = fplot_colors) +
      scale_shape_manual(values = c("not_india" = 16, "india" = 18)) +
      labs(
        title    = "Doubling time for COVID-19 in India<br>by state/union territory",
        subtitle = glue("as of {format(as.Date(today), '%B %e')}"),
        x        = "State/Union territory",
        y        = "Doubling time (days)",
        caption  = glue("**\uA9 COV-IND-19 Study Group**<br>",
                        "**Source:** covid19india.org<br>",
                        "**Note:** <br>",
                        " - Colored red if estimate is below {dbl_danger} and green if above {dbl_safe}.<br>",
                        " - Intervals represent the range of doubling times over the last 7 days.")
      ) +
      coord_flip() +
      covind19_base
  
  r_safe   <- 1
  r_danger <- 1.5
  
  r_est_for <- r0 %>%
      mutate(
        fplot = ifelse(r > r_danger, "alarm", ifelse(r < r_safe, "good", "eh")),
        place = recode(place, "India" = "National estimate")
      ) %>%
      mutate(
        shape = ifelse(fplot == "india", "india", "not_india"),
        size  = ifelse(shape == "india", .5, .2),
        fplot = case_when(place == "National estimate" ~ "india", TRUE ~ fplot)
      ) %>%
      ggplot(aes(x = fct_relevel(reorder(place, r), "National estimate"), y = r, shape = shape)) +
      geom_hline(yintercept = r_safe, color = "gray40", linetype = 2) +
      geom_hline(yintercept = r_danger, color = "gray40", linetype = 2) +
      geom_pointrange(aes(ymin = lower, ymax = upper, color = fplot), size = 0.4) +
      scale_color_manual(values = fplot_colors) +
      scale_shape_manual(values = c("not_india" = 16, "india" = 18)) +
      labs(
        title    = "R for COVID-19 in India by state/union territory",
        subtitle = glue("as of {format(as.Date(today), '%B %e')}"),
        x        = "State/Union territory",
        y        = "R",
        caption  = glue("**\uA9 COV-IND-19 Study Group**<br>",
                        "**Source:** covid19india.org<br>",
                        "**Note:**<br>",
                        " - Average estimate and 95% confidence interval for last 7 days are provided in each plot by state.<br>",
                        " - Colored red if estimate is above {r_danger} and green if below {r_safe}.")
      ) +
      coord_flip(ylim = c(0, 3.5)) +
      covind19_base
  
  state_test_plt_dat <- everything %>%
    group_by(place) %>%
    slice((n()-6):n()) %>%
    summarise(
      lower    = min(tpr, na.rm = T),
      upper    = max(tpr, na.rm = T),
      test_pos = mean(tpr, na.rm = T)
    ) %>%
    drop_na(test_pos) %>%
    ungroup()
  
  tpr_safe   <- 0.02
  tpr_danger <- 0.06
  
  tp_for <- state_test_plt_dat %>%
      mutate(
        fplot = ifelse(test_pos > tpr_danger, "alarm", ifelse(test_pos < tpr_safe, "good", "eh")),
        place = recode(place, "India" = "National estimate")
      ) %>%
      mutate(
        shape = ifelse(fplot == "india", "india", "not_india"),
        size  = ifelse(shape == "india", .5, .2),
        fplot = case_when(place == "National estimate" ~ "india", TRUE ~ fplot)
      ) %>%
      ggplot(aes(x = fct_relevel(reorder(place, test_pos), "National estimate"), y = test_pos, shape = shape)) +
      geom_hline(yintercept = tpr_safe, color = "gray40", linetype = 2) +
      geom_hline(yintercept = tpr_danger, color = "gray40", linetype = 2) +
      geom_pointrange(aes(ymin = lower, ymax = upper, color = fplot), size = 0.4) +
      scale_color_manual(values = fplot_colors) +
      scale_shape_manual(values = c("not_india" = 16, "india" = 18)) +
      labs(
        title    = "Test-positive rate for COVID-19 in India<br>by state/union territory",
        subtitle = glue("as of {format(as.Date(today), '%B %e')}"),
        x        = "State/Union territory",
        y        = "Test-positive rate",
        caption  = glue("**\uA9 COV-IND-19 Study Group**<br>**Source:** covid19india.org<br>",
                        "**Note:**<br>",
                        " - Colored red if estimate is above {tpr_danger} and green if below {tpr_safe}.<br>",
                        " - Telangana is based on only 5 days of data.")
      ) +
      coord_flip() +
      covind19_base
  
  ga_for = arrangeGrob(
    cfr1_for + 
      theme(axis.title.y = element_blank()) + 
      labs(
        title = "a. Case-fatality rate",
        subtitle = NULL,
        caption = glue("**Notes:**<br>", 
                       " - 7-day average estimate with 95% confidence interval shown.<br>",
                       " - Colored red if estimate is above {cfr_danger} and green if below {cfr_safe}.")),
    dbl_for + 
      theme(axis.title.y = element_blank()) +
      labs(
        title = "b. Doubling time",
        subtitle = NULL,
        caption = glue("**Notes:**<br>", 
                       " - 7-day average estimate with range shown.<br>",
                       " - Colored red if estimate is below {dbl_danger} and green if above {dbl_safe}.")),
    r_est_for + 
      theme(axis.title.y = element_blank()) + 
      labs(
        title = "c. Effective reproduction number",
        subtitle = NULL,
        caption = glue("**Notes:**<br>", 
                       " - 7-day average estimate with 95% confidence interval shown.<br>",
                       " - Colored red if estimate is above {r_danger} and green if below {r_safe}.")
      ),
    tp_for +
      labs(
        title   = "d. Test-positive rate",
        subtitle = NULL,
        caption = glue("**Notes:**<br>", 
                       " - 7-day average estimate with range shown.<br>",
                       " - Colored red if estimate is above {tpr_danger} and green if below {tpr_safe}.")
      ) +
      theme(axis.title.y = element_blank()),
    ncol   = 2,
    nrow   = 2,
    top    = textGrob("COVID-19 in India Dashboard", hjust = 0, x = 0.1, gp = gpar(fontsize = 27, fontface = "bold")),
    bottom = textGrob(glue("\uA9 COV-IND-19 Study Group\n",
                           "Source: covid19india.org (data through {format(as.Date(today), '%B %e')})"),
                      hjust = 0, x = 0.1, gp = gpar(fontsize = 12))
  )
  
  return(list(
    cfr1_for = cfr1_for, dbl_for = dbl_for, 
    r_est_for = r_est_for, tp_for = tp_for, 
    ga_for = ga_for 
  ))
}

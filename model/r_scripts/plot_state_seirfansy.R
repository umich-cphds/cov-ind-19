suppressPackageStartupMessages({
library(vroom)
library(here)
library(tidyverse)
library(glue)
library(ggtext)
})

today <- Sys.getenv("today")
data_repo <- Sys.getenv("data_repo")
code_repo <- Sys.getenv("code_repo")

setwd(paste0(code_repo, "/model/r_scripts/"))
suppressPackageStartupMessages({
  library(tidyverse)
  library(magrittr)
  library(arm)
  library(janitor)
  library(DescTools)
  library(patchwork)
  library(pbapply)
  library(SEIRfansy)
})

f <- list.files(paste0(code_repo, "/model/r_scripts/functions"))
sapply(paste0("functions/", f), source)

n_date <- 61


x <- vroom(paste0(data_repo, "/", today, "/covid19india_data.csv"), col_types = cols()) %>%
  group_by(State) %>%
  filter(Date == max(Date) & State != "un" & State != "la" & State != "dd" & State != "hp" & State != "py" ) %>%
  ungroup() %>%
  top_n(20, Cases)

state_codes <- x$State
#state_codes <- gsub(".rds", "", gsub("prediction_pars_", "", lf[grepl(lf, pattern = "prediction_pars")])) # change to get_states

for (i in seq_along(state_codes)) {
  message(glue("***beep boop*** {state_codes[i]}"))
  tmp_prediction <- read_tsv(paste0(data_repo, "/", today, "/seirfansy/prediction_", # change to data repo
                                    tolower(state_codes[i]), ".txt"),
                             col_types = cols()) %>%
    dplyr::filter(pred == 1) %>%                                           # drop in future
    dplyr::rowwise(state, section, date, pred) %>%                         # drop in future
    dplyr::summarize(mean = mean(dplyr::c_across()), .groups = "drop") %>% # drop in future
    arrange(date)
  
  tmp_p_pred <- tmp_prediction %>%
    dplyr::filter(section == "positive_reported") %>%
    arrange(date) %>%
    slice_head(n = n_date)
  
  tmp_r_pred <- tmp_prediction %>%
    dplyr::filter(section == "recovered_reported") %>%
    arrange(date) %>%
    slice_head(n = n_date)
  
  tmp_d_pred <- tmp_prediction %>%
    dplyr::filter(section == "death_reported") %>%
    arrange(date) %>%
    slice_head(n = n_date)
  
  tmp_total_rep_case <- tmp_p_pred$mean + tmp_r_pred$mean + tmp_d_pred$mean
  tmp_total_rep_death <- tmp_d_pred$mean
  
  tmp_daily_rep_case <- my_diff(tmp_total_rep_case)
  tmp_daily_rep_death <- my_diff(tmp_total_rep_death)
  
  if (i == 1) {
    
    case_dat <- tibble(
      state = tmp_p_pred$state,
      date  = tmp_p_pred$date,
      value = tmp_daily_rep_case
    )
    
    death_dat <- tibble(
      state = tmp_p_pred$state,
      date  = tmp_p_pred$date,
      value = tmp_daily_rep_death
    )
    
  } else {
    
    case_dat <- bind_rows(
      case_dat,
      tibble(
        state = tmp_p_pred$state,
        date  = tmp_p_pred$date,
        value = tmp_daily_rep_case
      )
    )
    
    death_dat <- bind_rows(
      death_dat,
      tibble(
        state = tmp_p_pred$state,
        date  = tmp_p_pred$date,
        value = tmp_daily_rep_death
      )
    )
    
  }
  
}

super_special_line_plot <- function(dat, title) {
  
  dat %>%
    ggplot(aes(x = date, y = value, color = state, group = state)) +
    geom_line(size = 1) +
    labs(
      title = title,
      x     = "Date",
      y     = "Predicted counts"
    ) +
    scale_y_continuous(labels = addUnits) +
    final_theme +
    guides(size = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 2)))
  
}

case_plt <- case_dat %>%
  super_special_line_plot(title = "Projected daily new cases")

death_plt <- death_dat %>%
  super_special_line_plot(title = "Projected daily deaths") 

gA <- ggplotGrob(case_plt + theme(legend.position = "none"))
gB <- ggplotGrob(death_plt)

cairo_pdf(filename = paste0(data_repo, "/", today, "/seirfansy/prediction_stackplot.pdf"), # change output path
          width = 8, height = 10)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))
dev.off()

case_dat %>%
  ggplot(aes(x = date, y = value, group = state)) +
  geom_line(size = 1, color = "#36A30B") +
  geom_point(color = "black", shape = 3, size = 0.25) +
  facet_wrap(~state) +
  labs(
    title    = "Predicted daily cases",
    subtitle = "60 days using data through April 27",
    x        = "Date",
    y        = "Predicted counts",
    caption  = "**\UA9 COV-IND-19 Study Group**"
  ) +
  scale_x_date(date_labels = "%b %e") +
  scale_y_continuous(labels = scales::comma) +
  final_theme +
  theme(
    strip.text.x       = element_text(size=10, face="bold", hjust = 0, color = '#36A30B'),
    strip.background   = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border       = element_blank(),
    axis.title.x       = element_text(size = 12),
    axis.title.y       = element_text(size = 12),
    axis.text.y        = element_text(size = 10),
    axis.text.x        = element_text(size = 10, angle = 20),
    legend.position    = "none",
    plot.caption       = element_markdown(hjust = 0)
  )
ggsave(paste0(data_repo, "/", today, "/seirfansy/prediction_casegrid.pdf"), width = 10, height = 8, device = cairo_pdf)  # change output path

death_dat %>%
  ggplot(aes(x = date, y = value, group = state)) +
  geom_line(size = 1, color = "#36A30B") +
  geom_point(color = "black", shape = 3, size = 0.25) +
  facet_wrap(~state) +
  labs(
    title    = "Predicted daily deaths",
    subtitle = "60 days using data through April 27",
    x        = "Date",
    y        = "Predicted counts",
    caption  = "**\UA9 COV-IND-19 Study Group**"
  ) +
  scale_x_date(date_labels = "%b %e") +
  scale_y_continuous(labels = scales::comma) +
  final_theme +
  theme(
    strip.text.x       = element_text(size=10, face="bold", hjust = 0, color = '#36A30B'),
    strip.background   = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border       = element_blank(),
    axis.title.x       = element_text(size = 12),
    axis.title.y       = element_text(size = 12),
    axis.text.y        = element_text(size = 10),
    axis.text.x        = element_text(size = 10, angle = 20),
    legend.position    = "none",
    plot.caption       = element_markdown(hjust = 0)
  )
ggsave(paste0(data_repo, "/", today, "/seirfansy/prediction_deathgrid.pdf"), width = 12, height = 8, device = cairo_pdf)  # change output path

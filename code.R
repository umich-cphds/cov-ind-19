##### dependencies #####
require(tidyverse)
require(here)
require(janitor)
require(glue)
require(EpiEstim)
require(gghighlight)
require(simpleboot)
require(gt)
require(gridExtra)
require(grid)
require(lmPerm)
require(ggrepel)
require(coin)

##### cleaning #####
date <- "2020-05-12"
set_seed <- 46342
set.seed(set_seed)

f_col <- "#e01d96" # color for female countries
m_col <- "#00274C" # color for male countries
b_rep <- 100000     # number of bootstrap iterations

# set ggplot theme
fhos_theme <-   theme_minimal() +
  theme(
    text               = element_text(family = "Helvetica Neue"),
    plot.title         = ggtext::element_markdown(size = 16, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0, size = 12, lineheight = 1.1),
    axis.text          = element_text(size = 16, color = "#36454f"),
    strip.text.x       = element_text(size = 16, face = "bold", hjust = 0, color = '#36A30B'),
    axis.title         = element_text(size = 16, face = "italic"),
    legend.position    = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# source: https://en.wikipedia.org/wiki/List_of_elected_and_appointed_female_heads_of_state_and_government
women <- tibble::tribble(
  ~Name,                                                                                                                             ~Portrait,              ~Country,            ~Office,      ~Mandate.start, ~Mandate.end,         ~Term.length,   ~Head.of.state.or.government,
  "Angela Merkel",                                                                                           "Angela Merkel. Tallinn Digital Summit.jpg",             "Germany",       "Chancellor",  "22 November 2005",  "Incumbent", "14 years, 177 days",           "Head of government",
  "Sheikh Hasina",                                                                "Sheikh Hasina, Honourable Prime Minister of Bangladesh (cropped).jpg",          "Bangladesh",   "Prime Minister",    "6 January 2009",  "Incumbent", "11 years, 132 days",           "Head of government",
  "Erna Solberg",                                                                                                     "Erna Solberg 2015-01-20 001.jpg",              "Norway",   "Prime Minister",   "16 October 2013",  "Incumbent",  "6 years, 214 days",           "Head of government",
  "Saara Kuugongelwa",                                                                                                 "2020 Saara Kuugongelwa-Amadhila.jpg",             "Namibia",   "Prime Minister",     "21 March 2015",  "Incumbent",   "5 years, 57 days",                            "*",
  "Bidhya Devi Bhandari",                                                                                                                "Vidhya Bhandari2.JPG",               "Nepal",        "President",   "29 October 2015",  "Incumbent",  "4 years, 201 days",                "Head of state",
  "Aung San Suu Kyi",                                                        "Remise du Prix Sakharov à Aung San Suu Kyi Strasbourg 22 octobre 2013-18.jpg",             "Myanmar", "State Counsellor",      "6 April 2016",  "Incumbent",   "4 years, 41 days",                            "*",
  "Tsai Ing-wen",                                                                                                                      "蔡英文官方元首肖像照.png",              "Taiwan",        "President",       "20 May 2016",  "Incumbent",  "3 years, 363 days", "Head of state and government",
  "Kersti Kaljulaid", "Tallinn Digital Summit opening address by Kersti Kaljulaid, President of the Republic of Estonia Kersti Kaljulaid (37130700010).jpg",             "Estonia",        "President",   "10 October 2016",  "Incumbent",  "3 years, 220 days",                "Head of state",
  "Ana Brnabić",                                                                                                       "Ana Brnabic, July 3, 2018.jpg",              "Serbia",   "Prime Minister",      "29 June 2017",  "Incumbent",  "2 years, 323 days",           "Head of government",
  "Halimah Yacob",                                                                             "Halimah Yacob APEC Women and the Economy Forum 2012.jpg",           "Singapore",        "President", "14 September 2017",  "Incumbent",  "2 years, 246 days",                "Head of state",
  "Jacinda Ardern",                                                                                                  "Jacinda Ardern, 2017 (cropped).jpg",         "New Zealand",   "Prime Minister",   "26 October 2017",  "Incumbent",  "2 years, 204 days",           "Head of government",
  "Katrín Jakobsdóttir",                                                                                                   "Katrín Jakobsdóttir (cropped).jpg",             "Iceland",   "Prime Minister",  "30 November 2017",  "Incumbent",  "2 years, 169 days",           "Head of government",
  "Paula-Mae Weekes",                                       "Official portrait of HE Paula-Mae Weekes President of the Republic of Trinidad and Tobago.jpg", "Trinidad and Tobago",        "President",     "19 March 2018",  "Incumbent",   "2 years, 59 days",                "Head of state",
  "Mia Mottley",                                                                                                                "2019 Mia Mottley.jpg",            "Barbados",   "Prime Minister",       "25 May 2018",  "Incumbent",   "1 year, 358 days",           "Head of government",
  "Sahle-Work Zewde",                                                                                                                "Sahle-Work Zewde.jpg",            "Ethiopia",        "President",   "25 October 2018",  "Incumbent",   "1 year, 205 days",                "Head of state",
  "Salome Zurabishvili",                                                                                           "Salome Zurabishvili in 2018 (cropped).jpg",             "Georgia",        "President",  "16 December 2018",  "Incumbent",   "1 year, 153 days",                "Head of state",
  "Zuzana Čaputová",                                                                                                  "Zuzana Čaputová (20.6.2019)VII.jpg",            "Slovakia",        "President",      "15 June 2019",  "Incumbent",           "337 days",                "Head of state",
  "Mette Frederiksen",                                   "20190614 Folkemodet Bornholm Mette Frederiksen Socialdemokratiet 0285 (48063468172) (cropped).jpg",             "Denmark",   "Prime Minister",      "27 June 2019",  "Incumbent",           "325 days",           "Head of government",
  "Sophie Wilmès",                                                                                                    "Sophie Wilmès 2020 (cropped).jpg",             "Belgium",   "Prime Minister",   "27 October 2019",  "Incumbent",           "203 days",           "Head of government",
  "Jeanine Áñez",                                                                                                               "Jeanine Áñez 2016.png",             "Bolivia",        "President",  "12 November 2019",  "Incumbent",           "187 days", "Head of state and government",
  "Sanna Marin",                                                                            "Prime Minister of Finland Sanna Marin 2019 (cropped).jpg",             "Finland",   "Prime Minister",  "10 December 2019",  "Incumbent",           "159 days",           "Head of government",
  "Katerina Sakellaropoulou",                                                                                                        "Katerina Sakellaropoulou.jpg",              "Greece",        "President",     "13 March 2020",  "Incumbent",            "65 days",                "Head of state"
) %>%
  clean_names()

women_countries <- women %>% pull(country)

case_restrict <- 100

# load data ----------
dat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = cols()) %>%
  dplyr::rename(state = `Province/State`, name = `Country/Region`) %>%
  dplyr::select(-c(Lat, Long, state)) %>%
  group_by(name) %>%
  summarize_all(list(sum)) %>%
  ungroup() %>%
  pivot_longer(names_to = "date", values_to = "cases", -name) %>%
  mutate(
    date = as.Date(date, "%m/%d/%y"),
    fhos = as.numeric(name %in% women_countries)
  )

# vector of countries with more than case_restrict total cases (default: 500) ----------
keepr <- dat %>%
  group_by(name) %>%
  filter(cases == max(cases)) %>%
  filter(cases >= case_restrict) %>%
  pull(name) %>%
  unique()

# prep data -----------
dat <- dat %>%
  filter(name %in% keepr) %>%
  group_by(name) %>%
  arrange(date) %>%
  dplyr::rename(tot_cases = cases) %>%
  mutate(
    cases = tot_cases - dplyr::lag(tot_cases)
  ) %>%
  drop_na(cases) %>%
  ungroup() %>%
  filter(tot_cases > 50) %>%
  group_by(name) %>%
  arrange(date) %>%
  mutate(
    day = seq(n())
  ) %>%
  ungroup() %>%
  filter(!(cases <=0 | is.na(cases)))

add_drop <- names(table(dat$name)[table(dat$name) < 10])
dat      <- dat %>% filter(!(name %in% c(add_drop, "Diamond Princess")))


#### stop at June 04

dat <- dat %>% filter(date <= "2020-06-03")

# create time-varying R function ----------
estR0_out <- function(dat) {
  
  t_start <- seq(2, nrow(dat) - 4)
  t_end   <- t_start + 4
  
  # from EpiEstim
  res <- estimate_R(
    incid = dat$cases,
    method = "parametric_si",
    config = make_config(list(
      mean_si             = 7,
      std_si              = 4.5,
      si_parametric_distr = "G",
      t_start             = t_start,
      t_end               = t_end,
      seed                = set_seed))
  ) 
  
  tibble(
    date_num = res$dates
  ) %>% left_join(
    res$R, by = c("date_num" = "t_end")
  ) %>%
    dplyr::select(
      date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
    ) %>%
    add_column(date = dat$date) %>%
    dplyr::select(-date_num) %>%
    select(date, everything())
}



# time-varying R
# apply time-varying R function to all countries ----------
options(warn = -1)

r0_est <- dat %>%
  dplyr::select(date, cases, name) %>%
  nest(data = c(-name)) %>%
  mutate(
    estR0 = map(data, ~estR0_out(dat = .x))
  ) %>%
  unnest(estR0) %>%
  dplyr::select(-data)

options(warn = 1)

r0_est <- r0_est %>% 
  mutate(fhos = as.factor(as.numeric(name %in% women_countries))) %>%
  left_join(
    dat %>% dplyr::select(name, date, day), by = c("name", "date")
  )

#### comparison of r0_last

# save estimates for last day by country ----------
r0_last <- r0_est %>%
  group_by(name) %>%
  filter(date == max(date)) %>% 
  ungroup()





# calculate median R by country -----------
r0_med <- r0_est %>%
  group_by(fhos, day) %>%
  summarise(r = median(r, na.rm = T)) %>%
  ungroup() %>%
  filter(date >= "2020-03-01") %>%
  mutate(
    fhos = as.factor(fhos)
  )

# plot time-varying R by country ----------
ggsave("final/fig1.tiff", 
       r0_est %>%
         drop_na() %>%
         filter(date >= "2020-03-01") %>%
         ggplot(aes(x = day, y = r), group = name) +
         geom_hline(yintercept = 1, linetype = 2, color = "gray40", size = 1) +
         geom_line(aes(group = name, color = fhos), size = 1, alpha = 0.15) +
         geom_smooth(data = r0_med, aes(x = day, y = r, group = fhos, color = fhos), 
                     size = 1.5, 
                     linetype = 1, 
                     method = "loess", 
                     span = 0.5, 
                     formula = "y ~ x", 
                     se = FALSE) +
         labs(
           #title    = "Time varying R(t) for COVID-19 by country",
           #subtitle = glue("As of {format(max(r0_last$date), '%B %e')}."),
           x        = "Days since 50 cumulative cases",
           y        = "Time varying effective reproduction number (R)",
           caption  = glue("**\uA9 CPHDS**<br>**Source:** JHU CSSE GitHub<br>",
                           "**Notes:**<br>",
                           " - Dashed line is R = 1 threshold.<br>", 
                           " - <B><span style='color:#e01d96'>Pink lines represent countries headed by women.</span></B> and <B><span style='color:#00274C'>Blue lines represent countries headed by men.</span></B><br>",
                           " - Bolded lines represent loess-smoothed <B>median</B> time-varying R value, stratified by sex of head of state.<br>",
                           " - Includes {length(unique(r0_last$name))} countries: {length(unique(r0_last$name[r0_last$fhos == 1]))} and {length(unique(r0_last$name[r0_last$fhos == 0]))} with women and men heads of state, respectively. Restricted to countries with at least {case_restrict} cases.")
         ) +
         scale_color_manual(values = c("1" = f_col, "0" = m_col), 
                            name = "Sex of head of state", 
                            labels = c("Men", "Women")) +
         fhos_theme , 
       height = 768/4,
       width = 1366/4, 
       units = "mm",
       dpi = 300)



f2a <- r0_est %>%
  drop_na() %>%
  group_by(name) %>%
  summarize(
    r = mean(r, na.rm = TRUE)
  ) %>%
  mutate(
    fhos = as.factor(as.numeric(name %in% women_countries))
  ) %>%
  ggplot(aes(x = r, fill = fhos)) +
  geom_density(alpha = 0.3, size = 0) +
  scale_color_manual(values = c("1" = f_col, "0" = m_col)) + 
  labs(
    title    = "(A)",
    #subtitle = glue("As of {format(max(r0_last$date), '%B %e')}."),
    x        = "Mean of time varying effective reproduction number",
    y        = "Density",
    caption  = glue("**\uA9 CPHDS**<br>**Source:** JHU CSSE GitHub<br>",
                    "**Note:** <br>", 
                    "- Includes {length(unique(r0_last$name))} countries: {length(unique(r0_last$name[r0_last$fhos == 1]))} and {length(unique(r0_last$name[r0_last$fhos == 0]))} with <B><span style='color:#e01d96'>women</span></B> and <B><span style='color:#00274C'>men</span></B> heads of state, respectively.<br>", 
                    "- Dotted lines indicate median values of distribution of mean R, stratified by sex of head of state.<br>")
  ) + 
  geom_vline(data = r0_est %>%
               drop_na() %>%
               group_by(name) %>%
               summarize(
                 r = mean(r, na.rm = TRUE)
               ) %>%
               mutate(
                 fhos = as.factor(as.numeric(name %in% women_countries))
               ) %>%
               group_by(fhos) %>% 
               summarize(m = median(r)), 
             aes(xintercept = m, color = fhos), 
             linetype = "dashed") + 
  geom_label_repel(data = r0_est %>%
                     drop_na() %>%
                     group_by(name) %>%
                     summarize(
                       r = mean(r, na.rm = TRUE)
                     ) %>%
                     mutate(
                       fhos = as.factor(as.numeric(name %in% women_countries))
                     ) %>%
                     group_by(fhos) %>% 
                     summarize(m = median(r)), 
                   aes(x = m, 
                       y = c(0.9, 0.6), 
                       label = c("Median of distribution: 1.42", 
                                 "Median of distribution: 1.23"), 
                       fill = NULL, 
                       color = fhos), 
                   hjust = c(-1.25, -1.25),
                   show_guide = F) + 
  # annotate("text", label = glue("Median of distribution for countries with\nfemale heads of state: {round(1.23, 2)}"),
  #          x = 2, y = 0.4, hjust = 0, color = "#e01d96", fontface = "bold") +
  # annotate("text", label = glue("Median of distribution for countries with\nmale heads of state: {round(1.43, 2)}"),
  #           x = 2, y = 0.6, hjust = 0, color = "#36454f", fontface = "bold") + 
  fhos_theme + 
  scale_fill_manual(values = c("1" = f_col, "0" = m_col),
                    labels = c("Men", "Women"),
                    name = "Sex of head of state")  +
  guides(colour=FALSE)

f2b <- r0_est %>%
  drop_na() %>%
  group_by(name) %>%
  summarize(
    r = median(r, na.rm = TRUE)
  ) %>%
  mutate(
    fhos = as.factor(as.numeric(name %in% women_countries))
  ) %>%
  ggplot(aes(x = r, fill = fhos)) +
  geom_density(alpha = 0.3, size = 0) +
  scale_color_manual(values = c("1" = f_col, "0" = m_col)) + 
  labs(
    title    = "(B)",
    #subtitle = glue("As of {format(max(r0_last$date), '%B %e')}."),
    x        = "Median of time varying effective reproduction number",
    y        = "Density",
    caption  = glue("**\uA9 CPHDS**<br>**Source:** JHU CSSE GitHub<br>",
                    "**Note:** <br>", 
                    "- Includes {length(unique(r0_last$name))} countries: {length(unique(r0_last$name[r0_last$fhos == 1]))} and {length(unique(r0_last$name[r0_last$fhos == 0]))} with <B><span style='color:#e01d96'>women</span></B> and <B><span style='color:#00274C'>men</span></B> heads of state, respectively.<br>", 
                    "- Dotted lines indicate median values of distribution of median R, stratified by sex of head of state.<br>")
  ) + 
  geom_vline(data = r0_est %>%
               drop_na() %>%
               group_by(name) %>%
               summarize(
                 r = median(r, na.rm = TRUE)
               ) %>%
               mutate(
                 fhos = as.factor(as.numeric(name %in% women_countries))
               ) %>%
               group_by(fhos) %>% 
               summarize(m = median(r)), 
             aes(xintercept = m, color = fhos), 
             linetype = "dashed") + 
  geom_label_repel(data = r0_est %>%
                     drop_na() %>%
                     group_by(name) %>%
                     summarize(
                       r = median(r, na.rm = TRUE)
                     ) %>%
                     mutate(
                       fhos = as.factor(as.numeric(name %in% women_countries))
                     ) %>%
                     group_by(fhos) %>% 
                     summarize(m = median(r)), 
                   aes(x = m, 
                       y = c(0.9, 0.6), 
                       label = c("Median of distribution: 1.14", 
                                 "Median of distribution: 0.893"), 
                       fill = NULL, 
                       color = fhos), 
                   hjust = c(-1.45, -1.45),
                   show_guide = F) + 
  # annotate("text", label = glue("Median of distribution for countries with\nfemale heads of state: {round(1.23, 2)}"),
  #          x = 2, y = 0.4, hjust = 0, color = "#e01d96", fontface = "bold") +
  # annotate("text", label = glue("Median of distribution for countries with\nmale heads of state: {round(1.43, 2)}"),
  #           x = 2, y = 0.6, hjust = 0, color = "#36454f", fontface = "bold") + 
  fhos_theme + 
  scale_fill_manual(values = c("1" = f_col, "0" = m_col),
                    labels = c("Men", "Women"),
                    name = "Sex of head of state")  +
  guides(colour=FALSE)

f2c <- r0_est %>%
  drop_na() %>%
  group_by(name) %>%
  summarize(
    r = max(r, na.rm = TRUE)
  ) %>%
  mutate(
    fhos = as.factor(as.numeric(name %in% women_countries))
  ) %>%
  ggplot(aes(x = r, fill = fhos)) +
  geom_density(alpha = 0.3, size = 0) +
  scale_color_manual(values = c("1" = f_col, "0" = m_col)) + 
  labs(
    title    = "(C)",
    #subtitle = glue("As of {format(max(r0_last$date), '%B %e')}."),
    x        = "Maximum of time varying effective reproduction number",
    y        = "Density",
    caption  = glue("**\uA9 CPHDS**<br>**Source:** JHU CSSE GitHub<br>",
                    "**Note:** <br>", 
                    "- Includes {length(unique(r0_last$name))} countries: {length(unique(r0_last$name[r0_last$fhos == 1]))} and {length(unique(r0_last$name[r0_last$fhos == 0]))} with <B><span style='color:#e01d96'>women</span></B> and <B><span style='color:#00274C'>men</span></B> heads of state, respectively.<br>", 
                    "- Dotted lines indicate median values of distribution of maximum R, stratified by sex of head of state.<br>")
  ) + 
  geom_vline(data = r0_est %>%
               drop_na() %>%
               group_by(name) %>%
               summarize(
                 r = max(r, na.rm = TRUE)
               ) %>%
               mutate(
                 fhos = as.factor(as.numeric(name %in% women_countries))
               ) %>%
               group_by(fhos) %>% 
               summarize(m = median(r)), 
             aes(xintercept = m, color = fhos), 
             linetype = "dashed") + 
  geom_label_repel(data = r0_est %>%
                     drop_na() %>%
                     group_by(name) %>%
                     summarize(
                       r = max(r, na.rm = TRUE)
                     ) %>%
                     mutate(
                       fhos = as.factor(as.numeric(name %in% women_countries))
                     ) %>%
                     group_by(fhos) %>% 
                     summarize(m = median(r)), 
                   aes(x = m, 
                       y = c(0.165, 0.125), 
                       label = c("Median of distribution: 4.35", 
                                 "Median of distribution: 4.61"), 
                       fill = NULL, 
                       color = fhos), 
                   hjust = c(-1.45, -1.45),
                   show_guide = F) + 
  # annotate("text", label = glue("Median of distribution for countries with\nfemale heads of state: {round(1.23, 2)}"),
  #          x = 2, y = 0.4, hjust = 0, color = "#e01d96", fontface = "bold") +
  # annotate("text", label = glue("Median of distribution for countries with\nmale heads of state: {round(1.43, 2)}"),
  #           x = 2, y = 0.6, hjust = 0, color = "#36454f", fontface = "bold") + 
  fhos_theme + 
  scale_fill_manual(values = c("1" = f_col, "0" = m_col),
                    labels = c("Men", "Women"),
                    name = "Sex of head of state")  +
  guides(colour=FALSE)


f2d <- r0_est %>%
  drop_na() %>%
  group_by(name) %>%
  filter(
    date == max(date)
  ) %>% 
  ungroup() %>% 
  mutate(
    fhos = as.factor(as.numeric(name %in% women_countries))
  ) %>%
  ggplot(aes(x = r, fill = fhos)) +
  geom_density(alpha = 0.3, size = 0) +
  scale_color_manual(values = c("1" = f_col, "0" = m_col)) + 
  labs(
    title    = "(D)",
    #subtitle = glue("As of {format(max(r0_last$date), '%B %e')}."),
    x        = "Most recent (June 3) time varying effective reproduction number",
    y        = "Density",
    caption  = glue("**\uA9 CPHDS**<br>**Source:** JHU CSSE GitHub<br>",
                    "**Note:** <br>", 
                    "- Includes {length(unique(r0_last$name))} countries: {length(unique(r0_last$name[r0_last$fhos == 1]))} and {length(unique(r0_last$name[r0_last$fhos == 0]))} with <B><span style='color:#e01d96'>women</span></B> and <B><span style='color:#00274C'>men</span></B> heads of state, respectively.<br>", 
                    "- Dotted lines indicate median values of distribution of R (on June 3), stratified by sex of head of state.<br>")
  ) + 
  geom_vline(data = r0_est %>%
               drop_na() %>%
               group_by(name) %>%
               filter(date == max(date)) %>%
               ungroup() %>% 
               mutate(
                 fhos = as.factor(as.numeric(name %in% women_countries))
               ) %>%
               group_by(fhos) %>% 
               summarize(m = median(r)), 
             aes(xintercept = m, color = fhos), 
             linetype = "dashed") + 
  geom_label_repel(data = r0_est %>%
                     drop_na() %>%
                     group_by(name) %>%
                     filter(
                       date == max(date)
                     ) %>% 
                     ungroup() %>% 
                     mutate(
                       fhos = as.factor(as.numeric(name %in% women_countries))
                     ) %>%
                     group_by(fhos) %>% 
                     summarize(m = median(r)), 
                   aes(x = m, 
                       y = c(0.7, 0.5), 
                       label = c("Median of distribution: 1.05", 
                                 "Median of distribution: 0.790"), 
                       fill = NULL, 
                       color = fhos), 
                   hjust = c(-1.15, -1.15),
                   show_guide = F) + 
  # annotate("text", label = glue("Median of distribution for countries with\nfemale heads of state: {round(1.23, 2)}"),
  #          x = 2, y = 0.4, hjust = 0, color = "#e01d96", fontface = "bold") +
  # annotate("text", label = glue("Median of distribution for countries with\nmale heads of state: {round(1.43, 2)}"),
  #           x = 2, y = 0.6, hjust = 0, color = "#36454f", fontface = "bold") + 
  fhos_theme + 
  scale_fill_manual(values = c("1" = f_col, "0" = m_col),
                    labels = c("Men", "Women"),
                    name = "Sex of head of state")  +
  guides(colour=FALSE)

ggsave("final/fig2.tiff", 
       grid.arrange(f2a, f2b, f2c, f2d, ncol = 2, nrow = 2), 
       height=768/2.5, 
       width = 1366/2.5, 
       units = "mm", 
       limitsize = FALSE,
       dpi = 300)





bstrap_dat <- r0_est %>%
  drop_na() %>%
  group_by(name) %>%
  summarize(
    r = median(r, na.rm = TRUE)
  ) %>%
  mutate(
    fhos = as.factor(as.numeric(name %in% women_countries))
  )
median_boot <- two.boot(
  bstrap_dat %>% filter(fhos == 1) %>% pull(r),
  bstrap_dat %>% filter(fhos == 0) %>% pull(r),
  median,
  b_rep
)
median_boot_dat           <-  tibble(median_boot$t)
colnames(median_boot_dat) <- "mean_diffs"
p <- pvalue(oneway_test(r~fhos,
                        data = bstrap_dat,
                        distribution = approximate(nresample = 1e+6)))[1]
round(c(median_boot$t0,
        quantile(median_boot_dat$mean_diffs, 0.025),
        quantile(median_boot_dat$mean_diffs, 0.975),
        p), 3)





bstrap_dat <- r0_est %>%
  drop_na() %>%
  group_by(name) %>%
  summarize(
    r = mean(r, na.rm = TRUE)
  ) %>%
  mutate(
    fhos = as.factor(as.numeric(name %in% women_countries))
  )
median_boot <- two.boot(
  bstrap_dat %>% filter(fhos == 1) %>% pull(r),
  bstrap_dat %>% filter(fhos == 0) %>% pull(r),
  median,
  b_rep
)
median_boot_dat           <-  tibble(median_boot$t)
colnames(median_boot_dat) <- "mean_diffs"
p <- pvalue(oneway_test(r~fhos,
                        data = bstrap_dat,
                        distribution = approximate(nresample = 1e+6)))[1]
round(c(median_boot$t0,
        quantile(median_boot_dat$mean_diffs, 0.025),
        quantile(median_boot_dat$mean_diffs, 0.975),
        p), 3)












bstrap_dat <- r0_est %>%
  drop_na() %>%
  group_by(name) %>%
  summarize(
    r = max(r, na.rm = TRUE)
  ) %>%
  mutate(
    fhos = as.factor(as.numeric(name %in% women_countries))
  )
median_boot <- two.boot(
  bstrap_dat %>% filter(fhos == 1) %>% pull(r),
  bstrap_dat %>% filter(fhos == 0) %>% pull(r),
  median,
  b_rep
)
median_boot_dat           <-  tibble(median_boot$t)
colnames(median_boot_dat) <- "mean_diffs"
p <- pvalue(oneway_test(r~fhos,
                        data = bstrap_dat,
                        distribution = approximate(nresample = 1e+6)))[1]
round(c(median_boot$t0,
        quantile(median_boot_dat$mean_diffs, 0.025),
        quantile(median_boot_dat$mean_diffs, 0.975),
        p), 3)






bstrap_dat <- r0_est %>%
  drop_na() %>%
  group_by(name) %>%
  filter(date == max(date)) %>%
  mutate(
    fhos = as.factor(as.numeric(name %in% women_countries))
  )
median_boot <- two.boot(
  bstrap_dat %>% filter(fhos == 1) %>% pull(r),
  bstrap_dat %>% filter(fhos == 0) %>% pull(r),
  median,
  b_rep
)
median_boot_dat           <-  tibble(median_boot$t)
colnames(median_boot_dat) <- "mean_diffs"
p <- pvalue(oneway_test(r~fhos,
                        data = bstrap_dat,
                        distribution = approximate(nresample = 1e+6)))[1]
round(c(median_boot$t0,
        quantile(median_boot_dat$mean_diffs, 0.025),
        quantile(median_boot_dat$mean_diffs, 0.975),
        p), 3)


dbl_timr <- function(data, end_date = NULL, time = 7) {
  
  if (is.null(end_date)) {
    end_date <- max(data$date)
  }
  
  start <-  data %>% filter(date == as.Date(as.Date(end_date) - time)) %>% pull(tot_cases)
  
  if (length(start) == 0) {
    NA
  } else if (start == 0) {
    NA
  } else {
    end   <- data %>% filter(date == as.Date(end_date)) %>% pull(tot_cases)
    
    r <- ((end - start) / start) * 100
    
    time * (log(2) / log(1 + (r / 100)))
  }
}

dbl_times <- NA
for (i in seq_along(unique(dat$name))) {
  tmp <- dat %>%
    filter(name == unique(dat$name)[i]) %>%
    arrange(date)
  
  tmp_v     <- NA
  
  for (j in seq_along(tmp$date)) {
    tmp_v[j] <- dbl_timr(data = tmp, end_date = tmp$date[j], time = 7)
  }
  
  if (i == 1) {
    dbl_times <- tmp_v
  } else {
    dbl_times <- c(dbl_times, tmp_v)
  }
}


dat <- dat %>%
  add_column(
    dbl = dbl_times
  )

dt_last <- dat %>%
  group_by(name) %>%
  filter(date == max(date)) 


bstrap_dat <- dat %>% 
  select(name, dbl) %>% 
  group_by(name) %>% 
  summarize(dt = median(dbl, na.rm = TRUE)) %>% 
  mutate(
    fhos = as.factor(as.numeric(name %in% women_countries))
  ) %>% 
  group_by(fhos) %>% 
  summarize(median(dt))


median_boot <- two.boot(
  bstrap_dat %>% filter(fhos == 1) %>% pull(dt),
  bstrap_dat %>% filter(fhos == 0) %>% pull(dt),
  median,
  b_rep
)

median_boot_dat           <-  tibble(median_boot$t)
colnames(median_boot_dat) <- "mean_diffs"

p <- pvalue(oneway_test(dt~fhos,
                        data = bstrap_dat,
                        distribution = approximate(nresample = 1e+6)))[1]

round(c(median_boot$t0,
        quantile(median_boot_dat$mean_diffs, 0.025),
        quantile(median_boot_dat$mean_diffs, 0.975),
        p), 3)


# case-fatality rate -----------
CFR <- function(C,D) {
  cfr       <- D / C
  cfr_logit <- log(cfr) - log(1 - cfr)
  sd_logit  <- sqrt(C / (D * (C - D)))
  
  lower_logit <- cfr_logit - qnorm(0.975) * sd_logit
  upper_logit <- cfr_logit + qnorm(0.975) * sd_logit
  
  upper <- exp(upper_logit) / (1 + exp(upper_logit))
  lower <- exp(lower_logit) / (1 + exp(lower_logit))
  
  return(c(cfr, upper, lower))
}

cfr_cases <- dat %>%
  group_by(name) %>%
  filter(date ==  "2020-06-03") %>%
  ungroup()


cfr_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", col_types = cols()) %>%
  dplyr::rename(state = `Province/State`, name = `Country/Region`) %>%
  dplyr::select(-c(Lat, Long, state)) %>%
  group_by(name) %>%
  summarize_all(list(sum)) %>%
  ungroup() %>%
  pivot_longer(names_to = "date", values_to = "deaths", -name) %>%
  mutate(
    date = as.Date(date, "%m/%d/%y"),
    fhos = as.numeric(name %in% women_countries)
  ) %>%
  group_by(name) %>%
  filter(date == "2020-06-03") %>%
  ungroup() %>%
  dplyr::select(-fhos)

cfr_dat <- cfr_cases %>%
  left_join(
    cfr_deaths,
    by = "name"
  )

test_data <- structure(list(name  = cfr_dat$name,
                            cfr   = rep(0,nrow(cfr_dat)),
                            upper = rep(0,nrow(cfr_dat)),
                            lower = rep(0,nrow(cfr_dat))),
                       class = c("tbl_df", "tbl", "data.frame"),
                       row.names = c(NA, -nrow(cfr_dat)))

for (i in 1:nrow(test_data)){
  C <- cfr_dat$tot_cases[i]
  D <- cfr_dat$deaths[i]
  
  result <- CFR(C,D)
  
  test_data$cfr[i]   <- result[1]
  test_data$upper[i] <- result[2]
  test_data$lower[i] <- result[3]
}

test_data <- test_data %>% 
  mutate(fhos = as.factor(ifelse(name %in% women_countries, 1, 0)))

require(tidyverse)
require(vroom)

helper <- function(x){
  max(c(100, x))
}


data <- vroom("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/2020-05-30/global_testing.csv") %>% 
  select(location, date, total_tests, total_cases, population) %>% 
  group_by(location) %>% 
  drop_na() %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  mutate(fhos = as.factor(ifelse(location %in% women_countries, 1, 0))) %>%
  mutate(ppt = 100*total_tests/population, 
         pp = 100*total_cases/total_tests) %>% 
  rowwise() %>%
  mutate(pp = min(100, pp))


data %>% 
  group_by(fhos) %>% 
  summarize(Tests.median = median(total_tests), 
            Cases.median = median(total_cases), 
            PPT = median(ppt), 
            TPR = median(pp))


1data %>% 
  group_by(fhos) %>% 
  summarize(median(ppt), 
            median(pp))


median_boot <- two.boot(
  r0_last %>% filter(fhos == 1) %>% pull(r),
  r0_last %>% filter(fhos == 0) %>% pull(r),
  median,
  b_rep
)
median_boot_dat           <-  tibble(median_boot$t)
colnames(median_boot_dat) <- "mean_diffs"
round(c(median_boot$t0,
        quantile(median_boot_dat$mean_diffs, 0.025),
        quantile(median_boot_dat$mean_diffs, 0.975),
        pvalue(oneway_test(r~fhos,
                           data = r0_last,
                           distribution = approximate(nresample = 1e+6)))[1]), 3)


median_boot <- two.boot(
  data %>% filter(fhos == 1) %>% pull(ppt),
  data %>% filter(fhos == 0) %>% pull(ppt),
  median,
  b_rep
)
median_boot_dat           <-  tibble(median_boot$t)
colnames(median_boot_dat) <- "mean_diffs"
round(c(median_boot$t0,
        quantile(median_boot_dat$mean_diffs, 0.025),
        quantile(median_boot_dat$mean_diffs, 0.975),
        pvalue(oneway_test(ppt~fhos,
                           data = data,
                           distribution = approximate(nresample = 1e+6)))[1]), 3)


median_boot <- two.boot(
  data %>% filter(fhos == 1) %>% pull(pp),
  data %>% filter(fhos == 0) %>% pull(pp),
  median,
  b_rep
)
median_boot_dat           <-  tibble(median_boot$t)
colnames(median_boot_dat) <- "mean_diffs"
round(c(median_boot$t0,
        quantile(median_boot_dat$mean_diffs, 0.025),
        quantile(median_boot_dat$mean_diffs, 0.975),
        pvalue(oneway_test(pp~fhos,
                           data = data,
                           distribution = approximate(nresample = 1e+6)))[1]), 3)






library(tidyverse)
library(vroom)
library(ggplot2)

start.date = as.Date("2020-03-01")
today <- Sys.getenv("today")

data <- vroom(paste0("~/cov-ind-19-data/", today, "/jhu_data.csv")) %>%
  group_by(Country) %>% filter(Case >= 100) %>%
  arrange(Date) %>%
  mutate(Day = seq(n()))

Day.max <- 60 # nrow(data %>% filter(Country == "India"))
data <- filter(data, Day <= Day.max) %>%
  mutate(Date = format(Date, format = "%b %e")) %>%
  ungroup() %>%
  mutate(num.fmt = format(Case, big.mark = ",", scientific = F, trim = T)) %>%
  mutate(text = paste0(Date, ": ", num.fmt, " cumulative cases"))

# title <- paste("Cumulative number of COVID-19 cases in India compared",
#                "to other countries affected by the pandemic")
# 
# cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
#               format(today, format = "%b %e"), sep = ' ')

colors <- c(
  "China"       = "#ED553B",
  "South Korea" = "#56738A",
  "Italy"       = "#0472CF",
  "Iran"        = "#173F5F",
  "France"      = "#3CAEA3",
  "Germany"     = "#f2c82e",
  "US"          = "#822F21",
  "India"       = "#138808"
)

ymax        <- log10(1e6)
mybreaks    <- c(2:6)

db = function(x, intercept, double) {
  log(intercept * 2^((1/double) * x), base = 10)
}

p1 = 
ggplot(data = data, aes(x = Day, y = log(Case, base = 10), color = Country)) + 
  geom_line(size = 1.0, alpha = 0.8) + 
  theme_bw() + 
  theme(panel.background = element_rect(fill = NA),
        panel.ontop = FALSE,
        panel.border = element_blank(),
        legend.position = c(.15, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "left",
        legend.margin = margin(2, 2, 2, 2),
        legend.background=element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, vjust = 2),
        axis.line = element_line(size = 1, colour = "black"), 
        axis.line.y.right = element_blank(), 
        axis.ticks.y.right = element_blank()) + 
  stat_function(fun = db, args = list(intercept = 100, double = 3), xlim = c(2, 60), 
                color = '#545451', size = 0.7, linetype = "f8", alpha = 0.75) +
  annotate("text", x = 36.9, y = 5.63, label = "3 days", angle = 45, color = '#545451') + 
  stat_function(fun = db, args = list(intercept = 100, double = 6), xlim = c(2, 60), 
                color = '#545451', size = 0.7, linetype = "f8", alpha = 0.75) + 
  annotate("text", x = 55, y = 4.7, label = "6 days", angle = 28, color = '#545451') + 
  stat_function(fun = db, args = list(intercept = 100, double = 14), xlim = c(2, 60), 
                color = '#545451', size = 0.7, linetype = "f8", alpha = 0.75) + 
  annotate("text", x = 55, y = 3.12, label = "14 days", angle = 13, color = '#545451') + 
  ylim(c(2, log(max(data$Case) + 1000000, base = 10))) + 
  scale_color_manual(values=c(colors)) + 
  xlab('Days since cases reached 100') + 
  ylab('Cumulative number of cases') + 
  scale_y_continuous(breaks   = mybreaks,
                     limits   = c(2, ymax),
                     labels   = format(round(10^(mybreaks), 0), big.mark = ",", 
                                       scientific = FALSE, trim = T),
                     sec.axis = sec_axis(~., name = "", breaks = mybreaks, 
                                         labels = format(round(10^(mybreaks), 0), 
                                                         big.mark = ",", scientific = FALSE, trim = T)))

p1

# Note: you will have to adjust text annotation angles to your plot resolution and dimensions...

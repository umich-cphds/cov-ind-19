library(tidyverse)
library(vroom)
library(ggplot2)

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
        data_repo <- "~/cov-ind-19-data/"
        today     <- Sys.getenv("today")
} else {
        data_repo <- "~/cov-ind-19-test/"
        today     <- max(as.Date(grep("[0-9]", list.files(data_repo), value = T)))
}

start.date = as.Date("2020-03-01")
data <- vroom(paste0(data_repo, today, "/jhu_data.csv")) %>%
  group_by(Country) %>% filter(Case >= 100) %>%
  arrange(Date) %>%
  mutate(Day = seq(n()))

Day.max <- 60 # nrow(data %>% filter(Country == "India"))
max.plot.day = as.Date("2020-04-10", format = "%Y-%m-%d")
data = 
  data %>% filter(Date < max.plot.day)

data <- filter(data, Day <= Day.max) %>%
  mutate(Date = format(Date, format = "%b %e")) %>%
  ungroup() %>%
  mutate(num.fmt = format(Cases, big.mark = ",", scientific = F, trim = T)) %>%
  mutate(text = paste0(Date, ": ", num.fmt, " cumulative cases"))



# title <- paste("Cumulative number of COVID-19 cases in India compared",
#                "to other countries affected by the pandemic")
# 
# cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
#               format(today, format = "%b %e"), sep = ' ')

colors <- c(
  "China"       = "#003366",
  "South Korea" = "#3366cc",
  "Italy"       = "#666699",
  "Iran"        = "#9900ff",
  "France"      = "#660066",
  "Germany"     = "#990033",
  "US"          = "#996600",
  "India"       = "#138808",
  "Spain" = "#999966",
  "United Kingdom" = "#009999",
  "Turkey" = "#0099ff",
  "Belgium" = "#9999ff",
  "Netherlands" = "#ff6699",
  "Switzerland" = "#ffcc66",
  "Canada" = "#ccccff"
)

ymax        <- log10(1e6)
mybreaks    <- c(2:6)

db = function(x, intercept, double) {
  log(intercept * 2^((1/double) * x), base = 10)
}

p1 = 
ggplot(data = data %>% filter(Country != "India"), aes(x = Day, y = log(Cases, base = 10), color = Country)) + 
  geom_line(size = 1.0, alpha = 0.75) + 
  theme_bw() + 
  theme(panel.background = element_rect(fill = NA),
        panel.ontop = FALSE,
        panel.border = element_blank(),
        legend.position = c(.13, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "left",
        legend.margin = margin(1, 1, 1, 1),
        legend.background=element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, vjust = 2),
        axis.line = element_line(size = 1, colour = "black"), 
        axis.line.y.right = element_blank(), 
        axis.ticks.y.right = element_blank()) + 
  geom_line(data = data %>% filter(Country == "India"), size = 2.0, alpha = 0.8) + 
  stat_function(fun = db, args = list(intercept = 100, double = 3), xlim = c(2, 60), 
                color = '#363534', size = 0.8, linetype = "f8", alpha = 0.75) +
  annotate("text", x = 39.2, y = 5.86, label = "3 days", angle = 47, color = '#545451') + 
  stat_function(fun = db, args = list(intercept = 100, double = 6), xlim = c(2, 60), 
                color = '#363534', size = 0.8, linetype = "f8", alpha = 0.75) + 
  annotate("text", x = 55, y = 4.7, label = "6 days", angle = 29, color = '#545451') + 
  stat_function(fun = db, args = list(intercept = 100, double = 14), xlim = c(2, 60), 
                color = '#363534', size = 0.8, linetype = "f8", alpha = 0.75) + 
  annotate("text", x = 55, y = 3.13, label = "14 days", angle = 13, color = '#545451') + 
  ylim(c(2, log(max(data$Cases) + 1000000, base = 10))) + 
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

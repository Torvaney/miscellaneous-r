library(ggplot2)
library(rvest)
library(pipeR)
library(dplyr)

# Please don't take this as evidence that I like/enjoy/tolerate Top Gear
top_gear_url <- "http://topgear.wikia.com/wiki/Star_in_a_Reasonably-Priced_Car"

web_page <- read_html(top_gear_url)

# Get the celebrity times in the Suzuki Liana
not_f1 <- web_page %>>%
  html_nodes("table:nth-child(5) li") %>>%
  html_text()

# ... and the F1 drivers' times in the same car
f1 <- web_page %>>%
  html_nodes("ol:nth-child(2) li") %>>%
  html_text()

all_drivers <- data_frame(raw_string = c(not_f1, f1),
                          is_f1 = c(rep(FALSE, length(not_f1)), rep(TRUE, length(f1)))) %>>%
  mutate(time_secs = gsub("[^0-9:\\.]", "", raw_string) %>>%
           (gsub(".*:", "", . )) %>>%
           (gsub("\\.$", "", . )) %>>%  # remove trailing period
           as.numeric(),
         time_mins = gsub("[^0-9:\\.]", "", raw_string) %>>%
           (gsub(":.*", "", . )) %>>%
           as.numeric(),
         time_total = (time_mins * 60) + time_secs,
         driver_name = gsub("[^A-Za-z]|\\(.*\\)", "", raw_string),
         is_wet = grepl("wet", raw_string),
         is_moist = grepl("moist", raw_string))

# Do some stats
summary(fit <- lm(time_total ~ is_f1, data = all_drivers))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 112.2451     0.4622 242.833  < 2e-16 ***
#   is_f1TRUE    -7.5989     1.1322  -6.711 3.05e-09 ***

# Plot it
ggplot(all_drivers, aes(y = ifelse(is_f1 == 1, "F1", "Celebrity"),
                        x = time_total)) +
  geom_point(alpha = 0.3, size = 5) + 
  theme_bw() +
  xlab("Lap time (secs)") +
  ylab("") +
  ggtitle("Are F1 drivers quicker in Top Gear?",
          paste0("source: ", top_gear_url)) +
  theme(plot.margin = unit(c(1, 1, 1, 0), units = "cm"))

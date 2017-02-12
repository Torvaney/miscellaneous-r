library(magrittr)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Load data
start_of_year <- as.Date("2016-01-01") 
start_day <- as.Date("2016-02-22")  # The day the coffee machine was installed (it's a Monday)
today <- Sys.Date()
coffee_data <- read_csv("data/coffee-machine-sample.csv")  # you're gonna need to set the working dir for this...

# Create dataframe with a row for each day
date_origin <- "1970-01-01"
day_range <- as.numeric(start_of_year, origin=date_origin):as.numeric(today, origin=date_origin)

dates <- data_frame(day_number=day_range,
                    day = as.Date(day_range, origin=date_origin))

# Get df of days in which coffee machine was not working
out_of_order_dates <- dates %>% 
  merge(coffee_data) %>% 
  filter((day >= start_date) & (day <= end_date)) %>% 
  select(day) %>% 
  mutate(out_of_order=TRUE)

status_by_day <- dates %>% 
  merge(out_of_order_dates, all.x=TRUE) %>% 
  mutate(out_of_order = ifelse(is.na(out_of_order),
                               ifelse(day >= start_day, FALSE, NA), TRUE),
         dd = lubridate::day(day),
         weekday = weekdays(as.Date(day), TRUE),
         weekday_factor = factor(weekday, levels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')),
         month_name = month(day, TRUE),
         month_number = month(day, FALSE),
         year = year(day),
         week_number = as.numeric(format(day,"%W")),
         month_and_year = paste(month_name, year)) %>% 
  group_by(month_name, month_number) %>% 
  mutate(week_in_month = week_number - min(week_number)) %>% 
  ungroup()

head(status_by_day)

# Get downtime estimates from a certain date
downtime_from_date <- function(downdate="2016-01-01") {
  downtime <- status_by_day %>% 
    filter(day >= as.Date(downdate)) %>% 
    use_series(out_of_order) %>% 
    mean(na.rm=TRUE)
  
  return(downtime)
}

# Get overall uptime
print(percent(1 - downtime_from_date()))

# Get uptime since first break
first_downdate <- status_by_day %>% 
  filter(out_of_order) %>% 
  arrange(day) %>% 
  slice(1) %>% 
  use_series(day)

print(percent(1 - downtime_from_date(first_downdate)))


# Plot it
status_by_day %>%
  ggplot(aes(x = week_in_month, y = weekday_factor,
             label = dd, fill = out_of_order)) +
  geom_tile(colour = "black") +
  geom_text(colour = "white") + 
  facet_wrap(year ~ month_name) +
  theme_minimal() +
  scale_fill_manual(values = c("brown", "grey"), na.value = "grey95",
                    guide = guide_legend(title = "Out of order: ")) +
  theme(legend.position = 'bottom') +
  xlab("") + ylab("") +
  ggtitle("Coffee machine status")



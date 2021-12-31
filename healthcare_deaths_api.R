#### Summary ####
# This script pulls data from the national dashboard on
# COVID-19 healthcare data and plots it

#### Load Packages ####
library(ukcovid19)
library(tidyverse)
library(httr)
library(lubridate)
library(reshape2)
library(curl)

#### set proxy port ####
set_config(use_proxy(url="http://proxy", port=80))

#### Download data via API ####

# Query for admissions data
hc_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsTrust&areaCode=RM3&metric=covidOccupiedMVBeds&metric=hospitalCases&metric=newAdmissions&format=csv"

# tempfile for download
temp <- tempfile()

# Download data
temp <- curl_download(url = hc_url, 
                      destfile = temp, 
                      quiet = FALSE, 
                      mode = "wb")

# Read downloaded data
hc_data <- read.csv(temp)

# Remove unused columns, convert date to date format, convert to long format, & replace NAs with 0s
hc_data <- hc_data %>% 
  select(-areaCode,
         -areaType) %>% 
  mutate(date = ymd(date)) %>% 
  melt(id = c("areaName", "date")) %>% 
  mutate(value = replace_na(value, replace = 0))

#### Make some plots ####

# Theme for plotting
theme_steve <- theme_classic() +
  theme(plot.title = element_text(face = "bold", size = 10),
        axis.title = element_text(size = 9),
        legend.title = element_text(size = 9),
        plot.subtitle = element_text(size = 10, colour = "grey40"),
        plot.caption = element_text(size = 9, hjust = 0),
        strip.text = element_text(size = 8),
        strip.background = element_blank())

# Rename levels of variable for plotting
levels(hc_data$variable) <- c("COVID-19 patients in mechanical ventilation beds", 
                              "COVID-19 inpatients", 
                              "Daily COVID-19 new admissions")

# Faceted plot of all data
hc_data %>% 
  ggplot(aes(x = date,
             y = value)) +
  geom_line() +
  facet_wrap(~variable,
             ncol = 1,
             scales = "free") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y",
               limits = c(ymd("2021/06/01", NA))) +
  labs(title = "Northern Care Alliance COVID-19 healthcare use",
       x = NULL,
       y = NULL,
       caption = str_wrap("Note: due to changes to the hospitals within the NCA organisation, NCA data is not comparable with previous time points in the pandemic.",
                          width = 180)) +
  theme_steve

#### Deaths data ###

# url for deaths data
deaths_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&areaCode=E08000002&metric=newDeaths28DaysByDeathDate&metric=newOnsDeathsByRegistrationDate&format=csv"

# tempfile for download
temp <- tempfile()

# Download data
temp <- curl_download(url = deaths_url, 
                      destfile = temp, 
                      quiet = FALSE, 
                      mode = "wb")

# Read downloaded data
deaths <- read.csv(temp)

# Remove unused columns, convert date to date format, convert to long format, & replace NAs with 0s
deaths <- deaths %>% 
  select(-areaCode,
         -areaType) %>% 
  mutate(date = ymd(date)) %>% 
  melt(id = c("areaName", "date")) %>% 
  mutate(value = replace_na(value, replace = 0))

#### Make some plots ####

# Rename levels of variable for plotting
levels(deaths$variable) <- c("Deaths within 28 days of a positive COVID-19 test", 
                             "Deaths deaths with COVID-19 on the death certificate")

# Faceted plot of all deaths data
deaths %>% 
  ggplot(aes(x = date,
             y = zoo::rollmean(value, k = 7, na.pad = TRUE),
             group = variable,
             colour = variable)) +
  geom_line() +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  labs(title = "Daily COVID-19 deaths in Bury",
       subtitle = "7-day rolling averages",
       x = NULL,
       y = "Deaths",
       colour = NULL) +
  theme_steve +
  theme(legend.position = "top")

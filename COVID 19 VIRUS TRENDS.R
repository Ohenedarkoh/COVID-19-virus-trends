#load required libraries
library(dplyr)
library(readr)
library(tibble)

#flitering and exploring country_wise_latest
country_wise_latest<- as_tibble(country_wise_latest)
summary(country_wise_latest)
glimpse(country_wise_latest)
library(tidyverse)
print(country_wise_latest)


# Selected relevant columns and filtered rows based on a condition
filtered <- country_wise_latest %>%
  select(`Country/Region`, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`) %>%
  filter(!is.na(Deaths) & !is.na(Recovered) & !is.na(`New cases`) & !is.na(`New deaths`) 
         & !is.na(`New recovered`))

#  filtered data
print(filtered)


# Aggregated data by country
aggregated_data <- filtered %>%
  group_by(`Country/Region`) %>%
  summarise(
    total_deaths = sum(Deaths, na.rm = TRUE),
    total_recovered = sum(Recovered, na.rm = TRUE),
    total_active = sum(Active, na.rm = TRUE),
    total_new_cases= sum(`New cases`, na.rm = TRUE),
    total_new_deaths = sum(`New deaths`, na.rm = TRUE),
    total_new_recoverd = sum(`New recovered`, na.rm = TRUE)
     
  )

#  aggregated data
print(aggregated_data)


# Created the top countries data sorted by total_tests (highest first)
top_countries <- aggregated_data %>%
  arrange(desc(`Recovered`))

# Step 2: Extract the top 5 countries based on 'Recovered'
top_countries_vector <- top_countries$`Country/Region`[1:5]

# Print the top 5 countries based on recovery numbers
print(top_countries_vector)

# Created a matrix with top 5 countries' data (tests, cases, and ratio)
top_countries_matrix <- as.matrix(top_countries[1:5, c("Country/Region", "Deaths", "Recovered", "Active", "`New cases`", "`New deaths`", "`New recovered`")])

# Print results
print(top_countries)
print(top_countries_matrix)





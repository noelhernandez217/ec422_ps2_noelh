library(tidyquant)
library(tidyverse)

# Commeting this stuff out because we don't want to be redownloading the data 

# unemp_raw <- tq_get(
#  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"), <- is this the symbols? 
#  get  = "economic.data",
#  from = "1976-01-01"
# )

# head(unemp_raw)

# dir.create("data", showWarnings = FALSE)

# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

head(unemp_raw, 10)

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      # I set the matching symbols here
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California", 
      symbol == "TXUR" ~ "Texas", 
      symbol == "NYUR" ~ "New York"
          )
  )

head(unemp)

# check to see if it works 

# only_oregon <- filter(unemp, symbol == "ORUR")
# head(only_oregon)
# 
# only_cali <- filter(unemp, symbol == "CAUR")
# head(only_cali)

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

head(unemp)

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

head(unemp)

unemp_recent <- filter(unemp, date > as.Date("2015-01-01"))
head(unemp_recent)

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

head(unemp_summary)

# I was looking at the documentation, technically with_ties is set to true by default. I just put it in anyways 
unemp_top_increases <- slice_max(unemp, unemployment_rate_change, n = 5, with_ties = TRUE)
unemp_top_increases

unemp_top_decreases <- slice_min(unemp, unemployment_rate_change, n = 5, with_ties = TRUE)
unemp_top_decreases


# excluding the year 2020 because of covid 

unemp_top_increases_no_covid <- unemp |> filter(year(date) != 2020) |> slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)
unemp_top_increases_no_covid  

unemp_top_decreases_no_covid <- unemp |> filter(year(date) != 2020) |> slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)
unemp_top_decreases_no_covid


# Standard ggplot, I used the geeksforgeeks documentation. Helps a lot its really quick and easy to follow
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +        # or geom_col(), geom_point(), etc.
  labs(title = "Unemployment Rate over Time", x = "Time (months)", y = "Unemployment Rate (%)") +
  theme_minimal()


# facet_wrap splits this up and gives each different region their own plot
ggplot(unemp, aes(x = date, y = unemployment_rate)) +
  geom_line() + facet_wrap(~ region)      # or geom_col(), geom_point(), etc.
  labs(title = "Unemployment Rate over Time", x = "Time (months)", y = "Unemployment Rate (%)") +
  theme_minimal()
  
  
# Bar Chart
  
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate, fill = region)) +
  labs(
    title = "Mean Unemployment Rate by Region",
    x = "Region", 
    y = "Mean Unemployment Rate"
  ) + 
  geom_col()

# Now I plot something I am interested in (Section 5.4)

# IDEA: I am curious about plotting the unemployment rate change instead of the unemployment rate. I can overlap them. 
# I was curious to see how the unemployment rate and the month to month changes would look alongside each other. 
# Month to month, the change is steady. We see the massive spike during covid in 2020. 

rate_vs_change <- ggplot(unemp, aes(x = date)) + 
  geom_line(aes(y = unemployment_rate, color = "Unemployment Rate")) + 
  geom_line(aes(y = unemployment_rate_change, color = "Unemployment Rate Change")) + 
  facet_wrap(~ region) +
  labs(
    title = "Unemployment Rate vs Unemployment Rate Change", 
    x = "Date",
    y = "Percentage / Point Change",
    color = "Metric"
  )

rate_vs_change


  



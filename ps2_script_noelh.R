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




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

only_oregon <- filter(unemp, symbol == "ORUR")
head(only_oregon)



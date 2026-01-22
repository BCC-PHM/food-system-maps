library(BSol.mapR)
library(dplyr)
library(readxl)

colours = list(
  "Food Economy & Employment" = "#63B0CA",
  "Food Production" = "#926849",
  "Multiple" = "#1A1A1A",
  "Food Skills & Knowledge" = "#3396FF",
  "Food Waste & Recycling" = "#008547"
)

data <- read_excel("data/BFLF Postcodes.xlsx")

main <- data %>%
  select(
    "Recipient Org:Name", "Main location", "Main location postcode"
  ) %>%
  rename(
    Name = `Recipient Org:Name`, 
    Address = `Main location`,
    Postcode = `Main location postcode`
  ) %>%
  mutate(
    Type = "Main location"
  )

other1 <- data %>%
  select(
    "Recipient Org:Name", "Other location 1", "Other location 1 postcode"
  ) %>%
  rename(
    Name = `Recipient Org:Name`,
    Address = `Other location 1`,
    Postcode = `Other location 1 postcode`
  ) %>%
  mutate(
    Type = "Additional location"
  ) %>%
  filter(
    Address != "N/A"
  )

other2 <- data %>%
  select(
    "Recipient Org:Name", "Other location 2", "Other location 2 postcode"
  ) %>%
  rename(
    Name = `Recipient Org:Name`,
    Address = `Other location 2`,
    Postcode = `Other location 2 postcode`
  ) %>%
  mutate(
    Type = "Additional location"
  ) %>%
  filter(
    Address != "N/A"
  )

data_long <- data.table::rbindlist(
  list(
    main, other1, other2
  )
)


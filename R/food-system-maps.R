library(BSol.mapR)
library(dplyr)
library(tmap)
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

data_long <- get_long_lat(
  data.table::rbindlist(
    list(
      main, other1, other2
    )
  )
) %>%
  select(
    c("Name", "Address", "Postcode", "Type", "LAT", "LONG")
  )

points_shape <- sf::st_as_sf(
    get_points_shape(
    data_long
  ) 
) %>%
  select(
    -c(LONG, LAT)
  )


map <- tm_shape(
  Ward
  ) +
  tm_borders() +
  tm_shape(points_shape) +
  tm_dots()
map
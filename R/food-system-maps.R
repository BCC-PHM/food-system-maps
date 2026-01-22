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

credits <- paste0("Contains OS data \u00A9 Crown copyright and database right ",
                 # Get current year
                 format(Sys.Date(), "%Y"),
                 ". Source:\nOffice for National Statistics licensed under the Open Government Licence v.3.0."
)

data <- read_excel("data/BFLF Postcodes.xlsx")

main <- data %>%
  select(
    "Recipient Org:Name", "Main location", "Main location postcode", "Primary Workstream"
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
    "Recipient Org:Name", "Other location 1", "Other location 1 postcode", "Primary Workstream"
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
    "Recipient Org:Name", "Other location 2", "Other location 2 postcode", "Primary Workstream"
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
    c("Name", "Address", "Postcode", "Type", "Primary Workstream", "LAT", "LONG")
  )

points_shape <- sf::st_as_sf(
    get_points_shape(
    data_long
  ) 
) %>%
  select(
    -c(LONG, LAT)
  )

# Fix column names
colnames(points_shape) <- gsub("\\.", " ", colnames(points_shape))

Brum_Wards <- sf::st_as_sf(Ward) %>%
  filter(
    Area == "Birmingham"
  )

################################################################################
#                          Plot interactive map                                # 
################################################################################

map <- tm_shape(
  Brum_Wards
  ) +
  tm_borders() +
  tm_shape(points_shape) +
  tm_dots(
    fill = "Primary Workstream",
    size = 0.5,
    fill.scale = tm_scale(
      breaks = names(colours),
      values = colours
    ),
    col = "#1A1A1A",
    shape = 21,
    lwd = 1.5
  ) +
  tm_credits(
    credits,
    size = 0.6,
    position = c("LEFT", "BOTTOM")
  )

save_map(map, "output/food-systems-interactive.html")


################################################################################
#                        Plot report map - no labels                           # 
################################################################################

map2 <- tm_shape(
  Brum_Wards
) +
  tm_borders() +
  tm_shape(points_shape) +
  tm_dots(
    fill = "Primary Workstream",
    size = 0.5,
    fill.scale = tm_scale(
      breaks = names(colours),
      values = colours
    ),
    col = "#1A1A1A",
    shape = 21,
    lwd = 1.5
  ) +
  tm_credits(
    credits,
    size = 0.6,
    position = c("LEFT", "BOTTOM")
  ) +
  tm_layout(
    legend.position = c("LEFT", "TOP"),
    scale = 0.8,
    legend.height = 8,
    legend.frame.alpha = 0,
    legend.frame.lwd = 0,
    legend.bg.alpha = 0.4,
    inner.margins = 0.08,
    frame = FALSE) +
  tm_compass(
    type = "8star",
    size = 4,
    position = c("right", "bottom"),
    color.light = "white"
  )

save_map(map2, "output/food-systems.png")


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
    lwd = 1.2
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

################################################################################
#                        Plot report map - no labels                           # 
################################################################################

points_shape2 <- points_shape %>%
  mutate(
    Index = row_number()
  ) %>%
  relocate(Index) %>%
  mutate(
    text_colour = case_when(
      `Primary Workstream` == "Multiple" ~ "white",
      TRUE ~ "black"
    )
  )

nudge <- function(pshape, dir, Index, change) {
  mask = pshape$Index == Index
  pshape$geometry[mask][[1]][[dir]] =  pshape$geometry[mask][[1]][[dir]] + change
  return(pshape)
}

# nudge points
points_shape2 <- nudge(points_shape2, 1, 52, -200)
points_shape2 <- nudge(points_shape2, 2, 52, -200)
points_shape2 <- nudge(points_shape2, 1, 40, +200)
points_shape2 <- nudge(points_shape2, 2, 40, +200)
points_shape2 <- nudge(points_shape2, 2, 46, +20)
points_shape2 <- nudge(points_shape2, 2,  2, +150)
points_shape2 <- nudge(points_shape2, 1,  2, +0)
points_shape2 <- nudge(points_shape2, 2, 29, -60)
points_shape2 <- nudge(points_shape2, 1, 29, -100)
points_shape2 <- nudge(points_shape2, 2,  1, -300)
points_shape2 <- nudge(points_shape2, 1,  1, +70)
points_shape2 <- nudge(points_shape2, 1, 25, +60)
points_shape2 <- nudge(points_shape2, 2, 25, +60)
points_shape2 <- nudge(points_shape2, 1, 30, -60)
points_shape2 <- nudge(points_shape2, 2, 30, -60)
points_shape2 <- nudge(points_shape2, 2, 46, +100)
points_shape2 <- nudge(points_shape2, 1, 46, -10)
points_shape2 <- nudge(points_shape2, 2, 17, -100)
points_shape2 <- nudge(points_shape2, 1, 17, +50)
points_shape2 <- nudge(points_shape2, 2, 22, -100)
points_shape2 <- nudge(points_shape2, 1, 22, +50)
points_shape2 <- nudge(points_shape2, 2, 20, +50)
points_shape2 <- nudge(points_shape2, 1, 20, +50)
points_shape2 <- nudge(points_shape2, 2,  6, -50)
points_shape2 <- nudge(points_shape2, 1,  6, -50)
map3 <- tm_shape(
  Brum_Wards
) +
  tm_borders() +
  tm_shape(points_shape2) +
  tm_dots(
    fill = "Primary Workstream",
    size = 0.8,
    fill.scale = tm_scale(
      breaks = names(colours),
      values = colours
    ),
    col = "#1A1A1A",
    shape = 21,
    lwd = 1.2
  ) +
  tm_text(
    text = "Index",
    component.autoscale = FALSE,
    size = 0.5,
    col = "text_colour"
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

map3

save_map(map3, "output/food-systems-labelled.png")

writexl::write_xlsx(
  points_shape2 %>% sf::st_drop_geometry() %>% select(-text_colour),
  "output/labelled_BFLF_data.xlsx"
)
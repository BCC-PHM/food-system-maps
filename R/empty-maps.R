library(BSol.mapR)
library(dplyr)
library(tmap)
library(readxl)

map_types <- c("Locality", "Locality", "Constituency", "Constituency", "Ward")
const_names<- c(FALSE, FALSE, FALSE, TRUE, FALSE)
const_lines<- c(FALSE, FALSE, TRUE, TRUE, FALSE)
local_names<- c(FALSE, TRUE, FALSE, FALSE, FALSE)

# Locality and Constituency with and without labels
for (i in 1:5) {
  
  map_i <- plot_empty_map(
    area_name = "Birmingham",
    map_type = map_types[[i]],
    const_names = const_names[[i]],
    locality_names = local_names[[i]],
    const_lines = const_lines[[i]]
  )
  
  save_name <- paste0(
    "output/empty-maps/",
    map_types[[i]],
    ifelse(const_names[[i]] | local_names[[i]], "-with-", "-without-"),
    "names.png"
  )
  
  save_map(map_i, save_name = save_name)
  
}


ward_shape <- Ward
ward_shape@data <- ward_shape@data %>%
  mutate(
    Ward = stringr::str_wrap(Ward, width = 10)
  )
ward_shape <- subset(ward_shape, ward_shape@data$Area == "Birmingham")

ward_shape@data$xmod <- c(0,0,0,0,0.1,                                                          # ie adjust text labels horizontally
                          0,0,-0.1,0.3,-0.4,                         
                          -0.09,0,0.3,-0.2,0,
                          0,0,0.2,0.3,0.1,                               
                          -1.0,0,-0.2,0,0,
                          0,0,-0.2,0,0,                             
                          0,-0.2,0.6,0,0.2,
                          0.1,0,-0.2,0,0.2,                                                
                          -0.3,0,0.3,0,0,
                          -0.2,0.15,0,0,0.2,                       
                          0,-0.1,-0.2,0,0,
                          0,0,-0.1,-0.2,0,                                              
                          0,-0.2,-0.2,0,-0.2,
                          0,-0.2,0.1,0)

ward_shape@data$ymod <- c(0,-0.2,0,0,-0.1,                                                        # ie adjust text labels vertically
                          0,0,-0.2,0.3,-0.1,                         
                          0,0,-0.1,0.1,0,
                          0.1,0,0,0,-0.1,                            
                          0.3,-0.1,0,0,-0.2,
                          0,0,0.1,0,0,                               
                          0.2,-0.1,0.3,0,0.15,
                          0,0.1,0,0,0.3,                                                                     
                          -0.4,0,-0.3,0,-0.2,
                          0,0,0.1,0,0.2,                       
                          0,0.2,0,0,0.1,
                          0,0,0.3,0,0,                                              
                          0,0,0,0,0,
                          -0.1,0,0,0)

# Ward map with names
map_ward <- plot_empty_map(
  area_name = "Birmingham",
  map_type = "Ward",
  const_names = F,
  locality_names = F,
) +
  tmap::tm_shape(ward_shape) +
  tm_text(text = "Ward", 
          size = 0.3,
          xmod="xmod",                                 
          ymod="ymod"
  ) 

save_map(map_ward, "output/empty-maps/Ward-with-names.png"
)

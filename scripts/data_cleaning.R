# Purpose: clean up raw data set ###############################################


## Load the necessary libraries ================================================
library(sf)
library(dplyr)
library(tidyverse)
library(lubridate)
library(forcats)

## Load raw datasets ===========================================================
setwd("~/Documents/Application_form/NGO_projects/data/raw/")

eng <- read_csv("epc/certificates.csv")
temp <- read_csv("epc/recommendations.csv")
codebook <- st_read("post_codebook/codepo_gb.gpkg") %>% 
  mutate(POSTCODE = postcode) %>% select("POSTCODE")
imd <- read_csv("uk_imd2019.csv") %>% 
  filter(LANAME == "Haringey") %>% 
  mutate(lsoa11cd = LSOA) %>% 
  select("lsoa11cd", "SOA_pct", "SOA_decile")  

lsoa_sdf <- st_read("lsoa_2011/IMD_2019.shp") %>% 
  select(1) %>% 
  right_join(imd, by = "lsoa11cd")

cb <- read.csv("community_building.csv")

imd <- NULL

## summarise with/ recommendations =============================================
temp$IMPROVEMENT_ID[temp$IMPROVEMENT_SUMMARY_TEXT == "Solar photovoltaic panels, 2.5 kWp" | 
                    temp$IMPROVEMENT_SUMMARY_TEXT == "Solar photovoltaic panels, 2.5kWp"] = 34
temp$IMPROVEMENT_ID[temp$IMPROVEMENT_SUMMARY_TEXT == "Solar water heating"] = 19
temp$IMPROVEMENT_ID[temp$IMPROVEMENT_SUMMARY_TEXT == "Low energy lighting for all fixed outlets"] = 35

g1 = unique(select(temp, IMPROVEMENT_ID, IMPROVEMENT_SUMMARY_TEXT))
g1 = g1[order(g1$IMPROVEMENT_ID),]
#writexl::write_xlsx(g1, "~/Documents/Application_form/NGO_projects/cleaned/recommendations_reference.xlsx")  

g = temp %>% 
  select("LMK_KEY", "IMPROVEMENT_ID") %>% 
  drop_na("IMPROVEMENT_ID") %>% 
  mutate(sug = 1, 
         IMPROVEMENT_ID = as.factor(paste0("G", IMPROVEMENT_ID))) %>% 
  pivot_wider(names_from = IMPROVEMENT_ID, values_from = sug) %>% 
  unnest(-1) %>% 
  unique()

#rec_sums = t(as.data.frame(t(colSums (g[, -1], na.rm = TRUE, dims = 1)) ) )
rec_sums = data_frame(
  IMPROVEMENT_ID = colnames(g[-1]), 
  N = colSums (g[, -1], na.rm = TRUE, dims = 1), 
  ratio = round(N/102226*100, 1)
)
rec_sums = rec_sums[order(rec_sums$N, decreasing = T), ]
g1$IMPROVEMENT_ID = paste0("G", g1$IMPROVEMENT_ID)

rec_sums <- left_join(rec_sums, g1, by = "IMPROVEMENT_ID") #, by.y = "IMPROVEMENT_ID")
rec_sums = rec_sums[-4, ]
rec_sums[5, 4] = "Replace single glazed windows"
rec_sums[7, 4] = "Floor insulation"
rec_sums[7, 2:3] = rec_sums[7, 2:3] + rec_sums[9, 2:3] 
rec_sums = rec_sums[-9, ]
rec_sums = rec_sums[order(rec_sums$N, decreasing = T), ]
#write_csv(rec_sums, "~/Documents/Application_form/NGO_projects/cleaned/recommendations_ratio.csv")  



ggplot(slice_head(rec_sums,n= 9) , aes(x = reorder(IMPROVEMENT_SUMMARY_TEXT, -ratio), y = ratio, fill = IMPROVEMENT_SUMMARY_TEXT)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(
    title = "Top 9 EPC Improvement Measures",
    x = NULL, # "Improvement Measures",
    y = "Households Receiving Improvements %"
  ) +  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2") + coord_flip() +
  geom_text(aes(label = paste0(ratio, "%")), vjust = -0.5, size = 3.5)

g2 = select(g, c("LMK_KEY", "G34", "G35", "G5"))
chart_rate <- eng %>% 
  mutate(year = lubridate::year(INSPECTION_DATE)) %>% 
  select(c("LMK_KEY", "year", 
           "CURRENT_ENERGY_RATING", "POTENTIAL_ENERGY_RATING" )) %>% 
  full_join(g2, by = "LMK_KEY")
year_total = count(chart_rate, year, name = "year_counts")
c2 = count(chart_rate, year, CURRENT_ENERGY_RATING, name = "rate_count") %>% 
  left_join(year_total, by = "year") %>% 
  mutate(ratio = round(100*rate_count/year_counts, 0)) 
#write_csv(c2, "~/Documents/Application_form/NGO_projects/cleaned/epc_stack.csv")  
# Create a custom color palette for energy ratings from 'A' to 'G'
energy_colors <- c("A" = "#006400",  # Dark Green
                   "B" = "#228B22",  # Forest Green
                   "C" = "#ADFF2F",  # GreenYellow
                   "D" = "#FFD700",  # Gold
                   "E" = "#FFA500",  # Orange
                   "F" = "#FF4500",  # OrangeRed
                   "G" = "#FF0000")  # Red

# Create the stacked bar chart
ggplot(filter(c2, year >= 2008), aes(x = as.factor(year), y = ratio, fill = CURRENT_ENERGY_RATING)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = energy_colors) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 25),  # Define breaks from 0% to 100% by 10%
    labels = paste0(seq(0, 100, by = 25), "%")  # Label these breaks with percentage signs
  ) +
  labs(
    title = NULL,  # No title
    x = NULL,  # No x-axis label
    y = NULL,  # No y-axis label
    fill = NULL  # No legend title
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()  # Remove legend title
  )

c3 = count(chart_rate, year, POTENTIAL_ENERGY_RATING, name = "rate_count") %>% 
  left_join(year_total, by = "year") %>% 
  mutate(ratio = round(100*rate_count/year_counts, 0)) 
#write_csv(c3, "~/Documents/Application_form/NGO_projects/cleaned/epc_stack_potential.csv")  
#c4 = count(chart_rate, year, CURRENT_ENERGY_RATING, name = "rate_count")
#### 
## grouping recommendations ====================================================
# Define the mapping of specific suggestions to broader categories
mapping <- list(
  "solar panels" = c("Solar photovoltaic panels, 2.5 kWp", 
                     "Solar photovoltaic panels, 2.5kWp"),
  "solar water heating" = "Solar water heating", 
  "increase loft insulation" = "Increase loft insulation to 270 mm",
  "condensing boiler" = c("Change heating to gas condensing boiler", 
                          "Change room heaters to condensing boiler", 
                          "Condensing boiler (separate from the range cooker)", 
                          "Replace boiler with new condensing boiler", 
                          "Replace heating unit with condensing unit"),
  "low energy lighting" = "Low energy lighting for all fixed outlets",
  "water cylinder insulation" = c("Add additional 80 mm jacket to hot water cylinder", 
                                  "Increase hot water cylinder insulation", 
                                  "Insulate hot water cylinder with 80 mm jacket"),
  "floor/wall/roof insulation" = c("Floor insulation", 
                                   "Floor insulation (solid floor)", 
                                   "Floor insulation (suspended floor)", 
                                   "Internal or external wall insulation", 
                                   "Cavity wall insulation", 
                                   "Party wall insulation", 
                                   "Flat roof or sloping ceiling insulation", 
                                   "Room-in-roof insulation"), 
  "heating controls" = c("Heating controls (programmer and room thermostat)", 
                         "Heating controls (programmer and TRVs)", 
                         "Heating controls (programmer, room thermostat and TRVs)", 
                         "Heating controls (room thermostat and TRVs)", 
                         "Heating controls (room thermostat)", 
                         "Heating controls (thermostatic radiator valves)", 
                         "Heating controls (time and temperature zone control)"),
  "high heat retention storage heaters" = c("High heat retention storage heaters", 
                                            "High heat retention storage heaters and dual immersion cylinder"),
  "replacement windows" = c("Replace single glazed windows with low-E double glazed windows", 
                            "Replacement glazing units", 
                            "Secondary glazing to single glazed windows"),
  "draught proofing" = "Draught proofing",
  "heat recovery device" = c("Flue gas heat recovery device in conjunction with boiler", 
                             "Heat recovery system for mixer showers"),
  "wood pellet stove" = "Wood pellet stove with boiler and radiators",
  "wind turbine" = "Wind turbine",
  "high performance external doors" = "High performance external doors",
  "hot water cylinder thermostat" = "Hot water cylinder thermostat",
  "replacement warm air unit" = "Replacement warm air unit"
)

# Function to recode based on mapping
recode_improvements <- function(text, mapping) {
  for (category in names(mapping)) {
    if (text %in% mapping[[category]]) {
      return(category)
    }
  }
  return(text)  # Return original text if no match found
}

# Apply recoding function to IMPROVEMENT_SUMMARY_TEXT
temp <- temp %>%
  mutate(IMPROVEMENT_SUMMARY_TEXT = sapply(as.character(IMPROVEMENT_SUMMARY_TEXT), recode_improvements, mapping)) %>% 
  select(c(1, 3))
mapping <- NULL

temp <- unique(temp)
temp <- drop_na(temp)

summary(as.factor(temp$IMPROVEMENT_SUMMARY_TEXT))


## select important epc features and add pp geometry ===========================
# "Insulate hot water cylinder with 80 mm jacket", "Add additional 80 mm jacket to hot water cylinder"
# "Increase loft insulation to 270 mm"
# "Fan assisted storage heaters", "Fan assisted storage heaters and dual immersion cylinder", "Fan-assisted storage heaters" 
df <- eng %>%  
  select(c("LMK_KEY", "POSTCODE", 
           "CURRENT_ENERGY_RATING", "POTENTIAL_ENERGY_RATING", # rating after improvement
           #"CURRENT_ENERGY_EFFICIENCY", "POTENTIAL_ENERGY_EFFICIENCY", 
           "INSPECTION_DATE", # date
           "CO2_EMISSIONS_CURRENT", "CO2_EMISSIONS_POTENTIAL", # co2
           "PHOTO_SUPPLY", # Percentage of photovoltaic area as a percentage of total roof area. 0% indicates that a Photovoltaic Supply is not present in the property.
           "TENURE" #, "TRANSACTION_TYPE" # type of house, help identify if rental
  ))  %>% 
  mutate(recommendations = ifelse(LMK_KEY %in% unique(temp$LMK_KEY), T, F), 
         #pv_needed = ifelse(LMK_KEY %in% (temp$LMK_KEY[temp$IMPROVEMENT_SUMMARY_TEXT == "solar panels"]), T, F), 
         #condensing_b = ifelse(LMK_KEY %in% (temp$LMK_KEY[temp$IMPROVEMENT_SUMMARY_TEXT == "condensing boiler"]), T, F), 
         #solar_water = ifelse(LMK_KEY %in% (temp$LMK_KEY[temp$IMPROVEMENT_SUMMARY_TEXT == "solar water heating"]), T, F), 
         led = ifelse(LMK_KEY %in% (temp$LMK_KEY[temp$IMPROVEMENT_SUMMARY_TEXT == "low energy lighting"]), T, F), 
         #loft = ifelse(LMK_KEY %in% (temp$LMK_KEY[str_detect(temp$IMPROVEMENT_SUMMARY_TEXT, "loft insulation")]), T, F), 
         social_rental = ifelse((TENURE == "rental (social)" | TENURE == "Rented (social)"), "Yes", "Other")
         ) %>% # loft insulation, condensing boilers, solar thermal heatings, leds
  # mutate(INSPECTION_DATE = format(INSPECTION_DATE, "%Y-%m")) %>% 
  left_join(codebook, by = "POSTCODE") # 131409
  
  rl = which(is.na(df$social_rental))
  df$social_rental[rl] = "Other"
  df$social_rental = as.factor(df$social_rental)

temp <- NULL

## assign lsoa code to each point ==============================================
## transform to WGS 84 CRS, to match with later leaflet or OSM =================
lsoa <- st_transform(lsoa_sdf, crs = '+proj=longlat +datum=WGS84')

df_pp <- df %>% 
  st_as_sf(crs = st_crs(codebook)) %>% 
  st_transform(crs = '+proj=longlat +datum=WGS84') 
## Clip or align CRS ===========================================================
if (st_crs(df_pp) == st_crs(lsoa)){
  print("CRS matched")
} else {
  print("please transform CRS")
}

# Perform spatial join to add LSOA code to each point
sdf <- df_pp %>% 
  st_join(lsoa, join = st_within) %>% 
  drop_na(lsoa11cd) 

pt_data = sdf %>% 
  select(c("POSTCODE", "lsoa11cd", "CURRENT_ENERGY_RATING",  
            "POTENTIAL_ENERGY_RATING", "INSPECTION_DATE", 
           "recommendations", "led", "social_rental", 
           "SOA_pct", "SOA_decile")) %>% 
  dplyr::mutate(lat = sf::st_coordinates(geometry)[,2],
                lon = sf::st_coordinates(geometry)[,1]) %>% 
  st_drop_geometry() %>% 
  mutate(INSPECTION_DATE = year(INSPECTION_DATE)) 

pt_data = pt_data[pt_data$CURRENT_ENERGY_RATING != "INVALID!", ]
pt_data$CURRENT_ENERGY_RATING = as.factor(pt_data$CURRENT_ENERGY_RATING)
pt_data$POTENTIAL_ENERGY_RATING = as.factor(pt_data$POTENTIAL_ENERGY_RATING)
pt_data$SOA_decile = as.factor(pt_data$SOA_decile)

#write_csv(pt_data, "~/Documents/Application_form/NGO_projects/cleaned/pt.csv")  

cb <- sdf %>% select(c("POSTCODE", "lsoa11cd")) %>% 
  dplyr::mutate(lat = sf::st_coordinates(geometry)[,2],
                lon = sf::st_coordinates(geometry)[,1]) %>% 
  st_drop_geometry() %>% 
  unique() %>% 
  inner_join(cb) %>% 
  select(c(1:5)) 
#write_csv(cb, "~/Documents/Application_form/NGO_projects/cleaned/cb.csv")  


#df_pp <- st_intersection(df_pp, sdf)
plot(st_geometry(sdf), add = T, col = "blue")
plot(st_geometry(lsoa))
plot(st_geometry(cb), add = T, col = "red")
## saved in new file ====================================================================
# sdf saved the epc and imd data (by postcode unit point objects)
#st_write(sdf, "~/Documents/Application_form/NGO_projects/cleaned/epc_postcode.gpkg", append = F) 

# lsoa, contain imd data (lsoa polygon)
#st_write(lsoa, "~/Documents/Application_form/NGO_projects/cleaned/lsoa.shp", append = F) 


rm(list = ls())




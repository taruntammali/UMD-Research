# =========================================================================================================
# Name: Tarun Tammali
# Project: Retail Electricity Project
# Date: 6/30/2026
# Session: Summer 2026
# =========================================================================================================

# Packages
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("lubridate")
#install.packages("corrplot")
#install.packages("gt")
#install.packages("webshot2")
#install.packages("sf")
#install.packages("tigris")
#install.packages("dplyr")
#install.packages("arcgislayers")


# Library
library("tidyverse") # primary library
library("ggplot2")
library("readxl") 
library("lubridate")
library("corrplot")
library("gt")
library("webshot2")
library("sf")
library("tigris")
library("dplyr")
library("arcgislayers")



# =========================================================================================================

# Avg_Monthly_Residential_Price Dataset 
avg_res_price <- read.csv("data_clean/Avg_Monthly_Residential_Price_EIA861_2020_2026_Clean.csv")

avg_res_price$Residential.electricity.price <-
  as.numeric(avg_res_price$Residential.electricity.price)

# Summary Statistics
mean(avg_res_price$Residential.electricity.price, na.rm = TRUE)
median(avg_res_price$Residential.electricity.price, na.rm = TRUE)
sd(avg_res_price$Residential.electricity.price, na.rm = TRUE)
var(avg_res_price$Residential.electricity.price, na.rm = TRUE)
range(avg_res_price$Residential.electricity.price, na.rm = TRUE)
quantile(avg_res_price$Residential.electricity.price, na.rm = TRUE)

# =========================================================================================================

# === Graph Plot with trend line ===

# Converting the date into YYYY/MM/DD
avg_res_price$Date <- as.Date(
  paste0(avg_res_price$Series.Key, "01"),
  format = "%Y%m%d"
)

plot1 <- ggplot(avg_res_price, aes(x = Date, y= Residential.electricity.price)) + 
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  geom_point(size = 2) + 
  labs(title = "Average Monthly Residential Retail Electricity Price (2018-2026)", x = 'Year', y = "Avg Residential Retail Electricity Price (cents/KW)") +
  theme_minimal()
plot1
ggsave("figures/plot1_avg_res_price_trend.png", plot1, width = 7, height = 5, dpi = 300)


# =========================================================================================================

# Monthly Dataset
monthly <- read.csv("data_clean/monthly_datasets_2018_2026_clean.csv")
monthly$Date <- ym(as.character(monthly$Date))


# ======= Monthly Summary Statistics =======

monthly_summary <- monthly %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Variance = var(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

View(monthly_summary)


monthly_summary_png <- monthly_summary %>%
  gt() %>%
  tab_header(
    title = "Monthly Summary Statistics"
  ) %>%
  fmt_number(
    columns = -Variable, 
    decimals = 2
  )


gtsave(
  monthly_summary_png,
  "figures/monthly_summary_statistics.png", 
  vwidth = 1600,
  vheight = 1200
)

# =========================================================================================================

# Yearly Dataset
yearly <- read.csv("data_clean/yearly_datasets_2018_2026.csv") 

# ======= Yearly Summary Statistics =======

yearly_summary <- yearly %>%
  select(where(is.numeric)) %>% 
  select(-Year) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "Variable", 
    values_to = "Value"
  ) %>% 
  group_by(Variable) %>% 
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE), 
    Variance = var(Value, na.rm = TRUE), 
    Min = min(Value, na.rm = TRUE), 
    Max = max(Value, na.rm = TRUE), 
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

View(yearly_summary)

yearly_summary_png <- yearly_summary %>%
  gt() %>%
  tab_header(
    title = "Yearly Summary Statistics"
  )

gtsave(
  yearly_summary_png,
  "figures/yearly_summary_statistics.png",
  vwidth = 1600,
  vheight = 1200
)



# ======= GIS MAPPING (Heat Circles) =======

datacenters <- read.csv("data_clean/virginia_data_centers_arcgis_2024_2026_clean_merge.csv") %>%
  filter(!is.na(Lat) & !is.na(Long)) %>%
  drop_na(Lat, Long)

dc_sf <- st_as_sf(datacenters, coords = c("Long", "Lat"), crs = 4326)

# Group nearby data centers into map circles
dc_heat_circles <- datacenters %>%
  mutate(
    # It rounds the decimal to the nearest 0.15 decimal
    Long_bin = round(Long / 0.15) * 0.15,
    Lat_bin  = round(Lat / 0.15) * 0.15
  ) %>%
  count(Long_bin, Lat_bin, name = "dc_count")

#options(tigris_use_cache = TRUE)
va_counties <- counties(state = "VA", cb = TRUE, class = "sf")

static_map <- ggplot() +
  geom_sf(
    data = va_counties,
    fill = "#f9f9f9",
    color = "#737373",
    size = 0.3
  ) +
  
  # replaces stat_density
  geom_point(
    data = dc_heat_circles,
    aes(
      x = Long_bin,
      y = Lat_bin,
      size = dc_count,
      fill = dc_count
    ),
    shape = 21,
    color = "white",
    stroke = 0.25,
    alpha = 0.65
  ) +
  
  geom_sf(
    data = dc_sf,
    aes(color = Build_Status),
    size = 1.2,
    alpha = 0.7
  ) +
  
  scale_fill_gradient(
    low = "#293FAB",
    high = "#CC3F37",
  ) +
  scale_size_continuous(
    range = c(2, 14),
    guide = "none"
  ) +
  scale_color_manual(
    values = c("Existing" = "#4EA370", "Proposed" = "#ff7f0e"),
    name = "Status"
  ) +
  
  coord_sf(xlim = c(-83.7, -75.2), ylim = c(36.5, 39.5)) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#e4e4e4", size = 0.2),
    plot.title = element_text(face = "bold", size = 8, color = "#222222"),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Virginia Data Center Clustering and Facility Locations (202",
    x = "Longitude",
    y = "Latitude"
  )

print(static_map)

ggsave(
  "figures/virginia_datacenter_clusters.png",
  plot = static_map,
  width = 10,
  height = 6,
  dpi = 300
)


# =========================================================================================================


# ======= GIS MAPPING WITH DOM MAP =======


# ArcGIS FeatureServer Link
dom_layer_url <- "https://services3.arcgis.com/OYP7N6mAJJCyH6hd/arcgis/rest/services/Electric_Retail_Service_Territories_HIFLD/FeatureServer/0"

dom_layer <- arc_open(dom_layer_url)

# DOM / Dominion service territory in Virginia
dom_regions <- arc_select(
  dom_layer,
  where = "STATE = 'VA' AND NAME = 'VIRGINIA ELECTRIC & POWER CO'",
  fields = c("NAME", "STATE", "TYPE")
) %>%
  st_transform(4326)

# Virginia counties
va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  st_transform(4326)

dom_map <- ggplot() +
  geom_sf(
    data = va_counties,
    fill = "#f9f9f9",
    color = "#737373",
    size = 0.3
  ) +
  geom_sf(
    data = dom_regions,
    #fill = "#293FAB",
    aes(fill = NAME),
    color = "#CC3F37",
    linewidth = 0.45,
    alpha = 0.45
  ) +
  coord_sf(xlim = c(-83.7, -75.2), ylim = c(36.5, 39.5)) +
  scale_fill_manual(
    values = c("VIRGINIA ELECTRIC & POWER CO" = "#293FAB"),
    name = "Utility Territory",
    labels = c("VIRGINIA ELECTRIC & POWER CO" = "Dominion Energy")
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    legend.position = "right", 
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#e4e4e4", size = 0.2),
    plot.title = element_text(face = "bold", size = 8, color = "#222222"),
    #legend.position = "none"
  ) +
  labs(
    title = "Dominion Energy / Virginia Electric & Power Service Territory",
    x = "Longitude",
    y = "Latitude"
  )

print(dom_map)

ggsave(
  "figures/virginia_dom_regions.png",
  plot = dom_map,
  width = 10,
  height = 6,
  dpi = 300
)


# =========================================================================================================



# ======= GRAPH FOR SALES, TEMPERATURE, & DOM LOAD =======

dom_temp_compare <- monthly %>%
  select(Date, HDD, CDD, Sales, Average_load_DOM) %>%
  pivot_longer(
    cols = c(HDD, CDD),
    names_to = "Temp_Type", 
    values_to = "Temp_Value"
  ) %>%
  pivot_longer(
    cols = c(Average_load_DOM, Sales), 
    names_to = "Demand_Variable", 
    values_to = "Demand_Value"
  )

ggplot(dom_temp_compare, aes(x = Temp_Value, y = Demand_Value)) + 
  geom_point(alpha = 0.7, color = "#5787C2") + 
  geom_smooth(method = "lm", se = FALSE, color = "#D94E4E") +
  facet_grid(Demand_Variable ~ Temp_Type, scales = "free_y") + 
  scale_y_continuous(labels = scales::comma) + 
  labs(
    title = "Monthly HDD and CDD Compared with DOM Load and Sales", 
    x = "Heating/Cooling Degree Days", 
    y = "Demand Variable"
  ) + 
  theme_minimal()


# HRL DOM METERED LOAD 2018-2025

dom_load <- read.csv("data_clean/hrl_load_metered_2018_2026_DOM_yearly_average.csv") %>%
  select(Year, average_mw) %>%
  filter(Year >= 2018, Year <= 2025)

dom_load_plot <- ggplot(dom_load, aes(x = Year, y = average_mw)) +
  geom_line(color = "#293FAB", linewidth = 1.2) +
  geom_point(color = "#293FAB", size = 2.5) +
  geom_vline(
    aes(xintercept = 2022, linetype = "AI Boom Begins"),
    color = "#CC3F37",
    linewidth = 0.8
  ) +
  scale_linetype_manual(
    values = c("AI Boom Begins" = "dashed"),
    name = ""
  ) +
  scale_x_continuous(breaks = 2018:2025) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#e4e4e4", size = 0.2),
    plot.title = element_text(face = "bold", size = 10, color = "#222222"),
    legend.position = "right"
  ) +
  labs(
    title = "Annual Average DOM Metered Load, 2018-2025",
    x = "Year",
    y = "Average Metered Load (MW)"
  )

print(dom_load_plot)

ggsave(
  "figures/dom_metered_load_annual_average.png",
  plot = dom_load_plot,
  width = 10,
  height = 6,
  dpi = 300
)



# VIJAY's GIS MAPPING


datacenters <- read.csv("data_clean/virginia_data_centers_arcgis_2024_2026_clean_merge.csv") %>%
  filter(!is.na(Lat) & !is.na(Long)) %>%
  drop_na(Lat, Long)

# Convert data centers to spatial points
dc_sf <- st_as_sf(datacenters, coords = c("Long", "Lat"), crs = 4326)

# Get Virginia county boundaries
va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  st_transform(4326)

# Count data centers per county
va_intensity <- va_counties %>%
  st_join(dc_sf) %>%
  group_by(GEOID, NAME) %>%
  summarize(
    Intensity_Count = sum(!is.na(Name)),
    .groups = "drop"
  )

dom_layer_url <- "https://services3.arcgis.com/OYP7N6mAJJCyH6hd/arcgis/rest/services/Electric_Retail_Service_Territories_HIFLD/FeatureServer/0"

dom_layer <- arc_open(dom_layer_url)

dom_regions <- arc_select(
  dom_layer,
  where = "STATE = 'VA' AND NAME = 'VIRGINIA ELECTRIC & POWER CO'",
  fields = c("NAME", "STATE", "TYPE")
) %>%
  st_transform(4326)


gis_map <- ggplot(data = va_intensity) +
  geom_sf(
    aes(fill = Intensity_Count),
    color = "gray",
    size = 0.2
  ) +
  
  # Dominion service territory overlay
  geom_sf(
    data = dom_regions,
    aes(linetype = "Dominion Energy Territory"),
    fill = NA,
    color = "#293FAB",
    linewidth = 0.5,
    alpha = 0.9
  ) +
  
  geom_sf(
    data = dc_sf,
    aes(color = Build_Status),
    size = 1.2,
    alpha = 0.8
  ) +
  scale_fill_gradientn(
    colors = c("#f2f2f2", "#d9d9d9", "#bdbdbd", "#969696", "#525252"),
    values = scales::rescale(c(0, 1, 5, 20, 100)),
    name = "Data Centers\nper County"
  ) + 
  scale_color_manual(
    values = c("Existing" = "#00cc44", "Proposed" = "#ff00ff"),
    name = "Facility Status"
  ) +
  scale_linetype_manual(
    values = c("Dominion Energy Territory" = "solid"),
    name = "Utility Region"
  ) +
  
  coord_sf(xlim = c(-83.7, -75.2), ylim = c(36.5, 39.5)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "right"
  ) +
  labs(
    title = "Virginia Data Center Intensity, Facility Status, and Dominion Territory",
    x = "Longitude",
    y = "Latitude"
  )

print(gis_map)

# Save the map 
ggsave("figures/virginia_datacenter_gis_map.png", plot = gis_map, width = 10, height = 6, dpi = 300)

















# # ======= GIS MAPPING: DATA CENTERS + DOM REGION =======
# 
# datacenters <- read.csv("data_clean/virginia_data_centers_arcgis_2024_2026_clean_merge.csv") %>%
#   filter(!is.na(Lat) & !is.na(Long)) %>%
#   drop_na(Lat, Long)
# 
# dc_sf <- st_as_sf(datacenters, coords = c("Long", "Lat"), crs = 4326)
# 
# dc_heat_circles <- datacenters %>%
#   mutate(
#     Long_bin = round(Long / 0.15) * 0.15,
#     Lat_bin  = round(Lat / 0.15) * 0.15
#   ) %>%
#   count(Long_bin, Lat_bin, name = "dc_count")
# 
# va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
#   st_transform(4326)
# 
# # ArcGIS FeatureServer Link
# dom_layer_url <- "https://services3.arcgis.com/OYP7N6mAJJCyH6hd/arcgis/rest/services/Electric_Retail_Service_Territories_HIFLD/FeatureServer/0"
# 
# dom_layer <- arc_open(dom_layer_url)
# 
# dom_regions <- arc_select(
#   dom_layer,
#   where = "STATE = 'VA' AND NAME = 'VIRGINIA ELECTRIC & POWER CO'",
#   fields = c("NAME", "STATE", "TYPE")
# ) %>%
#   st_transform(4326)
# 
# combined_map <- ggplot() +
#   geom_sf(
#     data = va_counties,
#     fill = "#f9f9f9",
#     color = "#737373",
#     size = 0.3
#   ) +
#   
#   # DOM / Dominion service territory
#   geom_sf(
#     data = dom_regions,
#     aes(linetype = "Dominion Energy Territory"),
#     fill = "#E8D5BC",
#     color = "#D68E88",
#     linewidth = 0.9,
#     alpha = 0.9
#   ) +
#   
#   # Data center heat circles
#   geom_point(
#     data = dc_heat_circles,
#     aes(
#       x = Long_bin,
#       y = Lat_bin,
#       size = dc_count,
#       fill = dc_count
#     ),
#     shape = 21,
#     color = "white",
#     stroke = 0.25,
#     alpha = 0.65
#   ) +
#   
#   # Individual facilities
#   geom_sf(
#     data = dc_sf,
#     aes(color = Build_Status),
#     size = 1.2,
#     alpha = 0.7
#   ) +
#   
#   scale_fill_gradient(
#     low = "#293FAB",
#     high = "#CC3F37",
#     name = "Data Center\nCluster Count"
#   ) +
#   scale_size_continuous(
#     name = "Data Center\nCluster Count",
#     range = c(2, 14),
#     breaks = c(1, 2, 5, 10, 20),
#     labels = scales::comma,
#     guide = guide_legend(
#       override.aes = list(
#         shape = 21,
#         fill = "#d95f0e",
#         color = "#333333",
#         alpha = 0.75
#       )
#     )
#   ) + 
#   scale_color_manual(
#     values = c("Existing" = "#4EA370", "Proposed" = "#ff7f0e"),
#     name = "Facility Status"
#   ) +
#   scale_linetype_manual(
#     values = c("Dominion Energy Territory" = "solid"),
#     name = "Utility Region"
#   ) +
#   
#   coord_sf(xlim = c(-83.7, -75.2), ylim = c(36.5, 39.5)) +
#   
#   theme_minimal(base_family = "sans") +
#   theme(
#     panel.background = element_rect(fill = "white", color = NA),
#     panel.grid.major = element_blank(),
#     plot.title = element_text(face = "bold", size = 10, color = "#222222"),
#     plot.caption = element_text(size = 9, hjust = 0, color = "#222222"),
#     legend.position = "right",
#     legend.title = element_text(size = 11),
#     legend.text = element_text(size = 10),
#     legend.background = element_rect(fill = "white", color = NA)
#   ) + 
#   labs(
#     title = "Virginia Data Center Clustering, Facility Locations, and Dominion Territory",
#     x = "Longitude",
#     y = "Latitude"
#   )
# 
# print(combined_map)
# 
# ggsave(
#   "figures/virginia_datacenter_clusters_dom_overlay.png",
#   plot = combined_map,
#   width = 10,
#   height = 6,
#   dpi = 300
# )


# ======= GIS MAPPING: DATA CENTERS + DOM REGION =======

datacenters <- read.csv("data_clean/virginia_data_centers_arcgis_2024_2026_clean_merge.csv") %>%
  filter(!is.na(Lat) & !is.na(Long)) %>%
  drop_na(Lat, Long)

dc_sf <- st_as_sf(datacenters, coords = c("Long", "Lat"), crs = 4326)

dc_status_circles <- datacenters %>%
  mutate(
    Long_bin = round(Long / 0.15) * 0.15,
    Lat_bin  = round(Lat / 0.15) * 0.15
  ) %>%
  count(Long_bin, Lat_bin, Build_Status, name = "dc_count")

va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  st_transform(4326)

# ArcGIS FeatureServer Link
dom_layer_url <- "https://services3.arcgis.com/OYP7N6mAJJCyH6hd/arcgis/rest/services/Electric_Retail_Service_Territories_HIFLD/FeatureServer/0"

dom_layer <- arc_open(dom_layer_url)

dom_regions <- arc_select(
  dom_layer,
  where = "STATE = 'VA' AND NAME = 'VIRGINIA ELECTRIC & POWER CO'",
  fields = c("NAME", "STATE", "TYPE")
) %>%
  st_transform(4326)

combined_map <- ggplot() +
  geom_sf(
    data = va_counties,
    fill = "#f9f9f9",
    color = "#737373",
    size = 0.3
  ) +
  
  # Dominion service territory
  geom_sf(
    data = dom_regions,
    aes(linetype = "Dominion Energy Territory"),
    fill = scales::alpha("#E8D5BC", 0.5),
    color = "#4EA370",
    linewidth = 0.9,
    alpha = 0.9
  ) +
  
  # Status-colored data center cluster bubbles
  geom_point(
    data = dc_status_circles,
    aes(
      x = Long_bin,
      y = Lat_bin,
      size = dc_count,
      fill = Build_Status
    ),
    shape = 21,
    color = "white",
    stroke = 0.35,
    alpha = 0.75
  ) +
  
  scale_linetype_manual(
    values = c("Dominion Energy Territory" = "solid"),
    name = "Utility Region"
  ) +
  
  scale_fill_manual(
    values = c(
      "Existing" = "#293FAB",
      "Proposed" = "#CC3F37"
    ),
    name = "Facility Status",
    guide = guide_legend(
      order = 2,
      override.aes = list(size = 3)   
    )
  ) +
  
  scale_size_area(
    name = "Data Center\nCluster Count",
    max_size = 14,
    breaks = c(1, 2, 3, 5, 10),
    labels = scales::comma,
    guide = guide_legend(
      order = 3,                    
      override.aes = list(
        shape = 21,
        fill = "black",
        color = "black",
        alpha = 1,
        stroke = 0.35
      )
    )
  ) +
  
  guides(
    linetype = guide_legend(order = 1),
  ) +
  
  coord_sf(xlim = c(-83.7, -75.2), ylim = c(36.5, 39.5)) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    plot.title = element_text(face = "bold", size = 10, color = "#222222"),
    plot.caption = element_text(size = 9, hjust = 0, color = "#222222"),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),  
    legend.key.size = unit(1.3, "lines"),                   
    legend.spacing.y = unit(0.3, "cm")                       
  ) +
  
  labs(
    title = "Virginia Data Center Clustering, Facility Locations, and Dominion Territory",
    caption = "\nCircle size indicates data center cluster count. Blue clusters are existing facilities; red clusters are proposed facilities.",
    x = NULL,
    y = NULL
  )

print(combined_map)

ggsave(
  "figures/virginia_datacenter_clusters_dom_overlay.png",
  plot = combined_map,
  width = 10,
  height = 6,
  dpi = 300
)





















# =========================================================================================================
# ======= EXTRA CODE (NOT USED) =======



# Monthly_Electricity_Generation Dataset
#elec_gen <- read.csv("data_clean/Monthly_electricity_generation_SEDS_2018_2026_Clean.csv")
#elec_gen$Date <- ym(as.character(elec_gen$Date))

# Sales_Revenue-Monthly Dataset
#sales_revenue <- read.csv("data_clean/sales_revenue-Monthly-States_EIA861_2010_2026.csv")
#sales_revenue$X <- ym(as.character(sales_revenue$X))

# Yearly Average Inflation and Population Dataset
#inflation <- read.csv("data_clean/cpi_yearly_inflation_data_FRBM_2018_2026.csv")
#population <- read.csv("data_clean/virginia_yearly_population_FRED_2018_2026.csv")

#pop_infla <- inflation %>% left_join(population, by = "Year")
#pop_infla

# AI Datacenters 
#ai_datacenter <- read.csv("data_clean/yearly_ai_data_center_McKinsey & Company_2015_2023_clean.csv")

# Electricity Consumption Dataset
#elec_cons <- read.csv("data_clean/VA_YearlyConsumptionPerCapita_EIA_2018_2024_Clean.csv")

# Virginia Entities Residential Sector Sales Revenue Dataset
#entity_res_sales <- read.csv("data_clean/virginia_entities_residential sector_sales_revenue_EIA861_2024_clean.csv")

# Natural Gas Prices
#nat_gas <- read.csv("data_clean/Monthly_natural_gas_prices_EIA_2018_2026_Clean.csv")
#nat_gas$date <- ym(as.character(nat_gas$date))

# Average Temperature
#temp <- read.csv("data_clean/Monthly_Average_Temp_Weather.gov_2018_2026_Clean.csv")
#temp$Date <- ym(as.character(temp$Date))

# Clean Environment
rm(list = ls())

# Load Required Libraries
library(readxl)
library(sf)
library(ggplot2)
library(maps)
library(dplyr)

# Load Market Coordinates Data
url <- "https://raw.githubusercontent.com/Viktoriag27/Geospatial_Assignment_2/main/Data/Part%202/MktCoords.xlsx"
temp_file <- tempfile(fileext = ".xlsx")  
download.file(url, temp_file, mode = "wb") 
market_data <- read_excel(temp_file) 

# Convert to sf object
market_sf <- st_as_sf(market_data, coords = c("longitude", "latitude"), crs = 4326)

# Print first few rows
print(head(market_sf))

# Create Africa-Only Map
world_map <- map_data("world")

# Filter for Africa’s bounding box (approximate lat/lon ranges)
africa_map <- subset(world_map, long > -25 & long < 60 & lat > -40 & lat < 40)

# Plot Market Locations in Africa
ggplot() +
  geom_polygon(data = africa_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "white") +
  geom_sf(data = market_sf, color = "red", size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Market Locations") +  
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    plot.title = element_text(hjust = 0.5, face = "bold")  # Center the title
  )

# Load Price Data
url2 <- "https://raw.githubusercontent.com/Viktoriag27/Geospatial_Assignment_2/main/Data/Part%202/PriceMaster4GAMS.xlsx"
temp_file2 <- tempfile(fileext = ".xlsx")  
download.file(url2, temp_file2, mode = "wb") 
price_data <- read_excel(temp_file2, sheet=1) 

# Convert numeric price columns (ignore first 4 columns)
price_data <- price_data %>%
  mutate(across(5:ncol(.), as.numeric, .names = "num_{.col}"))

# Compute average price per `mktcode`
average_price <- price_data %>%
  rowwise() %>%
  mutate(avg_price = mean(c_across(5:ncol(.)), na.rm = TRUE)) %>%
  ungroup() %>%
  select(mktcode, avg_price) %>%
  group_by(mktcode) %>%
  summarise(avg_price = mean(avg_price, na.rm = TRUE))

# Print first few rows to check
print(head(average_price))

# Merge Market Coordinates with Prices
merged_data <- market_sf %>%
  left_join(average_price, by = "mktcode")

# Print merged data to check
print(head(merged_data))

# Save Merged Data
#write.csv(merged_data, "/Users/irachevergaraochoa/Desktop/Master - BSE/Geospatial analysis/Assignments/Assignment 2/Data/Merged_Market_Price_Data.csv", row.names = FALSE)



bomber_data <- read.csv("data/operations.csv")
library(skimr)
library(tidyverse)
library(naniar)
install.packages("maps")
library(maps)
skim_without_charts(bomber_data)


count_outliers <- function(x) {
  if (is.numeric(x) && length(x) > 0) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    return(sum(x < lower_bound | x > upper_bound, na.rm = TRUE))
  } else {
    return(NA)  # Return NA if the column is not numeric or empty
  }
}

# Apply the function to all numeric columns and count outliers
outliers_count <- bomber_data %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), count_outliers))

# Display the count of outliers for each numeric variable
print(outliers_count)


## Map of bombing activities ----
bomber_data_filtered_by_long_lat <- bomber_data |>
  mutate(Country = ifelse(is.na(Country), "Not Stated", Country)) |>
  filter(
    between(Target.Latitude, -90, 90) &
    between(Target.Longitude, -180, 180)
  ) |>

world_map <- map_data("world")
ggplot() +
  mutate(Country = ifelse(is.na(Country), "Not Stated", Country))
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "gray90") +
  geom_point(data = bomber_data_filtered_by_long_lat, aes(x = Target.Longitude, y = Target.Latitude), color = Country) +
  coord_map()


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

#looking at missingness
gg_miss_var(bomber_data)


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

bomber_data |>
  mutate(Country = ifelse(is.na(Country), "Not Stated", Country)) |>
  ggplot(aes(x = Country)) +
  geom_bar()

bomber_data_filtered_by_long_lat |>
  mutate(Country = ifelse(is.na(Country), "Not Stated", Country)) |>
  ggplot(aes(x = Country)) +
  geom_bar()


# Count occurrences of each industry
bomber_data %>%
  count(Theater.of.Operations == "CBI" | Theater.of.Operations == "PTO")

# Print the top 3 most common industries
bomber_data |>
  filter(Theater.of.Operations %in% c("ETO", "MTO")) |>
  count(Target.Industry) |>
  arrange(desc(n)) |>
  slice(1:20) |>
  pull(Target.Industry)

bomber_data |>
  filter(Theater.of.Operations %in% c("PTO", "CBI")) |>
  count(Target.Industry) |>
  arrange(desc(n)) |>
  slice(1) |>
  pull(Target.Industry)
bomber_data |>
  count(Theater.of.Operations) |>
  arrange(desc(n)) |>
  slice(1:10) |>
  pull(Theater.of.Operations)

### Effectiveness of defense

prop_downed_based_on_theater <- bomber_data |>
  filter(is.na(Aircraft.Lost), 
         is.na(Airborne.Aircraft), 
         is.na(Theater.of.Operations), 
         Theater.of.Operations %in% c("PTO", "ETO", "CBI", "MTO")) |>
  mutate(
    Theater_Group = case_when(
      Theater.of.Operations %in% c("PTO", "CBI") ~ "East",
      Theater.of.Operations %in% c("ETO", "MTO") ~ "West"),
    Prop_Downed = Aircraft.Lost / Airborne.Aircraft)

prop_downed_based_on_theater |>
  count(Theater_Group) |>
  arrange(desc(n)) |>
  slice(1:10) |>
  pull(Theater_Group)

prop_downed_based_on_theater |>
  filter(Theater_Group == "CBI")

bomber_data |>
  filter(is.na(Airborne.Aircraft)) |>
  count(Theater.of.Operations == "PTO")


bomber_data |>
  group_by(Theater.of.Operations) |>
  count(Airborne.Aircraft)



## Close look at Germany ----
germany_bombed <- bomber_data |>
  filter(Target.Country == "GERMANY") |>
  filter(
    between(Target.Latitude, 45, 57) &
    between(Target.Longitude, 0, 20)
  )



ggplot() +
  geom_polygon(data = map_data("world", region = "Germany"), aes(x = long, y = lat, group = group), fill = "gray80") +
  geom_point(
    data = germany_bombed, 
    aes(
      x = Target.Longitude, 
      y = Target.Latitude, 
      color = Country, 
      size = Total.Weight..Tons.), 
    alpha = 0.01) +
  coord_map() +
  labs(
    title = "Allied Locations of Bombardment during WW2 in Germany",
    x = "Longitude",
    y = "Latitude",
    color = "Country",
    size = "Tonnage of Munitions per Bombardment"
  ) +
  theme_light()





berlin_bombed <- germany_bombed |>
  filter(
    between(Target.Latitude, 52, 53) &
      between(Target.Longitude, 12.75, 14.25)
  )

top_7_industries <- berlin_bombed %>%
  count(Target.Industry) %>%
  arrange(desc(n)) %>%
  head(7) %>%
  pull(Target.Industry)

# Filter the data based on the top 7 industries
berlin_bombed_top7 <- berlin_bombed %>%
  filter(Target.Industry %in% top_7_industries)
  
ggplot() +
  geom_polygon(data = map_data("world", region = "Germany"), aes(x = long, y = lat, group = group), fill = "gray80") +
  geom_point(
    data = berlin_bombed_top7,
    aes(
      x = Target.Longitude,
      y = Target.Latitude,
      color = Target.Industry,
      size = Total.Weight..Tons.
    ),
    alpha = 0.08
  ) +
  coord_map(xlim = c(12.75, 14.25), ylim = c(52, 53)) + 
  labs(
    title = "Allied Locations of Bombardment during WW2 in Berlin",
    x = "Longitude",
    y = "Latitude",
    color = "Target Industry",
    size = "Tonnage of Munitions per Bombardment"
  ) +
  theme_light()
  
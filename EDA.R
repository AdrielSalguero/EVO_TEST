library(dplyr)
library(geosphere)
library(lubridate)
library(tidyr)

#I started opening the csv, applying header and  sep to anticipate error like more columns than names and some warnings # nolint

store_master <- read.csv("c:/test_input/store_master.csv", header = TRUE, sep= ';') # nolint
sales_data <- read.csv("c:/test_input/sales_data.csv", header=TRUE, sep= ';') # nolint
cities <- read.csv("c:/test_input/cities.csv", header = TRUE, sep=",")
weather <- read.csv("c:/test_input/weather.csv", header = TRUE, sep=",")
holiday_uk <-read.csv("c:/test_input/festivos_2017_2019.csv", header = TRUE, sep= ",")


# Avoiding errors, I checked for nulls, and explore types of data
print(sum(is.na(cities)))
print(sum(is.na(store_master)))
print(str(cities))
print(str(store_master))

# I had to change some structures of data (types and lenght) here:
store_master$latitude <- as.numeric(gsub(",", ".", store_master$latitude))
store_master$longitude <- as.numeric(gsub(",", ".", store_master$longitude))
store_master$latitude <- round(store_master$latitude, 2)
store_master$longitude <- round(store_master$longitude, 2)

# Add the column "asignacion" to designate the nearest city
store_master$asignacion <- NA

# Function to find the nearest city
findNearestCityID <- function(store_lat, store_lon, cities) {

  distances <- distm(cbind(store_lon, store_lat), cbind(cities$longitude, cities$latitude))
  
  nearest_row <- which.min(distances)
  
#Obtein the nearest ID
  nearest_city_id <- cities$id[nearest_row]
  
  return(nearest_city_id)
}

# I walk the row to asginate the nearest city
for (i in 1:nrow(store_master)) {
  store_lat <- store_master$latitude[i]
  store_lon <- store_master$longitude[i]

  nearest_city_id <- findNearestCityID(store_lat, store_lon, cities)
  
  #Add to "asignacion" on store_master
  store_master$asignacion[i] <- nearest_city_id
}

#I asigned the store to our sales

# Join betwen store_master y sales_data
r1 <- merge(store_master, sales_data, by = "store", all.x = TRUE)

r1 <- r1[, c("date", "store", "item", "qty", "unit_price","item_category", "asignacion")]

r2 <- merge(r1, weather, by.x = c("date", "asignacion"), by.y = c("date", "id"),  all.x = TRUE)

f_df <- merge(r2, holiday_uk, by= "date", all.x = TRUE)
f_df <-f_df[, c("date","store", "item", "qty",  "unit_price",  "item_category", "TMAX", "PRCP", 'holiday_uk')]

f_df$holiday_uk[is.na(f_df$holiday_uk)] <- 0
# Change data types
f_df$date <- as.Date(f_df$date)
f_df$store <- as.character(f_df$store)
f_df$item <- as.character(f_df$item)
f_df$unit_price <- as.numeric(gsub(",", ".", f_df$unit_price))

#Check this merge ()
print(head(f_df),10)

#On this section, we start our EDA.

print("Summary")
print(summary(f_df))

print("Data type")
print(dim(f_df))

# Count NA
tmax_na_count <- tapply(is.na(f_df$TMAX), f_df$store, sum)
prcp_na_count <- tapply(is.na(f_df$PRCP), f_df$store, sum)
stores <- unique(f_df$store)


fecha_final <- as.Date("2019-03-30")
fecha_inicial <- "2017-03-30"
f_df_filtered <- f_df[year(f_df$date) >= fecha_inicial & f_df$date <= fecha_final, ]
fecha_inicial_mes <- "2018-10-01"
f_df_6month <- f_df[year(f_df$date) >= fecha_inicial_mes & f_df$date <= fecha_final, ]


# 1- graph our top 10
top_10_items <- f_df_filtered %>%
  group_by(item) %>%
  summarize(total_qty = sum(qty)) %>%
  arrange(desc(total_qty)) %>%
  top_n(10)

total_items_sold <- sum(f_df_filtered$qty)

top_10_items <- top_10_items %>%
  mutate(percentage = total_qty / total_items_sold * 100)

# 2- Top 10 items using our sales
top_10_items_revenue <- f_df_filtered %>%
  mutate(total_revenue = qty * unit_price) %>%
  group_by(item) %>%
  summarize(total_sales = sum(total_revenue)) %>%
  arrange(desc(total_sales)) %>%
  top_n(10)

# 3. Top 10 of items on the last 6 months

top_items_last_6_months <- f_df_6month %>%
  group_by(item) %>%
  summarize(total_qty = sum(qty)) %>%
  arrange(desc(total_qty)) %>%
  top_n(10)


# 5. Top 10 productos each month
top_items_per_month <- f_df_filtered %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month, item) %>%
  summarize(total_qty = sum(qty)) %>%
  arrange(month, desc(total_qty)) %>%
  group_by(month) %>%
  slice_max(n = 10, order_by = total_qty) %>%
  mutate(rank = row_number()) %>%
  pivot_wider(names_from = rank, values_from = item, names_prefix = "item") %>%
  ungroup() %>%
  arrange(month)


# 6- Top 10 of items with most sales for season

seasons <- c(1, 2, 3, 4)

f_df_filtered <- f_df_filtered %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ seasons[1],
    month(date) %in% c(3, 4, 5) ~ seasons[2],
    month(date) %in% c(6, 7, 8) ~ seasons[3],
    month(date) %in% c(9, 10, 11) ~ seasons[4]
  ))

top_items_per_season <- f_df_filtered %>%
  group_by(year = format(date, "%Y"), season, item) %>%
  summarize(total_qty = sum(qty)) %>%
  arrange(year, season, desc(total_qty)) %>%
  group_by(year, season) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10) %>%
  pivot_wider(names_from = rank, values_from = item, names_prefix = "item_") %>%
  ungroup() %>%
  select(year, season, starts_with("item_")) %>%
  arrange(year, season)  

write.csv(f_df_filtered, file = "archivo.csv", row.names = FALSE)


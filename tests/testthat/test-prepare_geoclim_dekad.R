library(cdms.products)
library(dplyr)
library(tidyr)
library(lubridate)

niger51 <- daily_niger %>%
  dplyr::filter(year == 1951)

niger_dekad <- niger51 %>%
  mutate(dekad = dekad(date)) %>%
  group_by(station_name, year, dekad) %>%
  summarise(rain = sum(rain))

df_dekad <- left_join(niger_dekad, stations_niger, by = "station_name")
r1 <- df_dekad %>% 
  rename(lon = long) %>%
  pivot_wider(id_cols = c(id, lat, lon, year), names_from = dekad,
              values_from = rain)

test_that("prepre_climdex correctly formats dekad data with metadata included", {
  t1 <- prepare_geoclim_dekad(data = df_dekad, year = "year", dekad,
                        element = "rain", station_id = "id", latitude = "lat",
                        longitude = "long", type = "dekad")
  expect_equal(t1, r1)
})

test_that("prepre_climdex correctly formats dekad data with metadata separate", {
  t1 <- prepare_geoclim_dekad(data = niger_dekad, year = "year", dekad, 
                                    element = "rain", station_id = "id",
                                    latitude = "lat", longitude = "long", 
                                    metadata = stations_niger, join_by = "station_name", 
                                    add_cols = NULL)

  expect_equal(t1, r1)
})


prepare_geoclim_dekad(data = niger_dekad, year = "year", dekad = "dekad" , element = "rain", station_id = "id",
                                  latitude = "lat", longitude = "long", metadata = stations_niger,
                                  join_by = "station_name", add_cols = NULL)

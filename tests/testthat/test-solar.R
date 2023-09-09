test_that("sol_julian_day.defaut", {
  datetime <- as.POSIXlt("2018-9-29")
  expect_equal(sol_julian_day(datetime), 272)
})

test_that("sol_julian_day.weather_station", {
  datetime <- as.POSIXlt(c("2018-9-29", "2018-9-30"))
  weather_station <- build_weather_station(datetime = datetime)
  expect_equal(sol_julian_day(weather_station), c(272, 273))
})

#test_that("sol_elevation", {
#  expect_equal(sol_elevation(datetime, lat, lon), 36.98267, tolerance = 1e-5)
#})

#test_that("sol_azimuth", {
#  expect_equal(sol_azimuth(datetime, lat, lon), 179.1576, tolerance = 1e-5)
#})

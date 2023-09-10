# test data
t1 <- 22
t2 <- 21
t_pot <- 23.1132
hum1 <- 89
hum2 <- 88
elev <- 200
z1 <- 2
z2 <- 10
p_vapor <- 23.83956
p1 <- 1000
p2 <- 1000


test_that("hum_sat_vapor_pres", {
  expect_equal(hum_sat_vapor_pres(t = t1), 26.78602, tolerance = 1e-3)
})

test_that("hum_vapor_pres", {
  expect_equal(hum_vapor_pres(hum = hum1, t = t1), 23.83956, tolerance = 1e-3)
})

test_that("hum_specific", {
  expect_equal(hum_specific(p_vapor = p_vapor, p = 1000), 0.0148282, tolerance = 1e-4)
})

test_that("hum_absolute", {
  expect_equal(hum_absolute(p_vapor = p_vapor, t_pot = t_pot), 0.01743569, tolerance = 1e-4)
})

test_that("hum_evap_heat", {
  expect_equal(hum_evap_heat(t = t1), 2448643, tolerance = 1e-4)
})

test_that("hum_precipitable_water works in northern hemisphere", {
  t0 = c(300, 294, 272.2, 287, 257.1)
  pwst = c(4.1167, 2.9243, 0.8539, 2.0852, 0.4176)
  
  datetime <- as.POSIXlt(c("2023-09-10", "2023-09-10", "2023-12-10", "2023-09-10", "2023-12-10"))
  lat <- c(5, 35, 35, 65, 65)
  a <- c()
  for (i in seq_along(lat)) {
    weather_station <- build_weather_station(
      datetime = datetime[i],
      lat = lat[i],
      elev = 0,
      temp = 20
    )
    expect_equal(
      hum_precipitable_water(weather_station),
      pwst[i] * (t0[i] / weather_station$temp)^0.5
    )
  }
})

test_that("hum_precipitable_water works in southern hemisphere", {
  t0 = c(300, 294, 272.2, 287, 257.1)
  pwst = c(4.1167, 2.9243, 0.8539, 2.0852, 0.4176)
  
  datetime <- as.POSIXlt(c("2023-09-10", "2023-09-10", "2023-12-10", "2023-09-10", "2023-12-10"))
  lat <- -c(5, 35, 35, 65, 65)
  a <- c()
  for (i in seq_along(lat)) {
    weather_station <- build_weather_station(
      datetime = datetime[i],
      lat = lat[i],
      elev = 0,
      temp = 20
    )
    a[i] <- hum_precipitable_water(weather_station)
  }
  expect_equal(
    a,
    c(
      pwst[1] * (t0[1] / weather_station$temp)^0.5,
      pwst[3] * (t0[3] / weather_station$temp)^0.5,
      pwst[2] * (t0[2] / weather_station$temp)^0.5,
      pwst[5] * (t0[5] / weather_station$temp)^0.5,
      pwst[4] * (t0[4] / weather_station$temp)^0.5
    )
  )
})

test_that("hum_moisture_gradient", {
  expect_equal(hum_moisture_gradient(hum1, hum2, t1, t2, p1, p2),
    -0.00013045,
    tolerance = 1e-6
  )
})

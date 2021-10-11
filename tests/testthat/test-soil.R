context("Soil functions")

# test data
s_Moist <- 13           # soil moisture in volume percent
s_flag <- 3             # 1 = sand, 2 = clay, 3 = silt
z1 <- 0.1               # soil temperature in 1 m depth
z2 <- 0.5               # soil temperature in 2 m depth
t1 <- 15                # temperature on z1 in °C
t2 <- 10                # temperature on z2 in °C
thermal_cond <- 1.699
vol_heat_cap <- 1.726

# values for linear interpolation:
sand_cond <- c(0.269,1.46,1.98,2.18,2.31,2.49,2.58)
clay_cond <- c(0.276,0.586,1.1,1.43,1.57,1.74,1.95)
moisture <- c(0, 5, 10, 15, 20, 30, 43)

test_that("soil_thermal_cond", {
  expect_equal(soil_thermal_cond(moisture, texture = "sand"), sand_cond)
  expect_equal(soil_thermal_cond(moisture, texture = "clay"), clay_cond)
  expect_equal(soil_thermal_cond(c(NA, -10), texture = "sand"), c(NA, NA_integer_))
  expect_equal(soil_thermal_cond(c(NA, -10), texture = "clay"), c(NA, NA_integer_))
  expect_error(soil_thermal_cond(moisture, texture = "error"))
})

# values for linear interpolation:
sand_cap <- c(1.17,1.38,1.59,1.8,2.0,2.42,2.97)
clay_cap <- c(1.19,1.4,1.61,1.82,2.03,2.45,2.99)

test_that("soil_heat_cap", {
  expect_equal(soil_heat_cap(moisture, texture = "sand"), sand_cap)
  expect_equal(soil_heat_cap(moisture, texture = "clay"), clay_cap)
  expect_equal(soil_heat_cap(c(NA, -10), texture = "sand"), c(NA, NA_integer_))
  expect_equal(soil_heat_cap(c(NA, -10), texture = "clay"), c(NA, NA_integer_))
  expect_error(soil_heat_cap(moisture, texture = "error"))
})

test_that("soil_heat_flux", {
  expect_equal(soil_heat_flux(ts1 = z1, ts2 = z2, depth1 = 1, depth = 2, thermal_cond = thermal_cond),
               0.6796, tolerance = 1e-3)
})

test_that("soil_attenuation", {
  expect_equal(soil_attenuation(thermal_cond = thermal_cond, vol_heat_cap = vol_heat_cap),
               0.1645349747, tolerance = 1e-3)
})


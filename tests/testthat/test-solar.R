context("Solar")

#test_that("sol_eccentricity", {
#  expect_equal(sol_eccentricity(datetime), 0.9965642, tolerance = 1e-5)
#})

#test_that("sol_day_angle", {
#  expect_equal(sol_day_angle(datetime), 0.9965642, tolerance = 1e-5)
#})

test_that("sol_julian_day", {
  datetime <- as.POSIXlt("2018-9-29")
  expect_equal(sol_julian_day(datetime), 272)
})

#test_that("sol_elevation", {
#  expect_equal(sol_elevation(datetime, lat, lon), 36.98267, tolerance = 1e-5)
#})

#test_that("sol_azimuth", {
#  expect_equal(sol_azimuth(datetime, lat, lon), 179.1576, tolerance = 1e-5)
#})

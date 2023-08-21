context("Radiation")

# Test data
t <- 20.8
hum1 <- 89
st <- 15 # surface temperature
p <- 1000
elev <- 200
datetime <- as.POSIXlt("2018-9-29 11:12:00", tz = "CET")
lat <- 50.84050277777778
lon <- 8.683303333333333
sky_view <- 0.8
em <- 0.95
tges <- 0.6058589
slope <- 20
sol_azimuth <- 179.1576
sol_elevation <- 36.98267
hex <- 180

test_that("rad_emissivity_air", {
  expect_equal(rad_emissivity_air(t = t, elev = elev), 0.9955568636562477, tolerance = 1e-5)
})

test_that("rad_lw_out", {
  expect_equal(rad_lw_out(t_surface = st), 359.64500057, tolerance = 1e-5)
})

test_that("rad_lw_in", {
  # expect_equal(rad_lw_in(0.2090643, t), 88.50861, tolerance = 1e-4)
  expect_equal(rad_lw_in(hum = hum1, t = t), 334.2968946, tolerance = 1e-4)
})

test_that("rad_sw_toa", {
  expect_equal(rad_sw_toa(datetime, lat, lon), 819.5255, tolerance = 1e-3)
})

test_that("rad_sw_in", {
  expect_equal(rad_sw_in(819.5255, tges), 496.5168, tolerance = 1e-3)
})

test_that("rad_sw_out", {
  expect_equal(rad_sw_out(rad_sw_in = 496.5168),
    99.30336,
    tolerance = 1e-3
  )
})

test_that("rad_sw_radiation_balance", {
  expect_equal(rad_sw_radiation_balance(496.5168, 74.47752),
    422.0393,
    tolerance = 1e-3
  )
})

test_that("rad_bal_total", {
  expect_equal(
    rad_bal_total(
      rad_sw_radiation_balance = 422.0393,
      rad_lw_out = 371.3726,
      rad_lw_in = 88.50861
    ),
    139.1754,
    tolerance = 1e-3
  )
})

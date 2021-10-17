context("Latent")

# test data
hum1 <- 89
hum2 <- 88
p0 <- 1013.25
p <- 1000
t1 <- 22
t2 <- 21
z1 <- 2
z2 <- 10
v1 <- 1
v2 <- 2.3
ludi <- 1.225  # air density
rad_bal <- 400
soil_flux <- 40

test_that("latent_bowen", {
  expect_equal(latent_bowen(t1 = t1, t2 = t2,
                            hum1 = hum1, hum2 = hum2,
                            p1 = p, p2 = p,
                            z1 = z1, z2 = z2,
                            rad_bal = rad_bal,
                            soil_flux = soil_flux),
               -311.6535, tolerance = 1e-3)
})

# monin test data
grad_rich_no <- -0.1573365
monin <- -63.55804
ustar <- 0.5867643

test_that("latent_monin", {
  expect_equal(latent_monin(hum1, hum2, t1, t2,
                            p1 = p, p2 = p, z1, z2,
                            monin, ustar, grad_rich_no),
               156.56204, tolerance = 1e-3)
})

test_that("latent_priestley_taylor", {
  expect_equal(latent_priestley_taylor(t = t1,
                                       rad_bal = rad_bal,
                                       soil_flux = soil_flux),
               -492.8, tolerance = 1e-1)
})

# Penman test data
v <- 4.72
hum <- 59
t <- 22.0
rad_bal <- 675
z <- 2.2
lat <- -35.37
lon <- 71.5946
elev <- 124

datetime <- as.POSIXlt("2007-12-25 10:30:00")

test_that("latent_penman", {
  expect_equal(latent_penman(datetime,
                             v, t, hum, z, rad_bal,
                             elev, lat, lon),
               -407.4534, tolerance = 1e-3)
})

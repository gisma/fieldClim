context("Sensible")

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
ludi <- 1.225
rad_bal <- 400
soil_flux <- 40
grad_rich_no <- -0.1573365
monin <- -63.55804
ustar <- 0.5867643

test_that("sensible_bowen", {
  expect_equal(
    sensible_bowen(
      t1 = t1, t2 = t2,
      hum1 = hum1, hum2 = hum2,
      p1 = p, p2 = p,
      z1 = z1, z2 = z2,
      rad_bal = rad_bal, soil_flux = soil_flux
    ),
    -128.3694,
    tolerance = 1e-3
  )
})

test_that("sensible_monin", {
  expect_equal(
    sensible_monin(t1, t2,
      p1 = p, p2 = p,
      monin = monin,
      ustar = ustar,
      grad_rich_no = grad_rich_no
    ),
    2822.372084,
    tolerance = 1e-3
  )
})

test_that("sensible_priestley_taylor", {
  expect_equal(sensible_priestley_taylor(t = t1, rad_bal, soil_flux),
    -84.21671594,
    tolerance = 1e-3
  )
})

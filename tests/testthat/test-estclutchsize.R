######################################
#####  Test argument stops        ####
######################################

test_that("Dataframe properly input", {
  expect_error(nw.estclutchsize(), )

  df <- data.frame(Attempt.ID = c(1, 2, 3))
  expect_error(nw.estclutchsize(data = df), "Augument 'data' must be a dataframe of merged NestWatch attempts and visits data.")
})

test_that("Output is properly input", {
  df <- data.frame(Species.Code = c("norcar", "carwre", "easblu"),
                   Visit.ID = c(1, 2, 3))
  expect_error(nw.estclutchsize(data = df, output = 10), "Augument 'output' must be a character vector.")
})

######################################
#####  Test function              ####
######################################

data <- data.frame(Attempt.ID            = c(1, 2,  3,  4,  5,  6),
                   Visit.ID              = c(1, 2,  3,  4,  5,  6),
                   Clutch.Size           = c(1, NA, NA, NA, NA, NA) ,
                   Host.Eggs.Count       = c(1, 2,  NA, NA, NA, 3),
                   Young.Total           = c(1, 2,  3,  3,  4,  NA),
                   Live.Host.Young.Count = c(1, 1,  2,  3,  1,  1),
                   Unhatched.Eggs        = c(0, 0,  NA, NA, 1,  3),
                   Young.Fledged         = c(1, 0,  NA, NA, 1,  3),
                   Dead.Host.Young.Count = c(0, NA, NA, 1,  0,  1))

test_that("Max clutch is estimating correctly", {
  # In this example, expect ID 1 = 1, 2 = 2, ...
  nw.estclutchsize(data = data, output = "output")
  expect_equal(output[1, "Clutch.Size"], 1)
  expect_equal(output[2, "Clutch.Size"], 2)
  expect_equal(output[3, "Clutch.Size"], 3)
  expect_equal(output[4, "Clutch.Size"], 4)
  expect_equal(output[5, "Clutch.Size"], 5)
  expect_equal(output[6, "Clutch.Size"], 6)
})

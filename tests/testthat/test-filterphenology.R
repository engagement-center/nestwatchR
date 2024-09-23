######################################
#####  Test argument stops        ####
######################################

data <- data.frame(Attempt.ID = c(1, 2, 3),
                   Species.Code = c("easblu", "eastow", "norcar"),
                   Visit.ID = c(1, 2, 3))

test_that("dataframe is properly input", {
  expect_error(nw.filterphenology(), )
  expect_error(nw.filterphenology(data = data.frame(Attempt.ID = c(1, 2, 3))), )
})

test_that("Mode is properly input", {
  expect_error(nw.filterphenology(data = data, sp = "norcar"), "Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  expect_error(nw.filterphenology(data = data, sp = "norcar", mode = "badmode"), "Invalid 'mode'. Please provide either 'flag' or 'remove'.")
})

test_that("Phenology is properly input", {
  expect_error(nw.filterphenology(data = data, sp = "norcar", mode = "flag"), )
  expect_error(nw.filterphenology(data = data, sp = "norcar", mode = "flag", phenology = "sometext"), )
  expect_error(nw.filterphenology(data = data, sp = "norcar", mode = "flag", phenology = c(1, 2, 3)), )
  })

test_that("Dates are dates", {
  data <- data.frame(Attempt.ID = c(1, 2, 3),
                     Species.Code = c("easblu", "eastow", "norcar"),
                     Visit.ID = c(1, 2, 3),
                     First.Lay.Date = as.Date(rep("2022-01-01", 3)),
                     Hatch.Date = as.Date(rep("2022-01-10", 3)),
                     Fledge.Date = (rep("2022-01-20", 3)))
  expect_error(nw.filterphenology(data = data, sp = "norcar", mode = "flag", phenology = c(1, 2, 3)), )
})

test_that("Datetimes are datetimes", {
  data <- data.frame(Attempt.ID = c(1, 2, 3),
                     Species.Code = c("easblu", "eastow", "norcar"),
                     Visit.ID = c(1, 2, 3),
                     First.Lay.Date = as.Date(rep("2022-01-01", 3)),
                     Hatch.Date = as.Date(rep("2022-01-10", 3)),
                     Fledge.Date = as.Date(rep("2022-01-20", 3)),
                     Visit.Datetime = rep("2022-01-20 12:12:12", 3))
  expect_error(nw.filterphenology(data = data, sp = "norcar", mode = "flag", phenology = c(1, 2, 3, 4)), )
})


######################################
#####  Test setup                 ####
######################################

data <- data.frame(Attempt.ID = c(1, 2, 3),
                   Species.Code = c("easblu", "eastow", "norcar"),
                   Visit.ID = c(1, 2, 3),
                   First.Lay.Date = as.Date(rep("2022-01-01", 3)),
                   Hatch.Date = as.Date(rep("2022-01-10", 3)),
                   Fledge.Date = as.Date(rep("2022-01-20", 3)),
                   Visit.Datetime = rep(as.POSIXct("2022-01-20 12:12:12"), 3))


#test_that("set up is setting up", {
#  expect_null(all(Flagged.Attempt, Attempt.ID, Species.Code, First.Lay.Date, Hatch.Date,
#                  Fledge.Date, Visit.Datetime, max_date, min_date, date_difference))


#})

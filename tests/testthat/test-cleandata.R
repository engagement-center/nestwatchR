######################################
#####  Test argument stops        ####
######################################

test_that("Mode is properly input", {
  expect_error(nw.cleandata(), "Invalid 'mode'. Please provide either 'flag' or 'remove'.")
  expect_error(nw.cleandata(mode = "bad mode"), "Invalid 'mode'. Please provide either 'flag' or 'remove'.")
})

test_that("Methods is properly input", {
  expect_error(nw.cleandata(mode = "remove"), )
  expect_error(nw.cleandata(mode = "flag", method = "bad"), )
})

######################################
#####  Test individual removals   ####
######################################

test_that("Method 'a'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 2, 3, 4),
                     Species.Code = c("bnhcow", "bnhcow", "carwre", "carwre", "bewwre", "btbwar"),
                     First.Lay.Date = rep("2024-01-01", 6),
                     Fledge.Date = rep("2024-01-01", 6),
                     Hatch.Date = rep("2024-01-01", 6),
                     Visit.Datetime = rep("2024-01-01 12:12:12", 6))
  nw.cleandata(data = data, mode = "remove", methods = "a", output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 3)
})

test_that("Method 'b'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 2, 3, 4),
                     Species.Code = c("bnhcow", "bnhcow", "carwre", "carwre", "bewwre", "btbwar"),
                     Outcome = c("s1", "s1", "i", "i", "u3", "n"),
                     First.Lay.Date = rep("2024-01-01", 6),
                     Fledge.Date = rep("2024-01-01", 6),
                     Hatch.Date = rep("2024-01-01", 6),
                     Visit.Datetime = rep("2024-01-01 12:12:12", 6))
  nw.cleandata(data = data, mode = "remove", methods = "b", output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 1)
})

test_that("Method 'c'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 2, 3, 4),
                     Species.Code = c("bnhcow", "bnhcow", "carwre", "carwre", "bewwre", "btbwar"),
                     Outcome = c("f5", "f5", "s1", "s1", "s1", "n"),
                     First.Lay.Date = rep("2024-01-01", 6),
                     Fledge.Date = rep("2024-01-01", 6),
                     Hatch.Date = rep("2024-01-01", 6),
                     Visit.Datetime = rep("2024-01-01 12:12:12", 6))
  nw.cleandata(data = data, mode = "remove", methods = "c", output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 3)
})

test_that("Method 'd'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 2, 3, 4),
                     Species.Code = c("bnhcow", "bnhcow", "carwre", "carwre", "bewwre", "btbwar"),
                     Outcome = c("f", "f", "f3", "f3", "f2", "s1"),
                     First.Lay.Date = rep("2024-01-01", 6),
                     Fledge.Date = rep("2024-01-01", 6),
                     Hatch.Date = rep("2024-01-01", 6),
                     Young.Fledged = c(0, 0, 3, 3, 0, 4),
                     Visit.Datetime = rep("2024-01-01 12:12:12", 6))
  nw.cleandata(data = data, mode = "remove", methods = "d", output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 3)
})

test_that("Method 'e'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 3, 4, 5),
                     Species.Code = c("norcar", "norcar", "carwre", "btgwar", "bewwre", "btbwar"),
                     Outcome = c("f", "f", "s1", "s1", "s1", "s1"),
                     Young.Fledged = c(0, 0, 0, 0, 2, 4),
                     First.Lay.Date = rep("2024-01-01", 6),
                     Fledge.Date = rep("2024-01-01", 6),
                     Hatch.Date = rep("2024-01-01", 6),
                     Visit.Datetime = rep("2024-01-01 12:12:12", 6))
  nw.cleandata(data = data, mode = "remove", methods = "e", output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 3)
})

test_that("Method 'f'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 3, 4, 5),
                     Species.Code = c("norcar", "norcar", "carwre", "btgwar", "bewwre", "btbwar"),
                     Outcome = c("s1", "s1", "s1", "s1", "s1", "s1"),
                     Young.Fledged = c(0, 0, 0, 0, 2, 4),
                     Young.Total = c(2, 2, 2, 2, 2, 2),
                     Clutch.Size = c(4, 4, 4, 3, 1, 0),
                     First.Lay.Date = rep("2024-01-01", 6),
                     Fledge.Date = rep("2024-01-01", 6),
                     Hatch.Date = rep("2024-01-01", 6),
                     Visit.Datetime = rep("2024-01-01 12:12:12", 6))
  nw.cleandata(data = data, mode = "remove", methods = "f", output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 3)
})

test_that("Method 'g'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 3, 4, 5),
                     Species.Code = c("norcar", "norcar", "carwre", "btgwar", "bewwre", "btbwar"),
                     Outcome = c("s1", "s1", "s1", "s1", "s1", "s1"),
                     Clutch.Size = c(4, 4, 4, 3, 1, 0),
                     Young.Total = c(4, 4, 4, 3, 1, 0),
                     Young.Fledged = c(3, 3, 4, 4, 0, 0),
                     First.Lay.Date = rep("2024-01-01", 6),
                     Fledge.Date = rep("2024-01-01", 6),
                     Hatch.Date = rep("2024-01-01", 6),
                     Visit.Datetime = rep("2024-01-01 12:12:12", 6))
  nw.cleandata(data = data, mode = "remove", methods = "g", output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 4)
})

test_that("Method 'h'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 3, 4, 5),
                     Species.Code = c("norcar", "norcar", "carwre", "btgwar", "bewwre", "btbwar"),
                     Subnational.Code = c("US-TX", "US-TX", "XX-", "US-NH", "US-CA", "US-NH"),
                     Outcome = c("s1", "s1", "s1", "s1", "s1", "s1"),
                     Clutch.Size = c(4, 4, 4, 3, 1, 0),
                     Young.Total = c(4, 4, 4, 3, 1, 0),
                     Young.Fledged = c(3, 3, 4, 4, 0, 0),
                     First.Lay.Date = rep("2024-01-01", 6),
                     Fledge.Date = rep("2024-01-01", 6),
                     Hatch.Date = rep("2024-01-01", 6),
                     Visit.Datetime = rep("2024-01-01 12:12:12", 6))
  nw.cleandata(data = data, mode = "remove", methods = "h", output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 4)
})

test_that("Method 'i'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 3, 4, 5),
                     Species.Code = c("norcar", "norcar", "carwre", "btgwar", "bewwre", "btbwar"),
                     Subnational.Code = c("US-TX", "US-TX", "XX-", "US-NH", "US-CA", "US-NH"),
                     Outcome = c("s1", "s1", "s1", "s1", "s1", "s1"),
                     Clutch.Size = c(4, 4, 4, 3, 1, 0),
                     Young.Total = c(4, 4, 4, 3, 1, 0),
                     Young.Fledged = c(3, 3, 4, 4, 0, 0),
                     First.Lay.Date = c("2024-05-01", "2024-05-01", "2024-05-01", "2024-05-01", "2024-05-01", "2024-05-01"),
                     Hatch.Date =     c("2024-05-10", "2024-05-10", NA,           "2025-05-10", "2024-05-10", "2024-05-10"),
                     Fledge.Date =    c("2024-05-20", "2024-05-20", "2025-05-20", "2024-05-20", "2025-05-20", "2024-05-20"),
                     Visit.Datetime = rep("2024-01-01 12:12:12", 6))
  nw.cleandata(data = data, mode = "remove", methods = "j", output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 2)
})

test_that("Method 'k'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 2, 3, 3),
                     Species.Code = c("norcar", "norcar", "carwre", "carwre", "btwb", "btbwar"),
                     Subnational.Code = c("US-TX", "US-TX", "US-GA", "US-GA", "US-NY", "US-NY"),
                     Outcome = c("s1", "s1", "s1", "s1", "s1", "s1"),
                     Clutch.Size =   c(4, 4, 4, 3, 4, 4),
                     Young.Total =   c(4, 4, 4, 3, 4, 4),
                     Young.Fledged = c(4, 4, 4, 3, 4, 4),
                     First.Lay.Date = rep("2024-05-01", 6),
                     Hatch.Date = rep("2024-05-01", 6),
                     Fledge.Date = rep("2024-05-01", 6),
                     Visit.Datetime = c("2024-05-01 12:12:12", "2022-05-10 12:12:12",
                                        "2024-05-01 12:12:12", "2024-05-10 12:12:12",
                                        "2024-05-01 12:12:12", "2024-05-12 12:12:12"))
  nw.cleandata(data = data, mode = "remove", methods = "k", output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 2)
})

test_that("Method 'a' and 'c'", {
  data <- data.frame(Attempt.ID = c(1, 2, 3),
                     Species.Code = c("bnhcow", "norcar", "carwre"),
                     Outcome = c("s1", "s1", "f5"),
                     Clutch.Size =   c(4, 4, 4),
                     Young.Total =   c(4, 4, 4),
                     Young.Fledged = c(4, 4, 4),
                     First.Lay.Date = rep("2024-05-01", 3),
                     Hatch.Date = rep("2024-05-01", 3),
                     Fledge.Date = rep("2024-05-01", 3),
                     Visit.Datetime = rep("2024-05-01 12:12:12", 3))
  nw.cleandata(data = data, mode = "remove", methods = c("a", "c"), output = "inspect")
  expect_equal(length(unique(inspect$Attempt.ID)), 1)
})


######################################
#####  Test a method for flag     ####
######################################

test_that("Method 'a'", {
  data <- data.frame(Attempt.ID = c(1, 1, 2, 2, 3, 4),
                     Species.Code = c("bnhcow", "bnhcow", "carwre", "carwre", "bewwre", "btbwar"),
                     First.Lay.Date = rep("2024-01-01", 6),
                     Fledge.Date = rep("2024-01-01", 6),
                     Hatch.Date = rep("2024-01-01", 6),
                     Visit.Datetime = rep("2024-01-01 12:12:12", 6),
                     Flagged.Attempt = NA)
  nw.cleandata(data = data, mode = "flag", methods = "a", output = "inspect")
  expect_equal(nrow(filter(inspect, Flagged.Attempt == "FLAGGED")), 2)
})


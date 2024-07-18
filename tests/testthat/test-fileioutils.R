test_that("file extension handling works", {

  expect_identical(get_file_extension("foo.bar"), ".bar")
  expect_identical(get_file_extension("foo"), "")
  expect_identical(get_file_extension(".Rprofile"), ".Rprofile")
  expect_identical(get_file_extension("foo.bar.baz"), ".baz")
  expect_identical(get_file_extension(NA_character_), NA_character_)
  expect_identical(get_file_extension(NULL), character(0))

  expect_identical(strip_file_extension("foo.bar"), "foo")
  expect_identical(strip_file_extension("foo"), "foo")
  expect_identical(strip_file_extension(".Rprofile"), "")
  expect_identical(strip_file_extension("foo.bar.baz"), "foo.bar")
  expect_identical(strip_file_extension(NA_character_), NA_character_)
  expect_identical(strip_file_extension(NULL), character(0))

})


test_that("path to filename conversion works", {

  path = "//DUNCAN-WINTER OPS DUN/FLOW-SPEC//1DAY/FLOODMODEL1/"
  f = "____DUNCAN-WINTER OPS DUN__FLOW-SPEC____1DAY__FLOODMODEL1__.dat"

  expect_identical(path_to_binary_name(path), f)
  expect_warning(path_to_binary_name("a/bad//path//"))

  expect_identical(binary_name_to_path(f), path)
  expect_warning(binary_name_to_path("a__bad____path____"))

})


test_that("timeseries binary read/write works", {

  testdf1 = data.frame(
    alt = "PA",
    path = "//DUNCAN-WINTER OPS DUN/FLOW-SPEC//1DAY/FLOODMODEL1/",
    date = seq(as.POSIXct("2021-02-04 14:00:00", tz = "UTC"),
      as.POSIXct("2021-02-11 14:00:00", tz = "UTC"), by = "1 day"),
    event = 10,
    value = rnorm(8)
  )
  outf1 = file.path(tempdir(), path_to_binary_name(testdf1$path[1]))

  testdf2 = data.frame(
    alt = "PA",
    path = "//DUNCAN-WINTER OPS DUN/FLOW-SPEC//1HOUR/FLOODMODEL1/",
    date = seq(as.POSIXct("2021-02-04 14:00:00", tz = "UTC"),
      as.POSIXct("2021-02-11 14:00:00", tz = "UTC"), by = "1 hour"),
    event = 10,
    value = rnorm(169)
  )
  outf2 = file.path(tempdir(), path_to_binary_name(testdf2$path[1]))

  # writing
  expect_equal(write_wat_binary(testdf1, tempdir()), outf1)
  expect_equal(write_wat_binary(testdf2, tempdir()), outf2)
  # multifile
  expect_equal(write_wat_binary(rbind(testdf1, testdf2), tempdir()),
    c(outf1, outf2))

  # reading
  indf1 = read_wat_binary(outf1, tz = attr(testdf1$date, "tzone"))
  indf2 = read_wat_binary(outf2, alt = testdf2$alt[1],
    path = testdf2$path[1], tz = attr(testdf1$date, "tzone"))
  expect_equal(indf1, testdf1[, 3:5], ignore_attr = "class",
    tolerance = 1e-7)
  expect_equal(indf2, testdf2, ignore_attr = "class", tolerance = 1e-7)
  expect_error(read_wat_binary(c("some/fake/path", outf1),
    tz = attr(testdf1$date, "tzone")))
  expect_warning(read_wat_binary(c("some/fake/path", outf1),
    tz = attr(testdf1$date, "tzone"), skip_missing = TRUE))
  # multifile
  indf3 = read_wat_binary(c(outf1, outf2),
    tz = attr(testdf1$date, "tzone"))
  expect_equal(indf3, rbind(testdf1, testdf2)[, 3:5],
    ignore_attr = "class", tolerance = 1e-7)
  # labels
  indf4 = read_wat_binary(c(outf1, outf2), alt = c("PA", "CCB"),
    tz = attr(testdf1$date, "tzone"))
  indf5 = read_wat_binary(c(outf1, outf2), alt = c("PA", "PA"),
    path = c(testdf1$path[1], testdf2$path[1]),
    tz = attr(testdf1$date, "tzone"))
  expect_identical(indf4$alt, c(rep("PA", nrow(testdf1)),
    rep("CCB", nrow(testdf2))))
  expect_equal(indf5, rbind(testdf1, testdf2), ignore_attr = "class",
    tolerance = 1e-7)


  # reading raw connections
  testraw1 = readBin(outf1, "raw", n = file.size(outf1))
  testraw2 = readBin(outf2, "raw", n = file.size(outf2))

  expect_equal(
    read_wat_binary(testraw1, tz = attr(testdf1$date, "tzone")),
    testdf1[, 3:5],
    ignore_attr = "class", tolerance = 1e-7
  )
  expect_equal(
    read_wat_binary(list(testraw1, testraw2), alt = c("PA", "PA"),
      path = c(testdf1$path[1], testdf2$path[1]),
      tz = attr(testdf1$date, "tzone")),
    rbind(testdf1, testdf2),
    ignore_attr = "class", tolerance = 1e-7
  )

})


test_that("summary binary read/write works", {

  testdf1 = data.frame(
    alt = "PA",
    path = "//CRT_HS/EVENT-YEAR//DATA YEARS/Hydrologic Sampler/",
    event = seq_len(10L),
    baseyear = factor(seq(1999L, 2008L))
  )
  outf1 = file.path(tempdir(), path_to_binary_name(testdf1$path[1]))

  # writing
  expect_equal(write_wat_binary(testdf1, tempdir()), outf1)

  # reading
  indf1 = read_wat_binary(outf1, format = ">i4;>i4")
  expect_identical(indf1, testdf1[, 3:4], ignore_attr = "class")

  testdf2 = data.frame(
    alt = "PA",
    path = "//CRT_HS/EVENT-YEAR//DATA YEARS/Hydrologic Sampler/",
    event = seq_len(10L),
    baseyear = factor(paste0("64-Y100-", seq(1999L, 2008L)))
  )
  outf2 = file.path(tempdir(), path_to_binary_name(testdf2$path[1]))

  # writing
  expect_equal(write_wat_binary(testdf2, tempdir()), outf2)

  # reading
  indf2 = read_wat_binary(outf2, format = ">i4;>s16")
  expect_identical(indf2, testdf2[, 3:4], ignore_attr = "class")

})

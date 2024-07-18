test_that("augment data works", {

  df1 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    run = "test",
    path = "//DUNCAN-WINTER OPS DUN/FLOW-SPEC//1DAY/FLOODMODEL1/",
    date = seq(as.POSIXct("2001-02-04 14:00:00", tz = "UTC"),
      as.POSIXct("2021-02-11 14:00:00", tz = "UTC"), length.out = 8L),
    event = 10L,
    value = rnorm(8)
  ))

  wy_df = data.frame(
    event = 10L,
    wateryear = seq(2001L, 2021L),
    wycategory = factor(rep(c("dry", "average", "wet"), c(7L, 8L, 6L)),
      c("dry", "average", "wet"))
  )
  ey_df = data.frame(
    event = 10L,
    wateryear = seq(2001L, 2021L),
    baseyear = seq(3031L, 3051L),
    baselabel = paste0("fake", seq(3031L, 3051L))
  )

  expect_error(augment_data(df1[, 1:6], wy_categories = wy_df))

  test_df1 = augment_data(df1, wy_categories = wy_df)
  test_df2 = augment_data(df1, event_years = ey_df)
  test_df3 = augment_data(df1, wy_categories = wy_df, event_years = ey_df)

  expect_identical(
    test_df1,
    cbind(df1,
      wycategory = wy_df$wycategory[c(1:3, 8:10, 16:17)])[names(test_df1)],
    ignore_attr = "row.names"
  )

  expect_identical(
    test_df2,
    cbind(df1,
      baseyear = c(3031L, 3034L, 3037L, 3039L, 3042L, 3045L, 3048L, 3051L),
      baselabel = paste0("fake", c(3031L, 3034L, 3037L, 3039L, 3042L, 3045L, 3048L, 3051L))
    )[names(test_df2)],
    ignore_attr = "row.names"
  )

  expect_identical(
    test_df3,
    cbind(df1,
      wycategory = wy_df$wycategory[c(1:3, 8:10, 16:17)],
      baseyear = c(3031L, 3034L, 3037L, 3039L, 3042L, 3045L, 3048L, 3051L),
      baselabel = paste0("fake", c(3031L, 3034L, 3037L, 3039L, 3042L, 3045L, 3048L, 3051L))
    )[names(test_df3)],
    ignore_attr = "row.names"
  )

  expect_identical(
    augment_data(df1),
    df1
  )
  expect_identical(
    augment_data(df1, NA, NA),
    df1
  )

})


test_that("prepare data works", {

  df1 = data.frame(
    alt = "PA",
    run = "test",
    path = "//DUNCAN-WINTER OPS DUN/FLOW-SPEC//1DAY/FLOODMODEL1/",
    date = seq(as.POSIXct("2021-02-04 14:00:00", tz = "UTC"),
      as.POSIXct("2021-02-11 14:00:00", tz = "UTC"), by = "1 day"),
    event = 10,
    value = rnorm(8),
    baseyear = rep(c(3005L, 3007L), c(5, 3)),
    wycategory = factor(rep(c("dry", "wet"), c(3, 5)),
      c("dry", "average", "wet"))
  )

  expect_error(prepare_data(df1, time_window = "01feb-07feb"))

  testdf1 = prepare_data(watplotter.core::standardize_dataset(df1),
    time_window = "01feb-07feb")
  testdf2 = prepare_data(watplotter.core::standardize_dataset(df1),
    wy_category = "dry")
  testdf3 = prepare_data(watplotter.core::standardize_dataset(df1),
    time_window = "01feb-07feb", wy_category = "wet")

  expect_identical(
    testdf1,
    cbind(watplotter.core::standardize_dataset(df1)[1:4, ],
      period = "01feb-07feb")[names(testdf1)],
    ignore_attr = "row.names"
  )

  expect_identical(
    testdf2,
    cbind(watplotter.core::standardize_dataset(df1)[1:3, ],
      period = "01feb-07feb")[names(testdf2)],
    ignore_attr = "row.names"
  )

  expect_identical(
    testdf3,
    cbind(watplotter.core::standardize_dataset(df1)[4, ],
      period = "01feb-07feb")[names(testdf3)],
    ignore_attr = "row.names"
  )

})

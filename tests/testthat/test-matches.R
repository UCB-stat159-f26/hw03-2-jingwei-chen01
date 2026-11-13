# Keep the first 17 fields of the assignment's example records.
female_js <- paste0(
  'var morematchmx = [["19951030","Canadian Open","Carpet","W","L",',
  '"","","WC","Q1","6-1 6-1","3","Anne Miller","149","7","","R","19770119"]];'
)
male_js <- paste0(
  'var morematchmx = [["20010910","Spain F10","Hard","S","L",',
  '"","","WC","R32","2-6 7-5 6-2","3","Guillermo Platel","731","",',
  '"","R","22.264202601"]];'
)

test_that("female records use opponent birth dates", {
  data <- clean_matches(parse_career_js(female_js), "1981-09-26")
  expected <- as.numeric(as.Date("1995-10-30") - as.Date("1977-01-19")) / 365.25
  expect_equal(nrow(data), 1L)
  expect_equal(data$opponent, "Anne Miller")
  expect_equal(data$opponent_age, expected)
  expect_equal(data$player_age, 14.09172, tolerance = 0.00001)
  expect_equal(data$age_diff, data$player_age - expected)
  expect_equal(as.character(data$result), "Loss")
})

test_that("male records retain numeric opponent ages", {
  data <- clean_matches(parse_career_js(male_js), "1986-06-03")
  expect_equal(data$opponent_age, 22.264202601)
  expect_equal(data$player_age, 15.27173, tolerance = 0.00001)
  expect_equal(data$age_diff, data$player_age - 22.264202601)
  win_js <- sub('"S","L"', '"S","W"', male_js, fixed = TRUE)
  win <- clean_matches(parse_career_js(win_js), "1986-06-03")
  expect_equal(as.character(win$result), "Win")
})

test_that("name formats and invalid inputs are handled", {
  for (name in c("serena williams", "Serena Williams", "SerenaWilliams")) {
    expect_equal(normalize_player_name(name), "SerenaWilliams")
  }
  expect_error(normalize_player_name(""), "nonempty")
  expect_error(normalize_player_name(NA_character_), "nonempty")
  expect_error(parse_career_js("invalid JS"), "Cannot parse")
  expect_error(clean_matches(list(), "1981-09-26"), "empty")
})

test_that("birth dates and missing opponent ages are handled", {
  expect_equal(extract_player_dob("var dob = 19810926;"), as.Date("1981-09-26"))
  expect_error(extract_player_dob("no birthday"), "not found")
  dates <- rep(as.Date("2000-01-01"), 3)
  expect_true(all(is.na(get_opponent_age(c("", NA, "unknown"), dates))))
})

test_that("plot contains the expected points and axes", {
  data <- clean_matches(parse_career_js(female_js), "1981-09-26")
  plot <- plot_age_difference(data)
  points <- ggplot2::ggplot_build(plot)$data[[2]]
  expect_equal(points$x, data$player_age)
  expect_equal(points$y, data$age_diff)
  expect_equal(points$colour, "#963795")
  expect_error(plot_age_difference(data.frame(x = 1)), "data frame")
})

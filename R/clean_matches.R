# Extract match records from a career JavaScript assignment.
parse_career_js <- function(js_text) {
  text <- paste(js_text, collapse = "\n")
  text <- sub("^\\s*var\\s+morematchmx\\s*=\\s*", "", text)
  text <- sub(";\\s*$", "", trimws(text))
  tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) stop("Cannot parse career match data.", call. = FALSE)
  )
}

# Interpret opponent values as birth dates or numeric ages.
get_opponent_age <- function(values, match_dates) {
  is_dob <- !is.na(values) & grepl("^[0-9]{8}$", values)
  ages <- suppressWarnings(as.numeric(values))
  birthdays <- as.Date(values[is_dob], format = "%Y%m%d")
  ages[is_dob] <- as.numeric(match_dates[is_dob] - birthdays) / 365.25
  ages[!is.finite(ages) | ages < 0 | ages > 100] <- NA_real_
  ages
}

# Convert parsed records into the data frame used for plotting.
clean_matches <- function(matches, player_dob) {
  if (!length(matches) || any(lengths(matches) < 17)) {
    stop("Match records are empty or incomplete.", call. = FALSE)
  }
  fields <- vapply(matches, function(row) {
    as.character(row[c(1, 5, 12, 17)])
  }, character(4))
  dates <- as.Date(fields[1, ], format = "%Y%m%d")
  player_age <- as.numeric(dates - as.Date(player_dob)) / 365.25
  opponent_age <- get_opponent_age(fields[4, ], dates)
  data.frame(
    match_date = dates, opponent = fields[3, ],
    player_age = player_age, opponent_age = opponent_age,
    age_diff = player_age - opponent_age,
    result = factor(fields[2, ], levels = c("W", "L"),
                    labels = c("Win", "Loss"))
  )
}

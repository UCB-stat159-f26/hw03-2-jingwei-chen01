# Accept spaced names or an existing CamelCase player identifier.
normalize_player_name <- function(player_name) {
  if (!is.character(player_name) || length(player_name) != 1 ||
      is.na(player_name) || !nzchar(trimws(player_name))) {
    stop("Provide one nonempty player name.", call. = FALSE)
  }
  words <- strsplit(trimws(player_name), "\\s+")[[1]]
  if (length(words) == 1) return(words)
  words <- tolower(words)
  paste0(toupper(substr(words, 1, 1)), substring(words, 2), collapse = "")
}

# Report download failures without confusing them with parsing errors.
fetch_text <- function(url) {
  tryCatch(
    readLines(url, warn = FALSE, encoding = "UTF-8"),
    error = function(e) stop(
      "Could not retrieve player data. Check the name and internet connection.",
      call. = FALSE
    )
  )
}

# Extract the player's date of birth from the profile page.
extract_player_dob <- function(page) {
  text <- paste(page, collapse = "\n")
  pattern <- "\\bvar\\s+dob\\s*=\\s*['\"]?([0-9]{8})"
  found <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
  if (length(found) < 2) stop("Player birth date not found.", call. = FALSE)
  dob <- as.Date(found[2], format = "%Y%m%d")
  if (is.na(dob)) stop("Invalid player birth date.", call. = FALSE)
  dob
}

# Choose the profile endpoint using the opponent age field.
get_profile_endpoint <- function(matches) {
  values <- vapply(matches, function(row) as.character(row[[17]]), character(1))
  if (any(grepl("^[0-9]{8}$", values))) "wplayer.cgi" else "player.cgi"
}

#' Retrieve a Player's Career Matches
#'
#' @param player_name Player name, such as "serena williams" or "RafaelNadal".
#' @return A data frame with match dates, opponents, ages, age differences,
#'   and Win/Loss results.
#' @export
get_player_matches <- function(player_name) {
  player_id <- normalize_player_name(player_name)
  base_url <- "https://www.tennisabstract.com/"
  encoded_id <- utils::URLencode(player_id, reserved = TRUE)
  career_url <- paste0(base_url, "jsmatches/", encoded_id, "Career.js")
  matches <- parse_career_js(fetch_text(career_url))
  if (!length(matches) || any(lengths(matches) < 17)) {
    stop("No valid career matches found for this player.", call. = FALSE)
  }
  endpoint <- get_profile_endpoint(matches)
  profile_url <- paste0(base_url, "cgi-bin/", endpoint, "?p=", encoded_id)
  dob <- extract_player_dob(fetch_text(profile_url))
  result <- clean_matches(matches, dob)
  attr(result, "player_name") <- player_name
  result
}

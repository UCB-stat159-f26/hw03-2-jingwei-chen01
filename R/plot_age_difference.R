# Check that the input contains the required plotting columns.
validate_plot_data <- function(data) {
  required <- c("player_age", "age_diff", "result")
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    stop("Provide a data frame returned by get_player_matches().", call. = FALSE)
  }
  if (!is.numeric(data$player_age) || !is.numeric(data$age_diff)) {
    stop("Age columns must be numeric.", call. = FALSE)
  }
}

#' Plot Career Match Age Differences
#'
#' @param data A data frame returned by get_player_matches().
#' @return A ggplot scatter plot colored by match outcome.
#' @export
plot_age_difference <- function(data) {
  validate_plot_data(data)
  ggplot2::ggplot(data, ggplot2::aes(
    x = player_age, y = age_diff, color = result
  )) +
    ggplot2::geom_hline(yintercept = 0, color = "gray60") +
    ggplot2::geom_point(size = 1.5, alpha = 0.85, na.rm = TRUE) +
    ggplot2::scale_color_manual(
      values = c("Win" = "#40C5E8", "Loss" = "#963795")
    ) +
    ggplot2::scale_x_continuous(position = "top") +
    ggplot2::labs(
      title = "Career Match Age Differences",
      x = "Player age (years)",
      y = "Player age minus opponent age (years)",
      color = NULL, caption = "Source: Tennis Abstract"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")
}

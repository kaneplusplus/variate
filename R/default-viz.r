
#' @importFrom dplyr %>% select
#' @importFrom gtsummary tbl_summary
v_x_table <- function(d, x_term, y_term, by) {
  d %>%
    select(x_term, by) %>%
    tbl_summary(by = by, missing = "always", missing_text = "NA")
}

#' @importFrom ggstatsplot ggscatterstats grouped_ggscatterstats
v_scatter <- function(d, x_term, y_term, by) {
  if (!is.null(by)) {
    ggscatterstats(
      d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)),
      results.subtitle = FALSE
    )
  } else {
    grouped_ggscatterstats(
      d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)), 
      grouping.var = !!sym(eval(by)),
      results.subtitle = FALSE
    )
  }
}

#' @importFrom ggstatsplot ggbetweenstats grouped_ggbetweenstats
v_between_x_fac_y_num  <- function(d, x_term, y_term, by) {
  if (is.null(by)) {
    ggbetweenstats(
      data = d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)),
      results.subtitle = FALSE
    )
  } else {
    grouped_ggbetweenstats(
      data = d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)), 
      grouping.var = !!sym(eval(by)),
      results.subtitle = FALSE
    )
  }
}

#' @importFrom ggstatsplot ggbetweenstats grouped_ggbetweenstats
v_between_x_num_y_fac <- function(d, x_term, y_term, by) {
  if (is.null(by)) {
    ggbetweenstats(
      data = d, 
      x = !!sym(eval(y_term)), 
      y = !!sym(eval(x_term)),
      results.subtitle = FALSE
    )
  } else {
    grouped_ggbetweenstats(
      data = d, 
      x = !!sym(eval(y_term)),
      y = !!sym(eval(x_term)),
      grouping.var = !!sym(eval(by)),
      results.subtitle = FALSE
    )
  }
}

#' @importFrom ggstatsplot ggbarstats grouped_ggbarstats
v_barstats <- function(d, x_term, y_term, by) {
  if (is.null(by)) {
    ggbarstats(
      data = d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)),
      results.subtitle = FALSE
    )
  } else {
    grouped_ggbarstats(
      data = d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)),
      by = !!sym(eval(by)),
      results.subtitle = FALSE
    )
  }
}

v_survplot <- function(d, x_term, y_term, by) {
  browser()
}

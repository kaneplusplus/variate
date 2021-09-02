
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
    p <- ggscatterstats(
      d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)),
      results.subtitle = FALSE
    )
  } else {
    p <- grouped_ggscatterstats(
      d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)), 
      grouping.var = !!sym(eval(by)),
      results.subtitle = FALSE
    )
  }
  p
}

#' @importFrom ggstatsplot ggbetweenstats grouped_ggbetweenstats
v_between_x_fac_y_num  <- function(d, x_term, y_term, by) {
  if (is.null(by)) {
    p <- ggbetweenstats(
      data = d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)),
      results.subtitle = FALSE
    )
  } else {
    p <- grouped_ggbetweenstats(
      data = d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)), 
      grouping.var = !!sym(eval(by)),
      results.subtitle = FALSE
    )
  }
  p
}

#' @importFrom ggstatsplot ggbetweenstats grouped_ggbetweenstats
v_between_x_num_y_fac <- function(d, x_term, y_term, by) {
  if (is.null(by)) {
    p <- ggbetweenstats(
      data = d, 
      x = !!sym(eval(y_term)), 
      y = !!sym(eval(x_term)),
      results.subtitle = FALSE
    )
  } else {
    p <- grouped_ggbetweenstats(
      data = d, 
      x = !!sym(eval(y_term)),
      y = !!sym(eval(x_term)),
      grouping.var = !!sym(eval(by)),
      results.subtitle = FALSE
    )
  }
  p
}

#' @importFrom ggstatsplot ggbarstats grouped_ggbarstats
v_barstats <- function(d, x_term, y_term, by) {
  if (is.null(by)) {
    p <- ggbarstats(
      data = d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)),
      results.subtitle = FALSE
    )
  } else {
    p <- grouped_ggbarstats(
      data = d, 
      x = !!sym(eval(x_term)), 
      y = !!sym(eval(y_term)),
      grouping.var = !!sym(eval(by)),
      results.subtitle = FALSE
    )
  }
  p
}


#' @importFrom survminer surv_fit ggsurvplot
v_survplot <- function(d, x_term, y_term, by) {
  form_str <- paste(y_term, "~", x_term)
  if (!is.null(by)) {
    if (!is.factor(d[[by]]) & !is.character(d[[by]])) {
      stop("By argument must be coersible into a factor.")
    }
    form_str <- paste(form_str, "+", by)
    nl <- length(levels(factor(paste(d[[x_term]], d[[by]]))))
  } else {
    nl <- length(levels(factor(paste(d[[x_term]]))))
  }
  conf_int <- TRUE
  risk_table = TRUE
  ncensor_plot = TRUE
  if (nl > 5) { 
    conf_int <- FALSE
    risk_table = FALSE
    ncensor_plot = FALSE
  }
  
  form <- as.formula(form_str)
  fit <- surv_fit(form, data = d)
  ggsurvplot(
    fit, 
    data = d, 
    conf.int = conf_int, 
    risk.table = risk_table,
    ncensor.plot = ncensor_plot 
  )
}

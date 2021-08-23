
perspective_map_lookup <- 
  function(
    x_type, 
    y_type, 
    by_type,
    y_integer_as_numeric = TRUE,
    x_integer_as_numeric = TRUE
  ) {

  if (y_integer_as_numeric) {
    if (y_type == "integer") {
      y_type <- "numeric"
    }
  }
  if (x_integer_as_numeric) {
    if (x_type == "integer") {
      x_type <- "numeric"
    }
  }

  perspective_map <- get_variate_private("perspective_map")

  ret <- 
    perspective_map[
      perspective_map$y_type == y_type & 
      perspective_map$x_type == x_type &
      perspective_map$by_type == by_type,
    ]
  if (nrow(ret) == 0) {
    stop(
      "Perspective_map_lookup failure. Returning NULL.\n\t",
      "x_type : ", x_type, "\n\t",
      "y_type : ", y_type, "\n\t",
      "by_type: ", by_type,
      sep = ""
    )
  }
  ret$fun[[1]]
}

#' @importFrom dplyr filter %>%
#' @importFrom checkmate assert check_function check_character
#' @export
register_perspective <- 
  function(fun, x_type, y_type = "NULL", by_type = "NULL") {

  assert(
    check_function(fun),
    check_character(x_type),
    check_character(y_type),
    check_character(by_type),
    combine = "or"
  )

  # To avoid ambiguity in the filter call below.
  xt <- x_type
  yt <- y_type
  bt <- by_type

  perspective_map <- get_variate_private('perspective_map') %>%
    dplyr::filter( !(x_type == xt & y_type == yt & by_type == bt))

  perspective_map <- 
    bind_rows(
      perspective_map, 
      tibble(
        y_type = y_type, 
        x_type = x_type, 
        by_type = by_type, 
        fun = list(fun)
      )
    )

  assign_variate_private("perspective_map", perspective_map)
  invisible(TRUE)
}

#' Perspective Variable Tables/Visualizations by Role
#'
#' @param x a data.frame object.
#' @param form a formula containing the role relationships.
#' @importFrom forceps roles 
#' @importFrom checkmate check_data_frame check_formula
#' @importFrom dplyr select bind_rows bind_cols mutate
#' @importFrom tibble tibble
#' @importFrom purrr pmap
#' @export
perspective <- function(x, form) {

  assert(
    check_data_frame(x),
    check_formula(form),
    length(form) %in% c(2, 3),
    combine = "and"
  )

  roles <- roles(x)
 
  if (is.null(roles)) { 
    stop(
      "\nNo roles have been assigned.\n",
      "Were they accidentally dropped in a sequence of piped operations?", 
      sep = ""
    )
  }

  eta <- attributes(terms(form))
  if (nrow(eta$factors) == ncol(eta$factors)) {
    y_role <- "NULL"
    y_roles <- tibble(y_term = NA_character_, y_type = "NULL")
  } else {
    y_role <- rownames(eta$factors)[1]
    y_roles <- roles %>%
      filter(role == y_role) %>%
      select(-role) %>%
      rename(y_term = term, y_type = type)

    ll <- 
      purrr::map(
        y_roles$y_term, 
        ~ get_variable_names(parse(text = .x)[[1]])
      )

    all_ll_in_rl <- function(ll, rl) {
      all(map_lgl(ll, ~ .x %in% rl))
    }

    vars_avail <- map_lgl(ll, ~ all_ll_in_rl(.x, names(x)))
    y_roles <- y_roles[vars_avail, ]
    
  }

  assert(length(colnames(eta$factors)) == 1)
  rvars <- strsplit(colnames(eta$factors), " \\| ")
  x_role <- rvars[[1]][1]
  cond_var <- NULL
  if (length(rvars[[1]]) == 2) {
    cond_var <- rvars[[1]][2]
  } 
 
  x_roles <- roles %>%
    filter(term %in% names(x) & role == x_role) %>%
    select(-role) %>%
    rename(x_term = term, x_type = type)

  if (nrow(x_roles) == 0) {
    stop(
      "\nNo variables found corresponding to role ", x_role, ".\n",
      "Are you sure use assigned variables to the role?",
      sep = ""
    )
  }
   
  combs <- NULL
  for (i in seq_len(nrow(y_roles))) {
    for (j in seq_len(nrow(x_roles))) {
      combs <- bind_rows(combs, bind_cols(y_roles[i,], x_roles[j,]))
    }
  }
 
  if (!is.null(cond_var)) {
    combs$by_type <- class(x[[cond_var]])
  } else {
    combs$by_type <- "NULL"
  }
  combs$perspective_fun <- 
    pmap(combs %>% select(x_type, y_type, by_type), perspective_map_lookup)

  combs$perspective <- 
    pmap(combs %>% select(x_term, y_term, perspective_fun), 
      function(x_term, y_term, perspective_fun) {
        perspective_fun(x, x_term, y_term, cond_var)
      }
    )

  if (is.null(cond_var)) {
    combs$by_var <- NA_character_
  } else {
    combs$by_var <- cond_var
  }
  select(combs, y_term, x_term, by_var, perspective) %>%
    mutate(x_role = x_role, y_role = y_role)
    
}


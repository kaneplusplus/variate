
#' @importFrom checkmate check_function check_character
bv_register_renderer <- function(fun, x_type, y_type = "NA") {

  assert(
    check_function(fun),
    check_character(x_type),
    check_character(y_type),
    combine = "or"
  )
 
  render_map <- render_map %>%
    filter(x_type != x_type & y_type != y_type)

  render_map <- 
    bind_rows(
      render_map, 
      tibble(y_type = y_type, x_type = x_type, fun = list(fun))
    )
  invisible(TRUE)
}


x_table <- function(d, x_term, y_term) {
  d %>%
    select(x_term) %>%
    tbl_summary(missing = "always", missing_text = "NA")
}

bv_register_renderer(y_type = "NA", x_type = "factor", fun = x_table)

# Make this a package variable
render_map <- tibble(y_type = "NA", x_type = "factor", fun = list(x_table))

render_map_lookup <- function(x_type, y_type) {
  ret <- render_map[render_map$y_type == y_type & render_map$x_type == x_type,]
  if (nrow(ret) == 0) {
    stop("render_map_lookup failure.")
  }
  ret$fun[[1]]
}

#' Render Variable Tables/Visualizations by Role
#'
#' @param x a data.frame object.
#' @param form a formula containing the role relationships.
#' @importFrom forceps roles 
#' @importFrom checkmate check_data_frame check_formula
#' @importFrom dplyr select bind_rows bind_cols mutate
#' @importFrom tibble tibble
#' @importFrom purrr pmap
#' @export
bv_render <- function(x, form) {

  assert(
    check_data_frame(x),
    check_formula(form),
    length(form) %in% c(2, 3),
    combine = "and"
  )

  roles <- roles(x)

  eta <- attributes(terms(form))
  if (nrow(eta$factors) == ncol(eta$factors)) {
    y_role <- "NA"
    y_roles <- tibble(y_term = "NA", y_type = "NA")
  } else {
    y_role <- rownames(eta$factors)[1]
  }

  x_role <- colnames(eta$factors)
  assert(length(x_role) == 1)
  
  x_roles <- roles %>%
    filter(term %in% names(x) & role == x_role) %>%
    select(-role) %>%
    rename(x_term = term, x_type = type)
   
  combs <- NULL
  for (i in seq_len(nrow(y_roles))) {
    for (j in seq_len(nrow(x_roles))) {
      combs <- bind_rows(combs, bind_cols(y_roles[i,], x_roles[j,]))
    }
  }
  
  combs$render_fun <- 
    pmap(combs %>% select(x_type, y_type), render_map_lookup)

  combs$perspective <- 
    pmap(combs %>% select(x_term, y_term, render_fun), 
      function(x_term, y_term, render_fun) {
        render_fun(x, x_term, y_term)
      })

  select(combs, y_term, x_term, perspective) %>%
    mutate(x_role = x_role, y_role = y_role)
    
}


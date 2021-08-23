
variate_private <- new.env(parent = emptyenv())

assign_variate_private <- function(varname, value) {
  tryCatch(
    {
      assign(varname, value, pos = variate_private, inherits = FALSE)
      invisible(TRUE)
    },
    error = function(e) {
      if (exists(varname, where = variate_private, inherits = FALSE)) {
        remove(varname, envir = variate_private) 
      }
      e
    }
  )
}

get_variate_private <- function(varname) {
  if (!exists(varname, where = variate_private, inherits = FALSE)) {
    warning("Variable not found.")
  }
  variate_private[[varname]]
}

assign_variate_private(
  'perspective_map',
  tibble::tibble(
    y_type = character(),
    x_type = character(),
    by = character(),
    fun = list()
  )
)


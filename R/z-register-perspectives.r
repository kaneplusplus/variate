
register_perspective(
  y_type = "NULL", 
  x_type = "factor", 
  fun = v_x_table
)

register_perspective(
  y_type = "NULL", 
  x_type = "factor", 
  by_type = "factor",
  fun = v_x_table
)

register_perspective(
  y_type = "numeric",
  x_type = "numeric",
  by_type = "factor",
  fun = v_scatter
)

register_perspective(
  y_type = "numeric",
  x_type = "numeric",
  fun = v_scatter
)

register_perspective(
  y_type = "numeric",
  x_type = "factor",
  by_type = "factor",
  fun = v_between_x_fac_y_num 
)

register_perspective(
  y_type = "numeric",
  x_type = "factor",
  fun = v_between_x_fac_y_num 
)

register_perspective(
  y_type = "factor",
  x_type = "numeric",
  fun = v_between_x_num_y_fac
)

register_perspective(
  y_type = "factor",
  x_type = "numeric",
  by_type = "factor",
  fun = v_between_x_num_y_fac
)

register_perspective(
  y_type = "factor",
  x_type = "factor",
  fun = v_barstats
)

register_perspective(
  y_type = "factor",
  x_type = "factor",
  by_type = "factor",
  fun = v_barstats
)


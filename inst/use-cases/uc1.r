library(devtools)
document("~/projects/forceps")
document()

library(dplyr)
library(gtsummary)
library(trelliscopejs)

data(lc_adsl, lc_biomarkers, lc_adverse_events, lc_demography)

# 1. Cohort and create data_list

data_list <- list(
  endpoints = lc_adsl %>%
    mutate_if(is.character, factor),
  biomarkers = lc_biomarkers %>%
    mutate_if(is.character, factor),
  adverse_events = lc_adverse_events %>% 
    cohort("usubjid", name = "adverse_events") %>%
    mutate_if(is.character, factor),
  demography = lc_demography %>%
    mutate_if(is.character, factor)
)

# 2. Check for name collisions and fix them.

duplicated_vars(data_list, on = "usubjid")

data_list$demography <- lc_demography %>% 
    cohort("usubjid") %>%
    rename(past_chemo_stop = chemo_stop) %>%
    mutate(refractory = as.factor(refractory))

# Note: usubjid, site_id, and arm are not included.
roles <- list(
  baseline = 
    setdiff(
      union(names(data_list$demography), c(names(data_list$biomarkers))),
      c("usubjid", "site_id")
    ),
  admin = "site_id",
  adverse_event = 
    setdiff(
      names(data_list$adverse_events), 
      c("adverse_events", "usubjid")
    ),
  endpoints = c(
    "best_response",
    "chemo_stop",
    "Surv(pfs_days, pfs_censor)",
    "Surv(os_days, os_censor)"
  ),
  arm = "arm"
)

x <- data_list %>%
  consolidate(on = "usubjid") %>%
  add_roles(roles)

document("~/projects/forceps")
x %>% 
  select_role("baseline") 

roles(x)

x <- add_roles(x, roles(x))

ps <- x %>% 
  select_role("baseline") %>%
  select_if( ~ inherits(., "factor") ) %>%
  tbl_summary(missing = "always", missing_text = "NA")

ps <- x %>% 
  select_if( ~ class(.)[1] == "factor") %>%
  perspective( ~ baseline)

# arm variable
ps <- x %>% 
  select_if( ~ inherits(., "factor") ) %>%
  perspective( ~ baseline | arm)

ps <- x %>% 
  perspective( adverse_event ~ baseline | arm )


ps %>%
  filter(y_term == "best_response") %>%
  trelliscope(
    name = "Best Response",
    panel_col = "perspective",
    width = 1500
  )

# Register your own perspective.

# arm role
ps <- x %>%
  perspective( endpoints ~ baseline ) 

ps %>%
  filter(y_term == "Surv(os_days, os_censor)") %>%
  trelliscope(
    name = "OS Survival",
    panel_col = "perspective",
    width = 1000,
    height = 1000
  )

ps <- ps %>% filter(y_term == "Surv(os_days, os_censor)")

# Add two cognostics
count_nas <- function(x, y_term, x_term) {
  vars <- c(extract_var_names(y_term), extract_var_names(x_term))
  xs <- x %>%
    select(all_of(vars)) %>%
    na.omit() 
  nrow(x) - nrow(xs)
}

make_surv <- function(x, y_term, x_term) {
  surv_fit(as.formula(paste(y_term, "~", x_term)), data = x)
}

ps$na_count <- pmap_int(
  ps %>% select(y_term, x_term), 
  function(y_term, x_term) {
    partial(count_nas, x = x)(y_term, x_term)
  }
)

ps$surv <- pmap(
  ps %>% select(y_term, x_term), 
  function(y_term, x_term) {
    partial(make_surv, x = x)(y_term, x_term)
  }
)

ps$p_val <- map_dbl(ps$surv, ~ surv_pvalue(.x, data = x)$pval)

trelliscope(
  ps %>% select(y_term, x_term, perspective, p_val, na_count),
  name = "OS Survival",
  panel_col = "perspective",
  width = 1000,
  height = 1000
)


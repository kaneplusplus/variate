library(devtools)
document("~/projects/forceps")
document()

library(dplyr)
library(gtsummary)

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

x %>% 
  select_role("baseline") %>%
  select_if( ~ inherits(., "factor") ) %>%
  tbl_summary(missing = "always", missing_text = "NA")

x %>% 
  select_if( ~ class(.)[1] == "factor") %>%
  perspective( ~ baseline)

# arm variable
x %>% 
  select_if( ~ class(.)[1] == "factor") %>%
  perspective( ~ baseline | arm)

ps <- x %>% 
  perspective( adverse_event ~ baseline | arm)

# arm role
ps <- x %>%
  perspective( endpoints ~ arm ) 

ps <- x %>%
  perspective( endpoints ~ arm )
 

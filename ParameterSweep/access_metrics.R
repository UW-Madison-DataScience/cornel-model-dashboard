library(tidyverse)
library(RPostgres)
library(odbc)
library(DBI)

db <- config::get("dbtest")

con <- dbConnect(RPostgres::Postgres(),
                 dbname = db$dbname,
                 host = db$host,
                 port = db$port,
                 user = db$user,
                 password = db$password)


## Metrics with sim_id and group_number 
metrics <- dbGetQuery(con, 'Select * from metrics') %>% 
  as_tibble() %>% 
  pivot_longer(cols = -c(sim_id, group_number), 
               names_to = "metric_name", 
               values_to = "metric_value")

## Parameters for each simulation 
all_params <-
  tbl(con, "group_params") %>%
  distinct() %>%
  collect() %>% 
  mutate(across(where(bit64::is.integer64), as.numeric))

all_params_wide <-
  all_params %>%
  select(-`_scenario_name`) %>%
  pivot_wider(id_cols = sim_id,
              names_from = group_number,
              values_from = c(everything(), -sim_id, -group_number)) %>%
  mutate(across(everything(), ~ as.factor(.x)))

skim_df <- 
  all_params_wide %>% 
  skimr::skim() %>% 
  filter(factor.n_unique > 1)


## A basic combination of the two with only the parameters that are varied
metrics %>% 
  pivot_wider(names_from = "metric_name", values_from = "metric_value") %>% 
  left_join(all_params_wide %>% 
              select(sim_id, skim_df$skim_variable),
            by = "sim_id")

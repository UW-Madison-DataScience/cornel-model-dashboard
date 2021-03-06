library(tidyverse)
library(odbc)
library(DBI)

# NOTE: all metrics are computed on each replicate for each group, and then
# averaged over the replicates to provide a single measure for each sim_id /
# group combination.  

con <- dbConnect(odbc(), 
                 # driver = "PostgreSQL", # for Rstudio Connect
                 driver = "PostgreSQL Driver", # for local driver
                 server = "database-2.clbsgd2qdkby.us-east-1.rds.amazonaws.com",
                 # database = "postgres",
                 database = "test",
                 uid = "postgres",
                 # pwd = "PUT PASSWORD HERE TO KNIT THE FILE", 
                 pwd = askpass::askpass(),
                 port = 5432)

dbGetQuery(con, 'DROP TABLE IF EXISTS metrics')

dbGetQuery(
  con, 
  '
  WITH 
    measures AS (
      SELECT "sim_id", "replicate_id", "t", "group_number",
      "cumulative_mild" + "cumulative_severe" + "cumulative_outside_infections" AS "cumulative_covid_pop",
      "QI" + "E_0" + "E_1" + "E_2" + "E_3" + "E_4" + "E_5" + "E_6" + "pre_ID_0" + "pre_ID_1" + "pre_ID_2" + "pre_ID_3" + "ID_0" + "ID_1" + "ID_2" + "ID_3" + "ID_4" + "ID_5" + "ID_6" + "ID_7" + "SyID_mild_0" + "SyID_mild_1" + "SyID_mild_2" + "SyID_mild_3" + "SyID_mild_4" + "SyID_mild_5" + "SyID_mild_6" + "SyID_mild_7" + "SyID_mild_8" + "SyID_mild_9" + "SyID_mild_10" + "SyID_mild_11" + "SyID_mild_12" + "SyID_mild_13" + "SyID_mild_14" + "SyID_mild_15" + "SyID_mild_16" + "SyID_mild_17" + "SyID_mild_18" + "SyID_mild_19" + "SyID_severe_0" + "SyID_severe_1" + "SyID_severe_2" + "SyID_severe_3" + "SyID_severe_4" + "SyID_severe_5" + "SyID_severe_6" + "SyID_severe_7" + "SyID_severe_8" + "SyID_severe_9" + "SyID_severe_10" + "SyID_severe_11" + "SyID_severe_12" + "SyID_severe_13" + "SyID_severe_14" + "SyID_severe_15" + "SyID_severe_16" + "SyID_severe_17" + "SyID_severe_18" + "SyID_severe_19" AS "covid_pop",
      "QS" + "QI" AS "quar_pop"
      FROM "results"
    ),
    max_measures AS (
      SELECT 
        "sim_id", "replicate_id", "group_number", 
        MAX("covid_pop") AS "covid_pop_peak",
        MAX("cumulative_covid_pop") AS "cumulative_covid_pop_peak",
        MAX("quar_pop") AS "quar_pop_peak"
      FROM measures
      GROUP BY "sim_id", "replicate_id", "group_number"
    ),
    -- pull out the first time of the peak covid population
    covid_peak_time AS (
      SELECT a.sim_id, a.replicate_id, a.group_number, a.covid_pop, MIN(a.t) AS t
      FROM measures a
      INNER JOIN max_measures b 
          ON a.sim_id = b.sim_id
          AND a.replicate_id = b.replicate_id
          AND a.group_number = b.group_number
          AND a."covid_pop" = b."covid_pop_peak"
      GROUP BY a.sim_id, a.replicate_id, a.group_number, a.covid_pop
    ),
    -- pull out the first time of the peak quarnatine population
    quar_peak_time AS (
      SELECT a.sim_id, a.replicate_id, a.group_number, a.quar_pop, MIN(a.t) AS t
      FROM measures a
      INNER JOIN max_measures b 
          ON a.sim_id = b.sim_id
          AND a.replicate_id = b.replicate_id
          AND a.group_number = b.group_number
          AND a."quar_pop" = b."quar_pop_peak"
      GROUP BY a.sim_id, a.replicate_id, a.group_number, a.quar_pop
    ),
    replicate_metrics AS (
      SELECT 
        a."sim_id", a."replicate_id", a."group_number", a."cumulative_covid_pop_peak", 
        b."covid_pop", b."t" AS "covid_t",
        c."quar_pop", c."t" AS "quar_t"
      FROM max_measures a
      LEFT JOIN covid_peak_time b
        ON a.sim_id = b.sim_id
        AND a.replicate_id = b.replicate_id
        AND a.group_number = b.group_number 
      LEFT JOIN quar_peak_time c 
        ON a.sim_id = c.sim_id
        AND a.replicate_id = c.replicate_id
        AND a.group_number = c.group_number 
    ),
    population_sizes AS (
      SELECT DISTINCT "group_number", "population_size"
      FROM group_params
    )
  SELECT 
    "sim_id", a."group_number", 
    AVG("covid_pop") AS "Peak active cases", 
    AVG("covid_pop") / b.population_size AS "Peak % with COVID-19", 
    AVG("covid_t") AS "Time of peak COVID-19 cases", 
    AVG("cumulative_covid_pop_peak") AS "Cumulative cases",
    AVG("cumulative_covid_pop_peak") / b.population_size AS "Cumulative % with COVID-19",
    AVG("quar_pop") AS "Peak quarantine census",
    AVG("quar_pop") / b.population_size AS "Peak % in quarantine",
    AVG("quar_t") AS "Time of peak quarantine"
  INTO metrics
  FROM replicate_metrics a 
  LEFT JOIN population_sizes b
    ON a.group_number = b.group_number
  GROUP BY "sim_id", a."group_number", b."population_size"
  '
)

tmp <- dbGetQuery(con, 'Select * from metrics')
  
as_tibble(tmp) %>% 
  pivot_longer(cols = -c(sim_id, group_number), 
               names_to = "metric_name", 
               values_to = "metric_value")

# TODO: unpivot this into a table with columns "sim_id", "group_number", "metric_name", "metric_value" in SQL



library(shiny)
library(shinydashboard)
library(scales)
library(tidyverse)
library(plotly)
library(ggbeeswarm)
library(odbc)
library(DBI)
library(skimr)
library(here)
library(fishualize)

# print(odbc::odbcListDrivers())
print(odbc::odbcListDrivers()$name)
# message(odbc::odbcListDrivers())


## Initial setup ----------------------------------------------------------

print("Trying to connect to SQL db at database-2.clbsgd2qdkby.us-east-1.rds.amazonaws.com...")
## Connection to SQL database 
con <- dbConnect(odbc(), 
                 driver = "PostgreSQL", # for Rstudio Connect
                 # driver = "PostgreSQL Driver", # for local driver
                 server = "database-2.clbsgd2qdkby.us-east-1.rds.amazonaws.com",
                 # database = "postgres",
                 database = "test",
                 uid = "postgres",
                 pwd = "YxDi7HnjfpBxHKQ",
                 # pwd = "PUT PASSWORD HERE TO KNIT THE FILE", 
                 # pwd = askpass::askpass(),
                 port = 5432)

print("Connection successful!")

## Group names 
sim_params_db <- tbl(con, "sim_params")
group_names <- sim_params_db %>% 
    # filter(ntrajectories == 2) %>% 
    # filter(ntrajectories %in% c(1, 3, 20)) %>% 
    distinct(group_index, group_names) %>% 
    collect() %>% 
    mutate(group_index = as.character(group_index)) %>% 
    mutate(across(where(bit64::is.integer64), as.numeric))

group_dict <- deframe(group_names)

## Parameters that get varied across simulations 
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

readable_number <- function(x) {
  scales::number(as.numeric(as.character(x)))
}

varied_parameters <- 
    all_params_wide %>% 
    select(sim_id, skim_df$skim_variable) %>% 
    pivot_longer(cols = -sim_id) %>% 
    group_by(name) %>% 
    summarize(value = unique(value)) %>% 
    pivot_wider(values_fn = list) %>% 
    pivot_longer(cols = everything(), 
                 names_to = c("variable", "Group"),
                 names_pattern = "(.*)_([:digit:])") %>% 
    pivot_wider(names_from = variable,
                values_from = value) %>% 
    mutate(across(.cols = -Group, .fns = ~map(.x, ~readable_number(sort(.x)))))
    # mutate(across(.cols = -Group, .fns = ~map(.x, ~scales::number(as.numeric(as.character(sort(.x))))))) 

parameter_choices <- colnames(varied_parameters)[-1]

pop_sizes <- all_params %>% distinct(group_number, population_size)

theme_set(theme_minimal(base_size = 16))
point_size = 2

## Helper functions ------------------------------------------------------------

plot_fun <- function(data, y, fill, y_lab = NULL) {
    alpha <- exp(-nrow(data) / (5000 / log(2)))
  
    p <- data %>% 
        # mutate(group = paste0(group_number, ": ", group_names)) %>% 
        ggplot(aes_(~as.factor(group_number), as.name(y), fill = as.name(fill))) +
        # geom_violin(draw_quantiles = seq(0, 1, by = 0.25), scale = "width", color = "grey50") +
        geom_quasirandom(aes_(color = as.name(fill)), dodge.width = 0.5, size = point_size, alpha = alpha) +
        stat_summary(aes_(color = as.name(fill)), fun = mean, geom = "point", shape = 3, size = point_size) +
        geom_hline(yintercept = 0) +
        scale_fill_viridis_d(option = "magma", end = 0.8, begin = 0.2, aesthetics = c("color", "fill"),
                             labels = readable_number) +
        # theme_minimal() +
        theme(legend.position = "bottom") +
        labs(x = "Group",
             y = y_lab,
             title = "Results of Cornell Model across groups") 
    
    if (y %in% c("covid_frac", "cumulative_covid_frac", "Q_frac")) {
        p <- p + scale_y_continuous(labels = scales::percent)
    }
    
    p
}

# df_isolation <- read_csv(here("data/2020-10-27_summary-of-results_quarantine.csv"))
# df_cases <- read_csv(here("data/2020-10-27_summary-of-results_covid.csv"))
# TODO: consider making these reactive to the input so only the data that's needed is pulled 


# print(df_cases)
# print(df_isolation)

metric_choices <- list(
    "Cases" = c(
        "Peak active cases" = "covid_pop",
        "Peak % with COVID-19" = "covid_frac",
        "Cumulative cases" = "cumulative_covid_pop",
        "Cumulative % with COVID-19" = "cumulative_covid_frac",
        "Time of peak COVID-19 cases" = "t"
    ),
    "Isolation" = c(
        "Peak quarantine census" = "Q",
        "Peak % in quarantine" = "Q_frac",
        "Time of peak quarantine" = "t"
    )
)

## UI --------------------------------------------------------------------------

make_param_filter_ui <- function(x, var) {
  var_param_list <- x %>% select(Group, !!var) %>% deframe()

  group_selects <- 
    var_param_list %>% 
    keep(~ length(.x) > 0) %>% 
    imap(~ selectizeInput(paste0(var, "_", .y),
                          group_dict[[.y]],
                          choices = .x,
                          selected = .x,
                          multiple = TRUE)) 
  
  box(title = var, width = 3, solidHeader = TRUE, status = input_element_color,
      collapsible = TRUE, collapsed = FALSE,
      group_selects) # might need to unlist this or something 
}

# fix the styles, will over-ride later
# see https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors
input_element_color <- "primary" 
highlight_color <- "olive" 
regular_color <- "navy"
# actual colors used:
# uw madison red #da004c
# darker red #8b0037

header <- dashboardHeader(
    title = "Cornell Model Parameter Sweep Visualization",
    tags$li(a(href = "https://datascience.wisc.edu/covid19/",
              img(src = "https://hr.wisc.edu/wp-content/uploads/2019/04/uw-crest-red-300x300.png",
                  title = "AFI DSI COVID-19", height = "30px"),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown")
)

sidebar <- dashboardSidebar(
    tags$style("@import url(https://use.fontawesome.com/releases/v5.14.0/css/all.css);"),
    sidebarMenu(
        id = "sidebar",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Filter Simulations", tabName = "filters", icon = icon("table"))
        # menuItem("Calculations (2)", tabName = "calculations_2", icon = icon("table")),
        # menuItem("Source Code", icon = icon("file-code-o"), 
        #          href = "https://github.com/UW-Madison-DataScience/Paltiel-COVID-19-Screening-for-College/blob/master/Screening/app.R"),
        # menuItem("Original Spreadsheet", icon = icon("google-drive"), 
        #          href = "https://docs.google.com/spreadsheets/d/1otD4h-DpmAmh4dUAM4favTjbsly3t5z-OXOtFSbF1lY/edit#gid=1783644071"),
        # menuItem("References", tabName = "references", icon = icon("book"))
    )
)

body <- dashboardBody(
    tags$style("@import url(https://use.fontawesome.com/releases/v5.14.0/css/all.css);"),
    # shinydashboard has fixed colors mapped to their CSS classes.  We over-ride those 
    # to produce a custom color palette.  Some details can be found here: https://stackoverflow.com/questions/36080529/r-shinydashboard-customize-box-status-color
    tags$style(HTML(
        "
    .box.box-solid.box-primary>.box-header {
    color:#fff;
    background:#646569
    }
    
    .box.box-solid.box-primary{
    border-bottom-color:#646569;
    border-left-color:#646569;
    border-right-color:#646569;
    border-top-color:#646569;
    }
    
    .bg-olive {
    background-color: #8F0000!important;
    }
    
    .bg-navy {
    background-color: #8F0000!important;
    }
    
    .skin-blue .main-header .navbar { background-color: #c5050c!important; }
    
    .skin-blue .main-header .logo { background-color: #9b0000; }
    .skin-blue .main-header .logo:hover { background-color: #8F0000; }
    .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
    border-left-color: #8F0000; 
    }
    .skin-blue .main-header .navbar .sidebar-toggle:hover{ background-color:#9b0000; }
    .skin-blue .main-header .navbar .dropdown-menu li a:hover{ background:#8F0000; }
    "
    )),
    tags$style(HTML(
        "
    .small-box h3 {
      font-size: 28px;
    }
    
    .small-box .icon-large {
      font-size: 55px;
    }
    "
    )),
    tabItems(
        ## Dashboard ---------------------------------------------------------------
        tabItem(
            tabName = "dashboard",
            ## Inputs ----------------------------------------------------------------
            column(width = 3,
                   ## Outcomes of interest
                   box(title = "Outcomes", width = NULL, solidHeader = TRUE, status = input_element_color,
                       collapsible = TRUE, collapsed = FALSE,
                       selectInput("outcome",
                                   "Outcome",
                                   choices = c("Cases", "Isolation"),
                                   selected = "Cases",
                                   selectize = TRUE),
                       selectInput("metric",
                                   "Metric to plot",
                                   choices = c(
                                       "covid_pop",
                                       "cumulative_covid_pop",
                                       "covid_frac",
                                       "cumulative_covid_frac",
                                       "t"
                                   ),
                                   selected = "covid_pop",
                                   selectize = TRUE)
                   ),
                   ## Parameter to evaluate 
                   box(title = "Select Parameter to Show Results", width = NULL, solidHeader = TRUE, status = input_element_color,
                       collapsible = TRUE, collapsed = FALSE,
                       selectInput("parameter1",
                                   "Parameter of Interest",
                                   choices = c("(Overview of all parameters)" = " ", parameter_choices),
                                   # selected = parameter_choices[1],
                                   selectize = TRUE),
                       selectInput("group",
                                   "Group to change parameter in",
                                   choices = c(0),
                                   # selected = output$group_choices[1],
                                   selectize = TRUE)
                   ),
                   box(title = "Group Codes", width = NULL, solidHeader = TRUE, status = input_element_color,
                       collapsible = TRUE, collapsed = FALSE,
                       tableOutput("group_table")
                   )
                   # ## Second parameter to evaluate (TODO)
                   # box(title = "Parameters", width = NULL, solidHeader = TRUE, status = input_element_color,
                   #     collapsible = TRUE, collapsed = FALSE,
                   #     selectInput("parameter1",
                   #                 "Parameter to evaluate",
                   #                 choices = putput$parameter_choices,
                   #                 selectize = TRUE),
                   #     selectInput("group",
                   #                 "Group for parameter",
                   #                 choices = output$group_choices,
                   #                 selectize = TRUE)
                   # ),
            ),
            ## Outputs: plot and metrics ---------------------------------------------
            column(width = 9, 
                   # fluidRow(
                   #     valueBoxOutput("testing_cost_box", width = 4),
                   #     valueBoxOutput("number_tested_box", width = 4),
                   #     valueBoxOutput("average_iu_census_box", width = 4),
                   # ),
                   # fluidRow(
                   #     valueBoxOutput("infections_box", width = 4),
                   #     valueBoxOutput("number_confirmatory_tests_box", width = 4),
                   #     valueBoxOutput("average_pct_isolated_box", width = 4),
                   # ),
                   box(plotOutput("plot1", height = "600px"), width = 400)
            )
        ),
        ## Parameter Filters ---------------------------------------------------
        tabItem(
            tabName = "filters",
            h3("Remove values from the group parameters to filter results in Dashboard. "),
            map(names(varied_parameters)[-1], ~ make_param_filter_ui(varied_parameters, .x))
        )
    )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
    
    filter_sims <- function(var) {
      var_param_list <- varied_parameters %>% select(Group, !!var) %>% deframe()
      
      param_index <- function(var, group) {
        id <- paste0(var, "_", group)
        readable_number(all_params_wide[[id]]) %in% input[[id]]
      }
      
      each_group <- 
        var_param_list %>% 
        keep(~ length(.x) > 0) %>% 
        imap(~ param_index(var, group = .y))
      
      reduce(each_group, ~ .x & .y)
    }
  
    ## Reactive elements ---------------------------------------------------------
    selected <- reactive({
      each_param <- map(names(varied_parameters)[-1], ~filter_sims(.x))
      idx <- reduce(each_param, ~ .x & .y)
      all_params_wide$sim_id[idx]
    })
    
    
    
    df <- reactive({
        if (input$outcome == "Cases") {
            df <- df_cases()
        } else {
            df <- df_isolation() 
        }
        
        df <- 
            df %>% 
            filter(sim_id %in% selected()) %>% 
            left_join(all_params_wide %>% 
                          select(sim_id, skim_df$skim_variable),
                      by = "sim_id") %>% 
            left_join(group_names %>% mutate(group_index = as.numeric(group_index)),
                      by = c("group_number" = "group_index")) %>% 
            mutate(across(c("t", "group_number"), as.double))
        
    })
    
    df_cases <- reactive({
        req(input$outcome == "Cases")
        ## Cases data frame
        # create a table that averages over the sim_id and group
        dbGetQuery(con,
                   '
           CREATE TEMP TABLE results_averaged AS
           SELECT "sim_id", "replicate_id", "t", "group_number",
           "cumulative_mild" + "cumulative_severe" + "cumulative_outside_infections" AS "cumulative_covid_pop",
           "QI" + "E_0" + "E_1" + "E_2" + "E_3" + "E_4" + "E_5" + "E_6" + "pre_ID_0" + "pre_ID_1" + "pre_ID_2" + "pre_ID_3" + "ID_0" + "ID_1" + "ID_2" + "ID_3" + "ID_4" + "ID_5" + "ID_6" + "ID_7" + "SyID_mild_0" + "SyID_mild_1" + "SyID_mild_2" + "SyID_mild_3" + "SyID_mild_4" + "SyID_mild_5" + "SyID_mild_6" + "SyID_mild_7" + "SyID_mild_8" + "SyID_mild_9" + "SyID_mild_10" + "SyID_mild_11" + "SyID_mild_12" + "SyID_mild_13" + "SyID_mild_14" + "SyID_mild_15" + "SyID_mild_16" + "SyID_mild_17" + "SyID_mild_18" + "SyID_mild_19" + "SyID_severe_0" + "SyID_severe_1" + "SyID_severe_2" + "SyID_severe_3" + "SyID_severe_4" + "SyID_severe_5" + "SyID_severe_6" + "SyID_severe_7" + "SyID_severe_8" + "SyID_severe_9" + "SyID_severe_10" + "SyID_severe_11" + "SyID_severe_12" + "SyID_severe_13" + "SyID_severe_14" + "SyID_severe_15" + "SyID_severe_16" + "SyID_severe_17" + "SyID_severe_18" + "SyID_severe_19" AS "covid_pop"
           FROM "results"
           ')
        
        # now calculate peak and time to peak
        df_cases <-
            dbGetQuery(con, '
           SELECT a.*
           FROM results_averaged a
           INNER JOIN
             (SELECT "sim_id", "group_number", MAX("covid_pop") AS "peak"
             FROM results_averaged
             GROUP BY "sim_id", "group_number") b
           ON a.sim_id = b.sim_id
           AND a.group_number = b.group_number
           AND a."covid_pop" = b."peak" ')
        
        df_cases <-
            df_cases %>%
            group_by(sim_id, group_number) %>%
            filter(t == min(t)) %>%
            ungroup() %>% 
            mutate(across(where(bit64::is.integer64), as.numeric))
        
        dbGetQuery(con, "DROP TABLE IF EXISTS results_averaged")
        
        df_cases <-
            df_cases %>%
            left_join(pop_sizes, by = "group_number") %>%
            mutate(covid_frac = covid_pop / population_size,
                   cumulative_covid_frac = cumulative_covid_pop / population_size)
    })
    
    df_isolation <- reactive({
        req(input$outcome == "Isolation")
        ## Isolation data frame
        # create a table that averages over the sim_id and group
        dbGetQuery(con,
                   '
           CREATE TEMP TABLE results_averaged AS
           SELECT "sim_id", "replicate_id", "t", "group_number", AVG("QS")  + AVG("QI") AS "Q"
           FROM "results"
           GROUP BY "sim_id", "replicate_id", "t", "group_number"
           ')
        
        # now calculate peak and time to peak
        df_isolation <-
            dbGetQuery(con, '
           SELECT a.*
           FROM results_averaged a
           INNER JOIN
             (SELECT "sim_id", "group_number", MAX("Q") AS "peak"
             FROM results_averaged
             GROUP BY "sim_id", "group_number") b
           ON a.sim_id = b.sim_id
           AND a.group_number = b.group_number
           AND a."Q" = b."peak" ')
        
        df_isolation <-
            df_isolation %>%
            group_by(sim_id, group_number) %>%
            filter(t == min(t)) %>%
            ungroup() %>% 
            mutate(across(where(bit64::is.integer64), as.numeric))
        
        dbGetQuery(con, "DROP TABLE IF EXISTS results_averaged")
        
        
        df_isolation <-
            df_isolation %>%
            left_join(pop_sizes, by = "group_number") %>%
            mutate(Q_frac = Q / population_size)
        
    })
    
    
    ## Observations to update input selections ---------------------------------
    observe({
        outcome <- input$outcome
        updateSelectInput(session, "metric", choices = metric_choices[[outcome]])
    })
    
    observe({
        parameter1 <- input$parameter1
        group_ind <- map_lgl(varied_parameters[[parameter1]], ~ length(.x) > 0)
        
        choices <- varied_parameters$Group[group_ind]
        updateSelectInput(session, "group", choices = choices)
    })
    
    
    ## OUTPUTS -------------------------------------------------------------------
    output$plot1 <- 
        renderPlot({
            # req(input$outcome,
            #     input$metric,
            #     input$parameter1,
            #     input$metric,
            #     cancelOutput = TRUE)
            
            df <- df()
            # print(input$parameter1)
            
            if (input$parameter1 == " ") {
                if (input$outcome == "Cases") {
                    df %>% 
                        mutate(group = paste0(group_number, ": ", group_names)) %>%
                        ggplot(aes(t, covid_pop)) +
                        geom_quasirandom(aes(color = as.factor(group)), alpha = 1, size = point_size) +
                        # scale_color_viridis_d(name = "Group", option = "B", begin = 0.2, end = 0.8) +
                        scale_color_fish_d(name = "Group", option = "Coris_gaimard") +
                        labs(x = "Time of Peak COVID-19 cases", y = "Peak active cases",
                             title = "COVID-19 Cases: Overview of all simulations ")    
                } else {
                    df %>% 
                        mutate(group = paste0(group_number, ": ", group_names)) %>%
                        ggplot(aes(t, Q)) +
                        geom_quasirandom(aes(color = as.factor(group)), alpha = 1, size = point_size) +
                        # scale_color_viridis_d(name = "Group", option = "B", begin = 0.2, end = 0.8) +
                        scale_color_fish_d(name = "Group", option = "Coris_gaimard") +
                        labs(x = "Time of peak quarantine", y = "Peak quarantine census",
                             title = "Quarantine Census: Overview of all simulations")
                }
                
            } else {
                var_to_plot <- paste0(input$parameter1, "_", input$group)
                plot_fun(df, input$metric, var_to_plot, y_lab = names(which(metric_choices[[input$outcome]] == input$metric)))    
            }
            # TODO: update this to be a hidden tabset panel: https://mastering-shiny.org/action-dynamic.html#dynamic-visibility 
        })
    
    output$group_table <- 
        renderTable({
            group_names %>% 
                arrange(group_index) %>% 
                setNames(c("Index", "Names"))
        })
    
}

shinyApp(ui, server)


#' TODO: list
#' - Update the "Group to change parameter in" to have the group names?
#' - Add in second parameter option and visualization
#' - Information on the parameters that don't change and are default
#' - General references and documentation 

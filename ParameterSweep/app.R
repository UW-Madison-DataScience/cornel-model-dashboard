library(shiny)
library(shinydashboard)
library(scales)
library(tidyverse)
library(plotly)
library(ggbeeswarm)
library(odbc)
library(DBI)
library(RPostgres)
library(skimr)
library(here)
library(fishualize)
library(plotly)
library(janitor)

# print(odbc::odbcListDrivers())
# print(odbc::odbcListDrivers()$name)
# message(odbc::odbcListDrivers())


## Initial setup ----------------------------------------------------------

## Connection to SQL database 
print("Trying to connect to SQL db at database-2.clbsgd2qdkby.us-east-1.rds.amazonaws.com...")
db <- config::get("dbtest")

con <- dbConnect(RPostgres::Postgres(),
                 dbname = db$dbname,
                 host = db$host,
                 port = db$port,
                 user = db$user,
                 password = db$password)

print("Connection successful!")

## Parameters that get varied across simulations 
all_params <-
    tbl(con, "group_params") %>%
    distinct() %>%
    collect() %>% 
    mutate(across(where(bit64::is.integer64), as.numeric))

all_params_wide <-
    all_params %>%
    rename(
      "Contact rate multiplier" = contact_rate_multiplier,
      "Initial prevalence" = initial_ID_prevalence,
      "Test fraction" = test_population_fraction
    ) %>% 
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
                 names_to = c("variable", "group_number"),
                 names_pattern = "(.*)_([:digit:])") %>% 
    pivot_wider(names_from = variable,
                values_from = value) %>% 
    mutate(across(.cols = -group_number, .fns = ~map(.x, ~readable_number(sort(.x)))))

parameter_choices <- colnames(varied_parameters)[-1]

## Metrics and values from results
metrics <- tbl(con, "metrics") %>% 
  collect() %>% 
  pivot_longer(cols = -c(sim_id, group_number), 
               names_to = "metric_name", 
               values_to = "metric_value") %>% 
  mutate(group_number = as.double(group_number))

metric_choices <- list(
  `COVID-19 Cases` = list(
    "Peak active cases", 
    "Peak % with COVID-19",
    "Time of peak COVID-19 cases",
    "Cumulative cases",
    "Cumulative % with COVID-19"
  ),
  `Isolation Capacity` = list(
    "Peak quarantine census",
    "Peak % in quarantine",         
    "Time of peak quarantine"   
  )
)

percent_metrics <- c("Peak % with COVID-19", "Cumulative % with COVID-19", "Peak % in quarantine")

## Group names 
group_names <- tribble(
  ~group_number, ~group_name, ~group_name2, ~group_name_db, ~long_name, 
  0, "UG (Dorm)", "UG \n(Dorm)", "undergraduates_dorm", "Undergraduates residing in dorms",
  1, "UG (Off Campus)", "UG \n(Off Campus)", "undergraduates_off_campus", "Undergraduates residing off-campus",
  2, "Grad (Research)", "Grad \n(Research)", "grad_research", "Graduate students in research roles",
  3, "Grad (Teaching)", "Grad \n(Teaching)", "grad_teaching", "Graduate students in teaching roles",
  4, "F/S (Student Facing)", "F/S \n(Student Facing)", "fac_staff_student_facing", "Student-facing Faculty and Staff",
  5, "F/S (Other)", "F/S \n(Other)", "fac_staff_non_student_facing", "Other Faculty and Staff",
  6, "F/S (WFH)", "F/S \n(WFH)", "fac_staff_WFH", "Faculty and Staff working from home",
  7, "Communitiy", "Communitiy", "community", "Madison Community"
)

group_dict <- group_names %>% select(group_number, group_name) %>% deframe()
group_dict2 <- group_names %>% select(group_number, group_name2) %>% deframe()
group_dict_rev <- group_names %>% select(group_name, group_number) %>% deframe()

## Plot themes 
theme_set(theme_minimal(base_size = 16))
point_size = 2

wrap_tick <- function(x) { paste0("`", x, "`") }

## Helper functions ------------------------------------------------------------


scatter_plot_overview <- function(data, x, y) {
  p <- data %>% 
    # mutate(group = paste0(group_number, ": ", group_names)) %>%
    ggplot(aes_(as.name(x), as.name(y))) +
    geom_quasirandom(aes(color = as.factor(group_number)), alpha = 1, size = point_size) +
    # scale_color_viridis_d(name = "Group", option = "B", begin = 0.2, end = 0.8) +
    scale_color_fish_d(name = "Group", option = "Coris_gaimard",
                       labels = group_dict) +
    labs(title = "COVID-19 Cases: Overview of all simulations ")    
  
  if (x %in% percent_metrics) {
    p <- p + scale_x_continuous(labels = scales::percent)
  }
  if (y %in% percent_metrics) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  
  p
}

scatter_plot_overview_plotly <- function(data, x, y) {
  x_axis <- list(title = x)
  y_axis <- list(title = y)
  if (x %in% percent_metrics) {
    x_axis$tickformat = "%"
  }
  if (y %in% percent_metrics) {
    y_axis$tickformat = "%"
  }
  group_names <- group_dict[as.character(data[["group_number"]])]
  
  plot_ly(
    x = data[[x]],
    y = data[[y]],
    color = factor(group_names, levels = names(group_dict_rev)),
    customdata = 1:nrow(data), 
    alpha = 1
  ) %>% 
    add_markers() %>% 
    layout(
      xaxis = x_axis,
      yaxis = y_axis,
      title = list(text = "COVID-19 Cases: Overview of all simulations "),
      dragmode = "select"
    ) %>% 
    event_register("plotly_selected")
}


one_param_quasi_plot <- function(data, y, fill, y_lab = NULL) {
    alpha <- exp(-nrow(data) / (5000 / log(2)))
    fill_match <- str_match(fill, "(.*)_(\\d+)")
    name = paste0(fill_match[2], " in ", group_dict[fill_match[3]])
  
    p <- data %>% 
        ggplot(aes_(~as.factor(group_number), as.name(y), fill = as.name(fill))) +
        geom_quasirandom(aes_(color = as.name(fill)), dodge.width = 0.5, size = point_size, alpha = alpha) +
        stat_summary(aes_(color = as.name(fill)), fun = mean, geom = "point", shape = 3, size = point_size) +
        geom_hline(yintercept = 0) +
        scale_x_discrete(labels = group_dict2) +
        scale_fill_viridis_d(option = "magma", end = 0.8, begin = 0.2, aesthetics = c("color", "fill"),
                             name = name, 
                             labels = readable_number) +
        theme(legend.position = "bottom") +
        labs(x = NULL,
             title = "Results of Cornell Model across groups") 
    
    if (y %in% percent_metrics) {
        p <- p + scale_y_continuous(labels = scales::percent)
    }
    
    p
    # TODO: update the name of the color legend
}

one_param_quasi_plotly <- function(data, y, fill, logscale) {
  # p <- one_param_quasi_plot(data, y, fill)
  # ggplotly(p)
  
  col_pal <- viridis::magma(length(unique(data[[fill]])), end = 0.8, begin = 0.2)
  fill_match <- str_match(fill, "(.*)_(\\d+)")
  leg_title = paste0(fill_match[2], " in ", group_dict[fill_match[3]])
  
  # data$fill_ <- data[[fill]]
  
  p <- data %>% 
    nest_by(group_number) %>% 
    mutate(plot = list(
      plot_ly(
        y = data[[y]],
        x = readable_number(data[[fill]]), 
        color = readable_number(data[[fill]]),
        colors = col_pal, 
        scalegroup = group_number,
        # side = "positive",
        type = "violin",
        box = list(
          visible = FALSE
        ),
        meanline = list(
          visible = TRUE
        ),
        # name = group_dict[as.character(group_number)],
        # x0 = group_dict[as.character(group_number)], 
        showlegend = ifelse(group_number == 0, TRUE, FALSE),
        legendgroup = "all",
        hoveron = "violins",
        points = FALSE,
      ) %>% 
        layout(xaxis = list(title = group_dict2[as.character(group_number)]))
    )) %>% 
    subplot(shareY = TRUE, titleX = TRUE, margin = 0.01)
  
  if (logscale) {
    p %>% 
      layout(yaxis = list(title = y, rangemode = "nonenegative", type = "log"),
             title = list(text = leg_title))
  } else {
    p %>% 
      layout(yaxis = list(title = y, rangemode = "nonnegative"),
             title = list(text = leg_title))  
  }
}

one_param_quasi_plot_faceted <- function(data, group, x, y, color, rows, cols, logscale) {
  data <- data %>% 
    mutate(across(skim_df$skim_variable[-1], .fns = readable_number),
           Group = group_dict[as.character(group_number)]) 
  
  `%ni%` <- Negate(`%in%`)
  if (-1 %ni% group) {
    data <- filter(data, group_number %in% group)
  }
  
  if (is.null(cols)) { cols <- "." }
  if (is.null(rows)) { rows <- "." }
  
  rows <- wrap_tick(rows)
  cols <- wrap_tick(cols)
  
  facet_form <- as.formula(paste0(
    paste0(rows, collapse = " + "),
    " ~ ",
    paste0(cols, collapse = " + ")
  ))
  
  p <- data %>%
    ggplot() +
    aes_(
      x = as.name(x),
      y = as.name(y),
      color = as.name(color),
      group = as.name(color)
    ) +
    geom_quasirandom() +
    geom_smooth(method = "lm", se = FALSE, formula = "y ~ x") +
    scale_fill_viridis_d(option = "magma", end = 0.8, begin = 0.2, aesthetics = c("color", "fill"),
    ) +
    facet_grid(
      facet_form,
      scales = "free",
      labeller = "label_both"
    ) +
    theme(legend.position = "bottom") +
    labs(y = y,
         title = paste("Comparison of", y, "in", group_dict[as.character(group)]))
  
  if (logscale) {
    p <- p + scale_y_log10() + labs(y = paste(y, "(log scale)"))
  } else if (y %in% percent_metrics) {
      p <- p + scale_y_continuous(labels = scales::percent)
  }
  
  p
}

## UI --------------------------------------------------------------------------

make_param_filter_ui <- function(x, var) {
  var_param_list <- x %>% select(group_number, !!var) %>% deframe()
  
  group_ui <- function(choices, group_number) {
    # selectizeInput(paste0(var, "_", group_number),
    #                group_dict[[group_number]],
    #                choices = choices,
    #                selected = choices,
    #                multiple = TRUE)
    checkboxGroupInput(paste0(var, "_", group_number),
                       group_dict[[group_number]],
                       choices = choices,
                       selected = choices)

  }
  
  group_uis <- 
    var_param_list %>% 
    keep(~ length(.x) > 0) %>% 
    imap(~ group_ui(choices = sort(.x), group_number = .y))
  
  n <- length(group_uis)
  n_2 <- ceiling(n / 2)
  
  tabPanelBody(value = var,
    column(width = 6, group_uis[1:n_2]),
    column(width = 6, group_uis[(n_2+1):n])
  )
  # list(
  #     column(width = 6, group_uis[1:n_2]),
  #     column(width = 6, group_uis[(n_2+1):n])
  # )
  # group_uis
  
  # tabPanelBody(value = var,
  #              group_uis)
  
  # tabPanel(title = "Hello",
  #          h3("Some text") 
  # )
  # box(title = var, width = 3, solidHeader = TRUE, status = input_element_color,
  #     collapsible = TRUE, collapsed = FALSE,
  #     # column(width = 6, group_uis[1:4]),
  #     # column(width = 6, group_uis[5:8])
  #     column(width = 6, group_uis[1:n_2]),
  #     column(width = 6, group_uis[(n_2+1):n])
      # ) # might need to unlist this or something 
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
        menuItem("Documentation", tabName = "documentation", icon = icon("book"))
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
                   ## Box to pick plot and axes 
                   box(title = "Plot Options", width = NULL, solidHeader = TRUE, status = input_element_color,
                       collapsible = TRUE, collapsed = FALSE,
                       selectInput("plot_type", 
                                   "Plot type",
                                   choices = c("Scatter plot overview (2 metrics)", 
                                               "Parameter comparison (1 metric)",
                                               "Parameter comparison - faceted"),
                                   selected = "Scatter plot overview (2 metrics)",
                                   selectize = TRUE),
                       h6("Use the selections below to change plot aesthetics. The options will change based on the plot type chosen above."), 
                       tabsetPanel(
                         id = "plot_axes",
                         type = "hidden", 
                         # Show these when plot_type == "Scatter plot overview (2 metrics)"
                         tabPanel("Scatter plot overview (2 metrics)",
                                  selectInput("metric_y", 
                                              "Metric to plot (y-axis)",
                                              choices = metric_choices,
                                              selected = "Peak active cases",
                                              selectize = TRUE),
                                  selectInput("metric_x",
                                              "Metric to plot (x-axis)",
                                              choices = metric_choices,
                                              selected = "Time of peak COVID-19 cases",
                                              selectize = TRUE),
                         ),
                         # Show these when plot_type == "Parameter comparison (1 metric)"
                         tabPanel("Parameter comparison (1 metric)",
                                  selectInput("metric", 
                                              "Metric to plot (y-axis)",
                                              choices = metric_choices,
                                              selected = "Peak active cases",
                                              selectize = TRUE),
                                  checkboxInput("pc_yscale", 
                                                "Log scale?", 
                                                value = FALSE), 
                                  selectInput("parameter1",
                                              "Parameter of Interest (color)",
                                              choices = c(parameter_choices),
                                              # selected = parameter_choices[1],
                                              selectize = TRUE),
                                  selectInput("group",
                                              "Group to change parameter in",
                                              choices = c(0),
                                              # selected = output$group_choices[1],
                                              selectize = TRUE)
                         ),
                         # Show these when plot_type == "Parameter comparison - faceted"
                         tabPanel("Parameter comparison - faceted",
                                  h6("Note: this plot uses group codes instead of group names for compactness. See the documentation tab for full names."),
                                  selectInput("pcf_group",
                                              "Filter plot by group",
                                              choices = c("All" = -1, group_dict_rev), 
                                              multiple = TRUE, 
                                              selected = 0,
                                              selectize = TRUE), 
                                  selectInput("pcf_y", 
                                              "Y-axis [metric]",
                                              choices = metric_choices,
                                              selected = "Peak active cases",
                                              selectize = TRUE),
                                  checkboxInput("pcf_yscale", 
                                                "Log scale?", 
                                                value = TRUE), 
                                  selectInput("pcf_x", 
                                              "X-axis [parameter]",
                                              choices = c("Group", skim_df$skim_variable[-1]),
                                              selected = "Contact rate multiplier_0",
                                              selectize = TRUE), 
                                  selectInput("pcf_color", 
                                              "Color [parameter]",
                                              choices = c("Group", skim_df$skim_variable[-1]),
                                              selected = "Test fraction_0",
                                              selectize = TRUE), 
                                  selectInput("pcf_row", 
                                              "Row facet [parameter]",
                                              choices = c("Group", skim_df$skim_variable[-1]),
                                              selected = "Test fraction_1",
                                              multiple = TRUE, 
                                              selectize = TRUE), 
                                  selectInput("pcf_col", 
                                              "Column facet [parameter]",
                                              choices = c("Group", skim_df$skim_variable[-1]),
                                              selected = c("Initial prevalence_0", "Initial prevalence_1"),
                                              multiple = TRUE,
                                              selectize = TRUE)
                         )
                       )
                   ),
                   box(
                     title = "Filter Simulations", width = NULL, solidHeader = TRUE, status = input_element_color,
                     collapsible = TRUE, collapsed = FALSE,
                     h6("After selecting a parameter, you will see all of the groups and values for that parameter.  Un-check boxes to remove simulations from the plots. "),
                     selectInput("filter_param",
                                 label = NULL,
                                 choices = parameter_choices
                     ), 
                     do.call(tabsetPanel, # see https://github.com/rstudio/shiny/issues/2927
                             args = c(
                               id = "sim_filter",
                               type = "hidden",
                               map(names(varied_parameters)[-1],
                                   ~ make_param_filter_ui(varied_parameters, .x))
                                   # ~ tabPanelBody(.x, make_param_filter_ui(varied_parameters, .x))
                               )
                             )
                   )
            ),
            ## Outputs: plot and metrics ---------------------------------------------
            column(width = 9, 
                   box(width = 400,
                       tabsetPanel(
                         id = "plot_panel",
                         type = "hidden",
                         tabPanel("plot1", plotOutput("plot1", height = "600px")),
                         tabPanel("plotly1", plotlyOutput("plotly1", height = "600px"))
                       )),
                   box(width = 400, 
                       uiOutput("selected_info"))
            )
        ),
        ## Parameter Filters ---------------------------------------------------
        tabItem(
            tabName = "documentation",
            h1("Documentation"),
            p("Documentation is still a work in progress. Updates coming soon."),
            tableOutput("group_table")
        )
    )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
    
    ## Helper functions --------------------------------------------------------
    filter_sims <- function(var) {
      var_param_list <- varied_parameters %>% select(group_number, !!var) %>% deframe()
      
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
  
    custom_table <- function(data, var) {
      tbl1 <- data %>% 
        tabyl({{ var }}, plot_selected) %>% 
        adorn_percentages(denominator = "col") %>% 
        adorn_pct_formatting() %>% 
        adorn_ns()
      
      tbl1[, 1] <- readable_number(tbl1[, 1])
      colnames(tbl1) <- c("Param value", "Not Selected", "Selected")
      
      tbl1[, c(1, 3)] 
    }
  
    ## Reactive elements ---------------------------------------------------------
    selected <- reactive({
      each_param <- map(names(varied_parameters)[-1], ~filter_sims(.x))
      idx <- reduce(each_param, ~ .x & .y)
      all_params_wide$sim_id[idx]
    })
    
    df <- reactive({
      metrics %>% 
        filter(sim_id %in% selected()) %>%
        pivot_wider(names_from = "metric_name", values_from = "metric_value") %>% 
        left_join(all_params_wide %>% 
                    select(sim_id, skim_df$skim_variable),
                  by = "sim_id") #%>% 
        #left_join(group_names %>% mutate(group_index = as.numeric(group_index)),
        #          by = c("group_number" = "group_index"))
    })
    
    plot_selected <- reactiveVal()
    
    observeEvent(event_data("plotly_selected"), {
      d <- event_data("plotly_selected")
      plot_selected(d$customdata)
    })
    
    observeEvent(selected(), {
      # if filters are selected, need to reset since plot data changes 
      plot_selected(NULL)
    })
    
    info_gain_table <- reactive({
      df <- df()
      vars_to_exclude <- skim(df) %>% filter(factor.n_unique == 1) %>% pull(skim_variable)
      
      df$plot_selected <- factor(0, levels = c(0, 1))
      df$plot_selected[plot_selected()] <- 1
      
      vars <- wrap_tick(skim_df$skim_variable[-1])
      var_form <- as.formula(paste("plot_selected ~", paste(vars, collapse = " + ")))
      
      FSelectorRcpp::information_gain(var_form, data = df, type = "gainratio") %>% 
        mutate(
          importance = ifelse(attributes %in% vars_to_exclude, 0, importance), 
          importance = importance / max(importance), # normalize to a percent score
          importance = readable_number(importance)
        ) %>% 
        arrange(desc(importance))
    })
    
    twoway_table_list <- reactive({
      ig_table <- info_gain_table()
      df <- df()
      df$plot_selected <- factor(0, levels = c(0, 1))
      df$plot_selected[plot_selected()] <- 1

      vars <- ig_table$attributes
      vars %>%
        map(as.name) %>%
        map(custom_table, data = df) %>% # see helper function above
        setNames(vars)
    })
    
    
    ## Observations to update input selections ---------------------------------
    observeEvent(input$plot_type, {
      updateTabsetPanel(session, "plot_axes", selected = input$plot_type)
      
      # switch between plotOutput and plotlyOutput 
      plot_panel_selection = switch(input$plot_type, 
                                    "Scatter plot overview (2 metrics)" = "plotly1",
                                    "Parameter comparison (1 metric)" = "plotly1",
                                    "Parameter comparison - faceted" = "plot1")
      
      updateTabsetPanel(session, "plot_panel", selected = plot_panel_selection)
    }) 
    
    observeEvent(input$filter_param, {
      print(input$filter_param)
      updateTabsetPanel(session, "sim_filter", selected = input$filter_param)
    }) 
    
    observe({
        parameter1 <- input$parameter1
        group_ind <- map_lgl(varied_parameters[[parameter1]], ~ length(.x) > 0)
        group_ind <- as.numeric(varied_parameters$group_number[group_ind])
        # choices <- group_dict_rev[group_ind]
        choices <- group_dict_rev[which(group_dict_rev %in% group_ind)]
        
        updateSelectInput(session, "group", choices = choices)
    })
    
    ## OUTPUTS -------------------------------------------------------------------
    output$plot1 <-
        renderPlot({
          req(input$plot_type)
          switch(input$plot_type,
            "Scatter plot overview (2 metrics)" = scatter_plot_overview(df(), input$metric_x, input$metric_y),
            "Parameter comparison (1 metric)" = one_param_quasi_plot(df(), input$metric, paste0(input$parameter1, "_", input$group)),
            "Parameter comparison - faceted" = one_param_quasi_plot_faceted(df(), input$pcf_group, input$pcf_x, input$pcf_y, input$pcf_color, input$pcf_row, input$pcf_col, input$pcf_yscale)
          )
        })

    output$plotly1 <-
      renderPlotly({
        req(input$plot_type)
        switch(input$plot_type,
               "Scatter plot overview (2 metrics)" = scatter_plot_overview_plotly(df(), input$metric_x, input$metric_y),
               "Parameter comparison (1 metric)" = one_param_quasi_plotly(df(), input$metric, paste0(input$parameter1, "_", input$group), input$pc_yscale),
               "Parameter comparison - faceted" = one_param_quasi_plot_faceted(df(), input$pcf_group, input$pcf_x, input$pcf_y, input$pcf_color, input$pcf_row, input$pcf_col, input$pcf_yscale)
        )
      })
    
    output$group_table <- 
        renderTable({
          group_names %>% 
            mutate(group_number = as.character(group_number)) %>% 
            select(group_number, group_name, long_name) %>% 
            arrange(group_number) %>% 
            setNames(c("Group Number", "Name", "Description"))
        })
    
    output$selected_info <- renderUI({
      one_row <- function(i) {
        var <- ig_table[i, "attributes"]
        var_match <- str_match(var, "(.*)_(\\d+)")
        name = paste0(var_match[2], " in ", group_dict[var_match[3]])
        
        fluidRow(
          column(width = 4, 
                 h4(name),
                 h5(paste("Importance score: ", ig_table[i, "importance"]))
          ),
          column(width = 8,
                 renderTable({ twoway_table_list()[[i]] }, spacing = "xs") 
          )
        )
      }
      
      if (is.null(plot_selected())) {
        h4("Make a selection in the ", em("scatter plot overview"),  " to display important parameters")
      } else {
        ig_table <- info_gain_table()
        map(1:nrow(ig_table), one_row)
      }
      
    })
      
}

shinyApp(ui, server)


#' TODO: list
#' - Update the "Group to change parameter in" to have the group names?
#' - Add in second parameter option and visualization
#' - Information on the parameters that don't change and are default
#' - General references and documentation 

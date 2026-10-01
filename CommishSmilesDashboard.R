#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# app.R
# Fantasy Football Analytics Dashboard
# Reads the "Summary" tab from Fantasy_Olaf_2026_Analysis.xlsx

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# ====================================================
# LOAD DATA
# ====================================================

summary_df <- read_excel(
  "Fantasy_Olaf_2026_Analysis.xlsx",
  sheet = "Summary"
)

summary_df <- as.data.frame(summary_df)

# Ensure numeric columns are detected correctly
numeric_cols <- names(summary_df)[sapply(summary_df, is.numeric)]

# Default metrics if they exist
default_rank_metric <- if ("Luck Rating" %in% numeric_cols)
  "Luck Rating" else numeric_cols[1]

default_x <- if ("Luck Rating" %in% numeric_cols)
  "Luck Rating" else numeric_cols[1]

default_y <- if ("Manager Rating" %in% numeric_cols)
  "Manager Rating" else numeric_cols[2]

default_size <- if ("Actual Points" %in% numeric_cols)
  "Actual Points" else numeric_cols[1]

# ====================================================
# UI
# ====================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Fantasy Analytics"
  ),
  
  dashboardSidebar(
    
    h4("Team Rankings"),
    
    selectInput(
      "metric",
      "Ranking Metric",
      choices = numeric_cols,
      selected = default_rank_metric
    ),
    
    tags$hr(),
    
    h4("Scatter Plot Explorer"),
    
    selectInput(
      "x_metric",
      "X-Axis Metric",
      choices = numeric_cols,
      selected = default_x
    ),
    
    selectInput(
      "y_metric",
      "Y-Axis Metric",
      choices = numeric_cols,
      selected = default_y
    ),
    
    selectInput(
      "size_metric",
      "Bubble Size",
      choices = c("None", numeric_cols),
      selected = default_size
    )
  ),
  
  dashboardBody(
    
    fluidRow(
      
      valueBoxOutput("leaderBox", width = 4),
      valueBoxOutput("avgBox", width = 4),
      valueBoxOutput("spreadBox", width = 4)
      
    ),
    
    fluidRow(
      
      box(
        width = 12,
        title = "Team Rankings",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("rankPlot", height = "550px")
      )
      
    ),
    
    fluidRow(
      
      box(
        width = 12,
        title = "Scatter Plot Explorer",
        status = "info",
        solidHeader = TRUE,
        plotlyOutput("scatterPlot", height = "650px")
      )
      
    ),
    
    fluidRow(
      
      box(
        width = 12,
        title = "League Data",
        status = "success",
        solidHeader = TRUE,
        DTOutput("summaryTable")
      )
      
    )
    
  )
)

# ====================================================
# SERVER
# ====================================================

server <- function(input, output, session) {
  
  # -----------------------------------------
  # Reactive Ranking Data
  # -----------------------------------------
  
  ranking_data <- reactive({
    
    summary_df %>%
      arrange(desc(.data[[input$metric]]))
  })
  
  # -----------------------------------------
  # KPI: Leader
  # -----------------------------------------
  
  output$leaderBox <- renderValueBox({
    
    leader <- ranking_data()[1, ]
    
    valueBox(
      value = round(leader[[input$metric]], 2),
      subtitle = paste("Leader:", leader$Team),
      icon = icon("trophy"),
      color = "green"
    )
  })
  
  # -----------------------------------------
  # KPI: Average
  # -----------------------------------------
  
  output$avgBox <- renderValueBox({
    
    avg_val <- mean(
      summary_df[[input$metric]],
      na.rm = TRUE
    )
    
    valueBox(
      value = round(avg_val, 2),
      subtitle = "League Average",
      icon = icon("calculator"),
      color = "blue"
    )
  })
  
  # -----------------------------------------
  # KPI: Spread
  # -----------------------------------------
  
  output$spreadBox <- renderValueBox({
    
    spread <- max(
      summary_df[[input$metric]],
      na.rm = TRUE
    ) -
      min(
        summary_df[[input$metric]],
        na.rm = TRUE
      )
    
    valueBox(
      value = round(spread, 2),
      subtitle = "Range",
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  # -----------------------------------------
  # Ranking Plot
  # -----------------------------------------
  
  output$rankPlot <- renderPlotly({
    
    df <- ranking_data()
    
    p <- ggplot(
      df,
      aes(
        x = reorder(Team, .data[[input$metric]]),
        y = .data[[input$metric]],
        fill = .data[[input$metric]],
        text = paste(
          "Team:", Team,
          "<br>",
          input$metric, ":",
          round(.data[[input$metric]], 2)
        )
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Team Rankings by", input$metric),
        x = "",
        y = input$metric
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none"
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  # -----------------------------------------
  # Scatter Explorer
  # -----------------------------------------
  
  output$scatterPlot <- renderPlotly({
    
    x_var <- input$x_metric
    y_var <- input$y_metric
    size_var <- input$size_metric
    
    if (size_var == "None") {
      
      p <- ggplot(
        summary_df,
        aes(
          x = .data[[x_var]],
          y = .data[[y_var]],
          label = Team,
          text = paste(
            "Team:", Team,
            "<br>", x_var, ": ", round(.data[[x_var]], 2),
            "<br>", y_var, ": ", round(.data[[y_var]], 2)
          )
        )
      ) +
        geom_point(
          size = 5,
          colour = "#2C7FB8"
        ) +
        geom_text(
          vjust = -0.8,
          size = 4
        ) +
        theme_minimal(base_size = 14) +
        labs(
          title = paste(y_var, "vs", x_var),
          x = x_var,
          y = y_var
        )
      
    } else {
      
      p <- ggplot(
        summary_df,
        aes(
          x = .data[[x_var]],
          y = .data[[y_var]],
          size = .data[[size_var]],
          label = Team,
          text = paste(
            "Team:", Team,
            "<br>", x_var, ": ", round(.data[[x_var]], 2),
            "<br>", y_var, ": ", round(.data[[y_var]], 2),
            "<br>", size_var, ": ",
            round(.data[[size_var]], 2)
          )
        )
      ) +
        geom_point(
          colour = "#2C7FB8",
          alpha = 0.75
        ) +
        geom_text(
          vjust = -0.8,
          size = 4
        ) +
        scale_size_continuous(range = c(4, 15)) +
        theme_minimal(base_size = 14) +
        labs(
          title = paste(y_var, "vs", x_var),
          x = x_var,
          y = y_var,
          size = size_var
        )
    }
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  # -----------------------------------------
  # Data Table
  # -----------------------------------------
  
  output$summaryTable <- renderDT({
    
    datatable(
      summary_df,
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c(
          "copy",
          "csv",
          "excel"
        )
      )
    )
  })
}

# ====================================================
# RUN APP
# ====================================================

shinyApp(ui, server)
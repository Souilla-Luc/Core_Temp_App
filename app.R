#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages("shinyTime")
library(shiny)
library(shinyTime)
library(data.table)
library(readr) 
library(readr)
library(dplyr)
library(tidyr)
library(purrr)# Assure-toi que readr est bien chargé
library(plotly)

source("utils_split_pills.R")
# Define UI for application that draws a histogram


ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/academicons@1.9.1/css/academicons.min.css")
  ),
navbarPage(
  title = div(
"Core Temp App", tags$img(src = "pill1.png", height = "30px", style = "margin-right:10px;"),
    
  ),
  
  # --- Page 1 : Upload CSV ---
  tabPanel("Import your data", 
    sidebarLayout(
      sidebarPanel(
        fileInput("imported_file", "Upload Data"),
      HTML("<div style='margin-top:-10px; color:red;'><b>Attention :</b> It requires a CSV file.</div>"),
      tags$br(),
          uiOutput("pill_selector"),
          conditionalPanel(
            condition = "input.selected_pills != null && input.selected_pills.length > 0",
          HTML("<div style='margin-top:-10px; color:red;'><b>Attention :</b> It displays only the last measurement.</div>")
          ),
      tags$br(),
    ),
    mainPanel(
      h3("Preview CSV"), 
      # Show this only when no pills are selected
      conditionalPanel(        
        condition = "input.selected_pills == null || input.selected_pills.length == 0",
        tableOutput("raw_preview")
      ),
      # Show this only when pills are selected
      conditionalPanel(
       condition = "input.selected_pills != null && input.selected_pills.length > 0",
       tableOutput("selected_pills_preview")
      )
    )
  )
),

  # --- Page 2 : TIME INPUTS ---
  tabPanel(
    "Time Period",
    fluidPage(
      theme = bslib::bs_theme(bootswatch = "flatly"),
      dateInput("date", "Select date (YYYY-MM-DD)"),
      tags$br(),
      HTML("<div style='margin-top:-10px; color:red;'><b>Attention :</b> Time should be enter as HH:MM:SS </div>"),
      fluidRow(
        column(
          3,
          div(
            style = "background-color: lightblue;",
            h2("PRE-EXPOSURE"),
            timeInput("start_baseline_out", "Baseline START"),
            timeInput("end_baseline_out", "Baseline END")
          )
        ),
        column(
          3,
          div(
            style = "background-color: lightgreen;",
            h2("EXPOSURE"),
            timeInput("start_rest_chamber", "Rest START CHAMBER"),
            timeInput("end_rest_chamber", "Rest END CHAMBER")
          )
          ),
        column(
          3,
          div(
            style = "background-color: lightcoral;",
            h2("EXERCISE"),
            timeInput("start_pre_exo", "START PRE-EXERCISE"),
            timeInput("end_pre_exo", "END PRE-EXERCISE"),
            timeInput("start_exo", "START EXERCISE"),
            timeInput("end_exo", "END EXERCISE")
          )
        ),
        column(
          3,
          div(
            style = "background-color: lightyellow;",
            h2("POST-EXERCISE"),
            timeInput("start_cooldown", "START ACTIVE cooldown"),
            timeInput("end_cooldown", "END ACTIVE cooldown"),
            timeInput("start_passive", "START PASSIVE cooldown"),
            timeInput("end_passive", "END PASSIVE cooldown")
          )
          ),
        tags$br(),
        tags$br(),
        mainPanel(
          tableOutput("summary_stats")
        )
        
      )
      )
  ), 

# --- Page 3 :  GRAPHS ---
tabPanel(
  "Graph visual",
  fluidPage(
    h3("Temperature over time by period"),
    plotlyOutput("temp_plot", height = "500px")
  )
),

# --- Page 4 :  Download ---

tabPanel(
  "Export Data",
    fluidPage(
      # This one is linked by the id 'download'
      downloadButton('download',"Download cleaned data"),
    tags$br(),
    fluidRow(column(7,dataTableOutput('dto')))
    )
  )

# --- FOOTNOTES ---
),
tags$footer(
  style = "
      position: fixed;
      bottom: 0;
      width: 100%;
      height: 30px;
      background-color: #f1f1f1;
      color: #444;
      text-align: center;
      padding-top: 5px;
      font-size: 12px;
      z-index: 1000;
    ",
  HTML('
      &#169; 2025 | Luc Souilla, PhD | Some Rights Reserved | &#8729; 
      <a class="link-dark me-1" href="https://github.com/Souilla-Luc" title="GitHub"  rel="noopener">
        <i class="fab fa-github"></i>
      </a> |
      <a class="link-dark me-1" href="https://orcid.org/0000-0002-5773-1440" title="ORCID"  rel="noopener">
        <i class="ai ai-orcid"></i>
      </a> |
      <a href="https://luc-souilla.netlify.app/project/" > <img src="logo_rounded.svg" height="15" style="vertical-align: middle;"/> Website
      </a>
      
    ')
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Stp 1 1st read csv file
  raw_csv <- reactive({
    req(input$imported_file)
    df <-  fread(input$imported_file$datapath, header= T, encoding = "Latin-1", skip = 4, fill = TRUE) #Function to guess the delimiter
  })
  output$raw_preview <- renderTable({
    req(raw_csv())
    head(raw_csv(),100)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Step 2 — Split into tidy pill data frames
  pill_data_list <- reactive({
    df <- raw_csv()
    split_pills_with_gaps(df, cols_per_pill = 5, sep_cols = 2)
  })
  
  # Step 3 — Populate dropdown selector
  output$pill_selector <- renderUI({
    req(pill_data_list())
    selectInput(
      "selected_pills", 
      "Select your pill", 
      choices = names(pill_data_list()),
      multiple = TRUE
    )
  })
  
  selected_pill_data <- reactive({
    req(input$selected_pills)
    all_data <- pill_data_list()
    bind_rows(all_data[input$selected_pills]) 
  })
  # Step 4 — Show preview of selected pill
  cleaned_pill_data <- reactive({
    req(selected_pill_data())
    df <- selected_pill_data()
    
    is_only_pill_line <- apply(df, 1, function(row) {
      has_pill <- any(grepl("^Pill\\s*[1-9]+$", row, ignore.case = TRUE))
      others_empty <- all(row[!grepl("^Pill\\s*[1-9]+$", row, ignore.case = TRUE)] %in% c("", NA))
      return(has_pill && others_empty)
    })
    
    df_clean <- df[!is_only_pill_line, ]
    df_clean
    
  })
  
  output$selected_pills_preview <- renderTable({
    tail(cleaned_pill_data(), 20) #display the last 20 rows
  })
  
  
  # --- Page 2 : TIME TREATMENT ---
  
  # 🔽 AJOUTE CE BLOC ICI
  defined_periods <- reactive({
    req(
      input$start_baseline_out, input$end_baseline_out,
      input$start_rest_chamber, input$end_rest_chamber,
      input$start_pre_exo, input$end_pre_exo,
      input$start_exo, input$end_exo,
      input$start_cooldown, 
      input$end_cooldown,
      input$start_passive,
      input$end_passive
    )
    
    list(
      "Baseline" = c(input$start_baseline_out, input$end_baseline_out),
      "Rest" = c(input$start_rest_chamber, input$end_rest_chamber),
      "Pre-Exercise" = c(input$start_pre_exo, input$end_pre_exo),
      "Exercise" = c(input$start_exo, input$end_exo),
      "Cooldown Active" = c(input$start_cooldown, input$end_cooldown), #(3 min of cooldown)
      "Cooldown Passive" = c(input$start_passive, input$end_passive) #(10 minutes de cooldown)
    )
  })
#Create reactive to get data from users
  
  date_selected <- reactive({
    req(input$date)
    input$date
  })
  
  
  period_stats <- reactive({
    req(cleaned_pill_data())
    req(input$date)
    
    df <- cleaned_pill_data()
    df <- as.data.frame(df)
    
    if (!is.null(input$date)) {
      df$Date <- as.character(df$Date)
      selected_date <- as.character(input$date)
      df <- df[df$Date == selected_date, ]
    }
    
    names(df) <- tolower(trimws(names(df)))
    
    # Identifier dynamiquement la colonne de temps
    time_col <- "time"
    req(length(time_col) == 1)  # S'assurer qu’on en trouve une
    
    # Identifier la colonne de température (exemple : "Temperature" ou "Température")
    temp_col <- "temperature"
    req(length(temp_col) == 1)
    
    # Renommer temporairement pour simplifier
    # Convertir la colonne temps en POSIXct
    df <- as.data.frame(df)  # Pour éviter les contraintes de data.table
    df$Time_col <- df[[time_col]]
    
    df$Temperature <- as.numeric(df[[temp_col]])
    
    
    
    # Pour chaque période, filtrer et calculer les stats
    result <- list()
    
    # Périodes à découper minute par minute
    minute_split_phases <- c("Exercise", "Cooldown Active")
    periods <- defined_periods()
    for (period_name in names(periods)) {
      start <- format(periods[[period_name]][1], "%H:%M:%S")
      end <- format(periods[[period_name]][2], "%H:%M:%S")
      
      df_period <- df[df$Time_col >= start & df$Time_col <= end, ]
      
      if (nrow(df_period) > 0) {
        n_val <- sum(!is.na(df_period$Temperature))
        if (period_name %in% minute_split_phases) {
          # Subdivision par minute
          start_time <- as.POSIXct(start, format = "%H:%M:%S")
          end_time <- as.POSIXct(end, format = "%H:%M:%S")
          breaks <- seq(from = start_time, to = end_time, by = "1 min")
          
          for (i in seq_len(length(breaks) - 1)) {
            min_start <- format(breaks[i], "%H:%M:%S")
            min_end <- format(breaks[i + 1], "%H:%M:%S")
            df_min <- df[df$Time_col >= min_start & df$Time_col < min_end, ]
            n_min <- sum(!is.na(df_min$Temperature))
            
            if (nrow(df_min) > 0) {
              if (n_val > 0) {
              label <- paste0(period_name, "_", i, "min")
              result[[label]] <- c(
                mean = mean(df_min$Temperature, na.rm = TRUE),
                sd = sd(df_min$Temperature, na.rm = TRUE),
                n = n_min
              )
            }
            }
          }
        } else {
          if (n_val > 0) {
          # Cas classique
          result[[period_name]] <- c(
            mean = mean(df_period$Temperature, na.rm = TRUE),
            sd = sd(df_period$Temperature, na.rm = TRUE),
            n = n_val
          )
        }
      }
    }
  }
    if (length(result) == 0) return(NULL)
    
    stats_df <- as.data.frame(do.call(cbind, result))
    rownames(stats_df) <- c("Mean", "SD", "n")
    stats_df
  })
  
  output$summary_stats <- renderTable({
    print("renderTable called")
    period_stats()
  }, rownames = TRUE)
  
  # --- Page 3 : GRAPHIQUE ---
  
  
   # ou simplement print(df$Time_only) pour tout voir
  output$temp_plot<-renderPlotly({
   req(cleaned_pill_data())
    req(input$date)
    
    
    
    df <- cleaned_pill_data()
    df <- as.data.frame(df)
    
    if (!is.null(input$date)) {
      df$Date <- as.character(df$Date)
      selected_date <- as.character(input$date)
      df <- df[df$Date == selected_date, ]
    }
    df$Date <- as.character(df$Date)
    df$Time <- as.character(df$Time)
    
    df <- df[!grepl("Date", df$Date), ] #supprime la premiere ligne qui se joitn au reste
    print(head(df$Date))
    
    df$Time_only <- as.POSIXct(paste(df$Date, df$Time), format = "%Y-%m-%d %H:%M:%S")
    
    
    print("Aperçu de Date + Time :")
    print(head(paste(df$Date, df$Time)))
    # 👇 Affiche les noms des colonnes pour vérification
    print("Colonnes disponibles dans df :")
    print(names(df))
    
    # if (!is.null(input$date)) {
    #   df$Time_only <- as.POSIXct(paste(df$Date, df$Time), format = "%Y-%m-%d %H:%M:%S")
    # }
   
    #df$Time_col <-  df$Time
    df$Temperature <- as.numeric(df$Temperature)
    
    periods <- defined_periods()
    df$Period <- NA
    
    df$Time_only_hms <- as.POSIXct(format(df$Time_only, format = "%H:%M:%S"), format = "%H:%M:%S")
    
    for (p in names(periods)) {
      start <- as.POSIXct(format(periods[[p]][1], "%H:%M:%S"), format = "%H:%M:%S")
      end   <- as.POSIXct(format(periods[[p]][2], "%H:%M:%S"), format = "%H:%M:%S")
      df$Period[df$Time_only_hms >= start & df$Time_only_hms <= end] <- p
    }
    
    df <- df[!is.na(df$Period), ]
    df <- df %>%
      group_by(Period) %>%
      filter(sum(!is.na(Temperature)) >= 3) %>%
      ungroup()
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    min_temp <- min(df$Temperature, na.rm = TRUE) - 0.1
    max_temp <- max(df$Temperature, na.rm = TRUE) + 0.1
    
    
    p<-ggplot(df, aes(x = Time_only, y = Temperature, color = Period)) +
      geom_line(size = 1) +
      scale_x_datetime(date_breaks = "15 min", date_labels = "%H:%M") +
      expand_limits(y = c(min(df$Temperature, na.rm = TRUE) - 1, max(df$Temperature, na.rm = TRUE) + 1)) +
      labs(title = "", x = "Time", y = "Temperature (°C)") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(p)
    
     })
  
  
  print("Contenu de la colonne Time_only :")
  print(head(df$Time_only, 20)) 
  
  # --- Page 4 : DOWNLOAD THE FILE ---
  
    output$download <- downloadHandler(
    filename = function(){
      req(input$imported_file)
      paste0(tools::file_path_sans_ext(input$imported_file$name), "_treated.tsv")
    },
    content = function(file){
      stats<-period_stats()
      req(stats)  # S'assure que stats est non NULL
      # Formater les valeurs numériques à 2 décimales
      stats_rounded <- as.data.frame(lapply(stats, function(col) {
        if (is.numeric(col)) {
          return(round(col, 2))
        } else {
          return(col)
        }
      }))
      
      # Ajouter manuellement la colonne "Description"
      stats_rounded$Description <- c("Mean", "SD", "n")  # Forcé ici
     
      # Ajouter la colonne Date
      req(input$date)
      stats_rounded$Date <- as.character(input$date)
      
      # Réorganiser les colonnes : Date à gauche
      stats_rounded <- stats_rounded[, c("Date", "Description", setdiff(names(stats_rounded), c("Date", "Description")))]
      
      write.table(
        stats_rounded,
        file,
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
      )
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

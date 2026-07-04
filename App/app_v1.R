library(wehoop)
library(tidyverse)
library(shiny)
library(hoopR)
library(shinydashboard)
library(shinythemes)
library(DT)
library(dplyr)
library(purrr)
library(heatmaply)

poss_player <- function(g_id, league){
  bs_df <- if (league == "WNBA") {
    wehoop::espn_wnba_player_box(game_id = g_id)
  } else {
    hoopR::espn_nba_player_box(game_id = g_id)
  }
  
  bs_df <- bs_df %>%
    select(c("game_id","game_date","athlete_display_name","team_id","team_name","minutes","field_goals_made","field_goals_attempted","free_throws_made","free_throws_attempted","offensive_rebounds",
             "defensive_rebounds","rebounds","assists","steals","blocks","turnovers","points","starter")) %>%
    within({
      possessions <- field_goals_attempted + 0.44 * free_throws_attempted + turnovers - offensive_rebounds 
      FP <- points + 1.2 * rebounds + 1.5 * assists + 3 * (steals + blocks) - turnovers
      pts_per_poss <- round(points * 100 / possessions, 2)
    })
  
  return(bs_df)
}

summarise_stats <- function(df, team, dates, suffix, agg_fun = mean) {
  team_df <- df %>%
    filter(team_name == team, game_date %in% dates) %>%
    group_by(team_name, athlete_display_name) %>%
    summarise(across(c("minutes", "field_goals_attempted", "field_goals_made",
                       "free_throws_made", "free_throws_attempted", "offensive_rebounds",
                       "defensive_rebounds", "rebounds", "assists", "steals",
                       "blocks", "turnovers", "points", "starter"), agg_fun, na.rm = TRUE),
              .groups = "drop")
  
  # Get total team blocks and steals (after aggregation)
  total_blocks <- sum(team_df$blocks, na.rm = TRUE)
  total_steals <- sum(team_df$steals, na.rm = TRUE)
  total_turnovers <- sum(team_df$turnovers, na.rm = TRUE)
  
  team_df %>%
    mutate(
      !!paste0("poss_", suffix) := round(field_goals_attempted + 0.44 * free_throws_attempted + turnovers - offensive_rebounds, 2),
      !!paste0("FP_", suffix) := round(points + 1.2 * rebounds + 1.5 * assists + 3 * (steals + blocks) - turnovers, 2),
      !!paste0("BLK_", suffix) := round(100 * blocks / ifelse(total_blocks == 0, 1, total_blocks), 2),
      !!paste0("STL_", suffix) := round(100 * steals / ifelse(total_steals == 0, 1, total_steals), 2),
      !!paste0("TO_", suffix) := round(100 * turnovers / ifelse(total_turnovers == 0, 1, total_turnovers), 2),      
    ) %>%
    select(team_name, athlete_display_name,
           !!sym(paste0("poss_", suffix)),
           !!sym(paste0("FP_", suffix)),
           !!sym(paste0("BLK_", suffix)),
           !!sym(paste0("STL_", suffix)),
           !!sym(paste0("TO_", suffix)),)
}

summarise_stats <- function(df, team, dates, suffix, agg_fun = mean) {
  team_df <- df %>%
    filter(team_name == team, game_date %in% dates) %>%
    group_by(team_name, athlete_display_name) %>%
    summarise(across(c("minutes", "field_goals_attempted", "field_goals_made",
                       "free_throws_made", "free_throws_attempted", "offensive_rebounds",
                       "defensive_rebounds", "rebounds", "assists", "steals",
                       "blocks", "turnovers", "points", "starter"), agg_fun, na.rm = TRUE),
              .groups = "drop")
  
  # Totals for percent calculations
  total_blocks <- sum(team_df$blocks, na.rm = TRUE)
  total_steals <- sum(team_df$steals, na.rm = TRUE)
  total_turnovers <- sum(team_df$turnovers, na.rm = TRUE)
  total_defense <- total_blocks + total_steals
  
  team_df %>%
    mutate(
      !!paste0("poss_", suffix)   := round(field_goals_attempted + 0.44 * free_throws_attempted + turnovers - offensive_rebounds, 2),
      !!paste0("FP_", suffix)     := round(points + 1.2 * rebounds + 1.5 * assists + 3 * (steals + blocks) - turnovers, 2),
      !!paste0("DEF_", suffix)    := round(100 * (blocks + steals) / ifelse(total_defense == 0, 1, total_defense), 2),
      !!paste0("TO_", suffix)     := round(100 * turnovers / ifelse(total_turnovers == 0, 1, total_turnovers), 2),
      !!paste0("ORTG_", suffix)   := round(100 * points / ifelse((field_goals_attempted + 0.44 * free_throws_attempted + turnovers - offensive_rebounds) == 0, 1,
                                                                 field_goals_attempted + 0.44 * free_throws_attempted + turnovers - offensive_rebounds), 2)
    ) %>%
    select(team_name, athlete_display_name,
           !!sym(paste0("poss_", suffix)),
           !!sym(paste0("FP_", suffix)),
           !!sym(paste0("DEF_", suffix)),
           !!sym(paste0("TO_", suffix)),
           !!sym(paste0("ORTG_", suffix)))
}

ui <- fluidPage(
  tags$div(
    style = "text-align:center; padding: 10px;",
    tags$h1("🏀 WNBA Stats Explorer", style = "color:#2c3e50; font-weight:bold;")
  ),
  theme = shinythemes::shinytheme("cosmo"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      fluidRow(
        column(width = 6,
               actionButton("women", label = "WNBA", width = "100%")
        ),
        column(width = 6,
               actionButton("men", label = "NBA", width = "100%")
        )
      ),
      
      br(),
      br(),
      br(),
      selectInput("team1", label = "Select Home Team", choices = NULL),
      selectInput("team2", label = "Select Away Team", choices = NULL),
      actionButton("analyze_btn", "Analyze Match", icon = icon("search")),
      tags$p(textOutput("league_text")),
      tags$p(
        tags$span("Data updated on: ", style = "font-size: 10px; color: gray;"),
        textOutput("data_update", inline = TRUE)
      ),
      
      br(),
      
      tags$h4("Upcoming Schedule", style = "font-weight : bold; text-align : center;"),
      
      uiOutput("upcoming_matches"),  # Will render upcoming match info
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Summary Stats", DTOutput("Table")

        ),
        
        tabPanel("Player Correlations", tags$h3("Player Correlations will come here..!! WIP"),
                 
                 fluidRow(column(4, selectInput("corr_team", label = "Select Team", choices = c("Choice A", "Choice B")) ),
                          column(4, selectInput("corr_metric", label = "Select Metric", choices = c("Fantasy Points", "Possessions")))),
                 
                 plotlyOutput("corr_plot", height = "600px")
        )
      )
      )
    )
  )

server <- function(input, output, session){
  
  rv <- reactiveValues(
    league = "WNBA",
    teams = NULL,
    schedule = NULL,
    results = NULL,
    upcoming_schedule = NULL
  )
  
  output$league_text <- renderText({ paste("Selected League:", rv$league) })
  
  
  master_data <- eventReactive(input$analyze_btn, {
    req(input$team1, input$team2)
    selected_teams <- rv$teams %>% filter(display_name %in% c(input$team1, input$team2)) %>% pull(short_name)
    
    games_list <- rv$results %>%
      filter(home_name %in% selected_teams | away_name %in% selected_teams) %>%
      pull(id)
    
    master_df <- data.frame()
    
    withProgress(message = "Fetching Player Stats..", value = 0, {
      master_df <- map_dfr(seq_along(games_list), function(i) {
        incProgress(1 / length(games_list))
        poss_player(games_list[i], rv$league)
      })
    })
    
    master_df
  })
    

  
  filtered_data <-reactive({
    
    master_df <- master_data()
    
    req(master_df)
    
    selected_teams <- rv$teams %>% filter(display_name %in% c(input$team1, input$team2)) %>% pull(short_name)
    
    # Initialize final df
    ts_df <- data.frame()
    
    # Loop through each team
    for (i in selected_teams) {
      
      all_dates <- master_df %>%
        drop_na() %>%
        filter(team_name == i) %>%
        arrange(desc(game_date)) %>%
        pull(game_date) %>%
        unique()
      
      # Latest 1, 3, 5 dates
      latest_1 <- all_dates[1]
      latest_3 <- all_dates[1:3]
      latest_5 <- all_dates[1:5]
      
      # Compute summaries
      df_L1  <- summarise_stats(master_df, i, latest_1, "L1", sum)
      df_L3  <- summarise_stats(master_df, i, latest_3, "L3", mean)
      df_L5  <- summarise_stats(master_df, i, latest_5, "L5", mean)
      df_avg <- summarise_stats(master_df, i, all_dates, "avg", mean)
      
      # Merge all summaries
      stats_df <- reduce(list(df_avg, df_L1, df_L3, df_L5), ~merge(.x, .y, by = c("team_name", "athlete_display_name"), all.x = TRUE)) %>%
        select(team_name, athlete_display_name,
               poss_avg, FP_avg, poss_L1, poss_L3, poss_L5,
               FP_L1, FP_L3, FP_L5, DEF_L1, DEF_L3, DEF_L5, ORTG_L1, ORTG_L3, ORTG_L5, TO_L1, TO_L3, TO_L5) %>%
        arrange(desc(FP_avg))
      
      # Append to final df
      ts_df <- bind_rows(ts_df, stats_df) %>% arrange(desc(team_name))
    }
    
    ts_df
    
  })
  

  output$Table <- renderDT({
    df <- filtered_data()
    
    # Identify columns by type
    poss_cols <- names(df)[grepl("^poss_", names(df))]
    fp_cols   <- names(df)[grepl("^FP_", names(df))]
    stl_cols  <- names(df)[grepl("^DEF_", names(df))]
    blk_cols  <- names(df)[grepl("^ORTG_", names(df))]
    to_cols  <- names(df)[grepl("^TO_", names(df))]
    
    # Define color palettes for each category
    poss_palette <- c("#e0f7fa", "#80deea", "#26c6da", "#00838f")  # Blue-green
    fp_palette   <- c("#e8f5e9", "#a5d6a7", "#66bb6a", "#2e7d32")  # Greens
    stl_palette  <- c("#fff3e0", "#ffcc80", "#ffa726", "#ef6c00")  # Orange
    blk_palette  <- c("#f3e5f5", "#ce93d8", "#ab47bc", "#6a1b9a")  # Purple
    to_palette   <- c("#fde0dc", "#f8bbd0", "#f48fb1", "#c2185b")
    
    # Build datatable
    dt <- datatable(df, options = list(pageLength = 100))
    
    # Helper to apply color formatting
    apply_format <- function(dt_obj, cols, palette) {
      for (col in cols) {
        dt_obj <- formatStyle(
          dt_obj,
          columns = col,
          backgroundColor = styleInterval(
            quantile(df[[col]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
            palette
          ),
          color = "black",
          fontWeight = "bold"
        )
      }
      return(dt_obj)
    }
    
    # Apply formatting by group
    dt <- apply_format(dt, poss_cols, poss_palette)
    dt <- apply_format(dt, fp_cols,   fp_palette)
    dt <- apply_format(dt, stl_cols,  stl_palette)
    dt <- apply_format(dt, blk_cols,  blk_palette)
    dt <- apply_format(dt, to_cols,  to_palette)
    
    dt
  })
  
 observe({
   req(input$team1, input$team2)
   
   corr_teams = c(input$team1, input$team2)
   
   updateSelectInput(session, "corr_team", choices = corr_teams, selected = corr_teams[1])
   
 }) 
 
 corr_df <- reactive({
   
   master_df <- master_data()
   
   req(input$corr_team, input$corr_metric, master_df)
   
   if(input$corr_metric == "Possessions"){
     cor_df = master_df %>% filter(team_name == rv$teams %>% filter(display_name == input$corr_team) %>% pull(short_name) )%>% 
       select(c("athlete_display_name","game_id","FP","possessions")) %>% 
       pivot_wider(id_cols = "game_id", names_from = "athlete_display_name", values_from = "possessions") %>% 
       mutate(across(everything(), ~replace_na(.,0))) %>% select(-c("game_id")) %>% cor() %>% round(2)
   }else{
     
     cor_df = master_df %>% filter(team_name == rv$teams %>% filter(display_name == input$corr_team) %>% pull(short_name) )%>% 
       select(c("athlete_display_name","game_id","FP","possessions")) %>% 
       pivot_wider(id_cols = "game_id", names_from = "athlete_display_name", values_from = "FP") %>% 
       mutate(across(everything(), ~replace_na(.,0))) %>% select(-c("game_id")) %>% cor() %>% round(2)
   }
   cor_df
   
 })
 
 output$data_update <- renderText({
   
   req(input$team1, input$team2)
   
   selected_teams <- c(input$team1, input$team2)
   latest_date = isolate({rv$schedule %>% filter(game_json == TRUE) %>% filter(home_display_name %in% selected_teams | away_display_name %in% selected_teams) %>% arrange(desc(game_date)) %>% 
     slice(1)})
   
   latest_date = format(latest_date$game_date, "%B %d, %Y")
 })
 
 # Change to WNBA
 observeEvent(input$women, {
   print("WNBA got triggered")
   isolate({
     rv$league <- "WNBA"
     rv$teams <- espn_wnba_teams() %>% select(display_name, short_name)
     rv$schedule <- load_wnba_schedule(seasons = most_recent_wnba_season())
     rv$results <- rv$schedule %>% filter(game_json == TRUE) %>% select(id, game_date, home_name, away_name)
     rv$upcoming_schedule <- rv$schedule %>% filter(game_json == FALSE) %>% select(id, game_date, home_name, away_name)
     
     updateSelectInput(session, "team1", choices = rv$teams$display_name, selected = rv$teams$display_name[1])
     updateSelectInput(session, "team2", choices = rv$teams$display_name, selected = rv$teams$display_name[2])
   })
 })
 
 # Change to NBA
 observeEvent(input$men, {
   print("NBA got triggered")
   isolate({
     rv$league <- "NBA"
     rv$teams <- espn_nba_teams() %>% select(display_name, short_name) 
     rv$schedule <- load_nba_schedule(seasons = most_recent_nba_season())
     rv$results <- rv$schedule %>% filter(game_json == TRUE) %>% select(id, game_date, home_name, away_name)
     rv$upcoming_schedule <- rv$schedule %>% filter(game_json == FALSE) %>% select(id, game_date, home_name, away_name)
     
     updateSelectInput(session, "team1", choices = rv$teams$display_name, selected = rv$teams$display_name[1])
     updateSelectInput(session, "team2", choices = rv$teams$display_name, selected = rv$teams$display_name[2])
   })
 }) 
 output$corr_plot <- renderPlotly({
   df <- corr_df()
   
   # Convert to matrix (if not already)
   mat <- as.matrix(df)
   
   # Plotly heatmap
   plot_ly(
     x = colnames(mat),
     y = rownames(mat),
     z = mat,
     type = "heatmap",
     colorscale = "RdYlGn",  # Bright diverging colors
     zmin = -1,
     zmax = 1,
     showscale = TRUE
   ) %>%
     layout(
       title = "Player Correlation Heatmap",
       xaxis = list(title = "", tickangle = -45),
       yaxis = list(title = ""),
       margin = list(l = 100, b = 100)
     ) %>%
     add_annotations(
       x = rep(colnames(mat), each = nrow(mat)),
       y = rep(rownames(mat), times = ncol(mat)),
       text = sprintf("%.2f", as.vector(mat)),
       showarrow = FALSE,
       font = list(color = "black", size = 10)
     )
 })
}

shinyApp(ui = ui, server = server)



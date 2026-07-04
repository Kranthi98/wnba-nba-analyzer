library(wehoop)
library(tidyverse)
library(shiny)
library(hoopR)
library(shinydashboard)
library(shinythemes)
library(DT)
library(dplyr)
library(purrr)
library(rlang)
library(heatmaply)

poss_player <- function(g_id, league){
  bs_df <- if (league == "WNBA") {
    wehoop::espn_wnba_player_box(game_id = g_id)
  } else {
    hoopR::espn_nba_player_box(game_id = g_id)
  }
  
  bs_df <- bs_df %>%
    select(c("game_id","game_date","athlete_display_name","team_id","team_name","minutes","field_goals_made","field_goals_attempted","free_throws_made","free_throws_attempted","offensive_rebounds",
             "defensive_rebounds","rebounds","assists","steals","blocks","turnovers","points","starter", "fouls", "three_point_field_goals_made","three_point_field_goals_attempted"))
  
  return(bs_df)
}

summarise_stats_v1 <- function(df, team, dates, gm_ids, suffix, agg_fun = mean) {
  
  # Team players' box score
  team_df <- df %>%
    filter(team_name == team, game_date %in% dates) %>%
    group_by(team_name, athlete_display_name) %>%
    summarise(across(c("minutes", "field_goals_attempted", "field_goals_made", "three_point_field_goals_made",
                       "free_throws_made", "free_throws_attempted", "offensive_rebounds",
                       "defensive_rebounds", "rebounds", "assists", "steals",
                       "blocks", "turnovers", "points", "fouls", "starter"), agg_fun, na.rm = TRUE),
              .groups = "drop")
  
  # Opponent aggregated stats (as 1 row)
  opp_df <- df %>%
    filter(game_id %in% gm_ids, team_name != team) %>%
    mutate(athlete_display_name = "Opponent") %>%
    group_by(athlete_display_name) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
    select(-athlete_display_name) %>%
    rename_with(~paste0(., "_opp"))
  print(opp_df)
  # Defensive team totals
  total_blocks <- sum(team_df$blocks, na.rm = TRUE)
  total_steals <- sum(team_df$steals, na.rm = TRUE)
  total_turnovers <- sum(team_df$turnovers, na.rm = TRUE)
  total_defense <- total_blocks + total_steals
  
  # Join player stats with opponent totals
  df_joined <- cbind(team_df, opp_df)
  
  # Now calculate advanced metrics
  df_joined <- df_joined %>%
    rowwise() %>%
    mutate(
      # Simple Possession & Fantasy Points
      !!paste0("poss_", suffix) := round(field_goals_attempted + 0.44 * free_throws_attempted + turnovers - offensive_rebounds, 2),
      !!paste0("FP_", suffix) := round(points + 1.2 * rebounds + 1.5 * assists + 3 * (steals + blocks) - turnovers, 2),
      !!paste0("DEF_", suffix) := round(100 * (blocks + steals) / ifelse(total_defense == 0, 1, total_defense), 2),
      !!paste0("TO_", suffix) := round(100 * turnovers / ifelse(total_turnovers == 0, 1, total_turnovers), 2),
      
      # --- DRtg Calculation (Dean Oliver-based) ---
      opp_DFG = ifelse(field_goals_attempted_opp == 0, 0, field_goals_made_opp / field_goals_attempted_opp),
      opp_DOR = ifelse(rebounds_opp == 0, 0, offensive_rebounds_opp / rebounds_opp),
      fmwt = ifelse((opp_DFG*(1-opp_DOR)+(1-opp_DFG)*opp_DOR)==0, 0.5,
                    (opp_DFG*(1-opp_DOR)) / (opp_DFG*(1-opp_DOR) + (1-opp_DFG)*opp_DOR)),
      stops1 = steals + blocks * fmwt * (1 - 1.07 * opp_DOR) + defensive_rebounds * (1 - fmwt),
      stops2 = (((field_goals_attempted_opp - field_goals_made_opp - sum(blocks))/sum(minutes)) * fmwt * (1 - 1.07 * opp_DOR)) +
        (((turnovers_opp - sum(steals))/sum(minutes)) * minutes) +
        ((fouls/sum(fouls)) * 0.4 * free_throws_attempted_opp * (1 - (free_throws_made_opp/free_throws_attempted_opp))^2),
      stops = stops1 + stops2,
      opp_team_poss = field_goals_attempted_opp + 0.44 * free_throws_attempted_opp - offensive_rebounds_opp + turnovers_opp,
      stop_per = (stops * minutes_opp) / (opp_team_poss * sum(minutes)),
      Tm_Drtg = 100 * (points_opp / opp_team_poss),
      Dpts_per_Scposs = points_opp / (field_goals_made_opp + (1 - (1 - (free_throws_made_opp/free_throws_attempted_opp))^2) * 0.4 * free_throws_attempted_opp),
      !!paste0("DRTG_", suffix) := round(Tm_Drtg + 0.2 * (100 * Dpts_per_Scposs * (1 - stop_per) - Tm_Drtg), 2),
      
      # --- ORtg Calculation (Dean Oliver-based) ---
        Team_ScPoss = sum(field_goals_made, na.rm = TRUE) + 
          (1 - (1 - (sum(free_throws_made, na.rm = TRUE) / ifelse(sum(free_throws_attempted, na.rm = TRUE) == 0, 1, sum(free_throws_attempted, na.rm = TRUE))))^2) * 
          sum(free_throws_attempted, na.rm = TRUE) * 0.4,
        
        Team_Play_per = Team_ScPoss / (
          sum(field_goals_attempted, na.rm = TRUE) +
            sum(free_throws_attempted, na.rm = TRUE) * 0.4 +
            sum(turnovers, na.rm = TRUE)
        ),
        
        Team_ORB_per = sum(offensive_rebounds, na.rm = TRUE) / 
          (sum(offensive_rebounds, na.rm = TRUE) + (rebounds_opp - offensive_rebounds_opp)),
        
        Team_ORB_Weight = ((1 - Team_ORB_per) * Team_Play_per) /
          ifelse(((1 - Team_ORB_per) * Team_Play_per + Team_ORB_per * (1 - Team_Play_per)) == 0, 1,
                 (1 - Team_ORB_per) * Team_Play_per + Team_ORB_per * (1 - Team_Play_per)),
        
        ORB_Part = offensive_rebounds * Team_ORB_Weight * Team_Play_per,
        
        FT_Part = ifelse(free_throws_attempted == 0, 0,
                         (1 - (1 - (free_throws_made / free_throws_attempted))^2) * 0.4 * free_throws_attempted),
        
        qassists = ((minutes / (sum(minutes, na.rm = TRUE) / 5)) *
                      (1.14 * ((sum(assists, na.rm = TRUE) - assists) / ifelse(sum(field_goals_made, na.rm = TRUE) == 0, 1, sum(field_goals_made, na.rm = TRUE))))) +
          ((((sum(assists, na.rm = TRUE) / sum(minutes, na.rm = TRUE)) * minutes * 5 - assists) /
              ifelse(((sum(field_goals_made, na.rm = TRUE) / sum(minutes, na.rm = TRUE)) * minutes * 5 - field_goals_made) == 0, 1,
                     (sum(field_goals_made, na.rm = TRUE) / sum(minutes, na.rm = TRUE)) * minutes * 5 - field_goals_made)) *
             (1 - (minutes / (sum(minutes, na.rm = TRUE) / 5)))),
        
        FG_Part = field_goals_made * 
          (1 - 0.5 * ((points - free_throws_made) / ifelse(2 * field_goals_attempted == 0, 1, 2 * field_goals_attempted)) * qassists),
        
        AST_Part = 0.5 * (((sum(points, na.rm = TRUE) - sum(free_throws_made, na.rm = TRUE)) - (points - free_throws_made)) /
                            ifelse((2 * (sum(field_goals_attempted, na.rm = TRUE) - field_goals_attempted)) == 0, 1,
                                   2 * (sum(field_goals_attempted, na.rm = TRUE) - field_goals_attempted))) * assists,
        
        ScPoss = (FG_Part + AST_Part + FT_Part) * 
          (1 - (sum(offensive_rebounds, na.rm = TRUE) / Team_ScPoss) * Team_ORB_Weight * Team_Play_per) + ORB_Part,
        
        FGxPoss = (field_goals_attempted - field_goals_made) * (1 - 1.07 * Team_ORB_per),
        
        FTxPoss = ifelse(free_throws_attempted == 0, 0,
                         ((1 - (free_throws_made / free_throws_attempted))^2) * 0.4 * free_throws_attempted),
        
        TotPoss = ScPoss + FGxPoss + FTxPoss + turnovers,
        
        PProd_ORB_Part = offensive_rebounds * Team_ORB_Weight * Team_Play_per *
          (sum(points, na.rm = TRUE) / 
             ifelse((sum(field_goals_made, na.rm = TRUE) + 
                       (1 - (1 - (sum(free_throws_made, na.rm = TRUE) / ifelse(sum(free_throws_attempted, na.rm = TRUE) == 0, 1, sum(free_throws_attempted, na.rm = TRUE))))^2) * 
                       0.4 * sum(free_throws_attempted, na.rm = TRUE)) == 0, 1,
                    sum(field_goals_made, na.rm = TRUE) + 
                      (1 - (1 - (sum(free_throws_made, na.rm = TRUE) / ifelse(sum(free_throws_attempted, na.rm = TRUE) == 0, 1, sum(free_throws_attempted, na.rm = TRUE))))^2) * 
                      0.4 * sum(free_throws_attempted, na.rm = TRUE))),
        
        PProd_AST_Part = 2 * ((sum(field_goals_made, na.rm = TRUE) - field_goals_made + 
                                 0.5 * (sum(three_point_field_goals_made, na.rm = TRUE) - three_point_field_goals_made)) /
                                ifelse((sum(field_goals_made, na.rm = TRUE) - field_goals_made) == 0, 1,
                                       sum(field_goals_made, na.rm = TRUE) - field_goals_made)) * 0.5 *
          (((sum(points, na.rm = TRUE) - sum(free_throws_made, na.rm = TRUE)) - (points - free_throws_made)) /
             ifelse((2 * (sum(field_goals_attempted, na.rm = TRUE) - field_goals_attempted)) == 0, 1,
                    2 * (sum(field_goals_attempted, na.rm = TRUE) - field_goals_attempted))) * assists,
        
        PProd_FG_Part = 2 * (field_goals_made + 0.5 * three_point_field_goals_made) * 
          (1 - 0.5 * ((points - free_throws_made) / 
                        ifelse(2 * field_goals_attempted == 0, 1, 2 * field_goals_attempted)) * qassists),
        
        PProd = (PProd_FG_Part + PProd_AST_Part + free_throws_made) * 
          (1 - (sum(offensive_rebounds, na.rm = TRUE) / Team_ScPoss) * Team_ORB_Weight * Team_Play_per) + PProd_ORB_Part,
        
        !!paste0("ORTG_", suffix) := round(ifelse(TotPoss == 0, NA, 100 * (PProd / TotPoss)), 2)
      
      
    ) %>%
    ungroup()
  
  # Select final output columns
  df_joined %>%
    select(team_name, athlete_display_name,
           !!sym(paste0("poss_", suffix)),
           !!sym(paste0("FP_", suffix)),
           !!sym(paste0("DEF_", suffix)),
           !!sym(paste0("TO_", suffix)),
           !!sym(paste0("ORTG_", suffix)),
           !!sym(paste0("DRTG_", suffix)))
  
}


summarise_stats <- function(df, team, dates, gm_ids, suffix, agg_fun = mean){
  
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
    for (i in selected_teams){
      
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
      
      gid_1 <- master_df %>% filter(team_name == i) %>% filter(game_date %in% latest_1) %>% pull(game_id) %>% unique()
      gid_3 <- master_df %>% filter(team_name == i) %>% filter(game_date %in% latest_3) %>% pull(game_id) %>% unique()
      gid_5 <- master_df %>% filter(team_name == i) %>% filter(game_date %in% latest_5) %>% pull(game_id) %>% unique()
      gid_all <- master_df %>% filter(team_name == i) %>% pull(game_id) %>% unique()
      
      # Compute summaries
      df_L1  <- summarise_stats_v1(master_df, i, latest_1, gid_1, "L1", mean)
      print(names(df_L1))
      df_L3  <- summarise_stats_v1(master_df, i, latest_3, gid_3, "L3", mean)
      print(names(df_L3))
      df_L5  <- summarise_stats_v1(master_df, i, latest_5, gid_5, "L5", mean)
      print(names(df_L5))
      df_avg <- summarise_stats_v1(master_df, i, all_dates, gid_all, "avg", mean)
      print(names(df_avg))
      
      # Merge all summaries
      stats_df <- reduce(list(df_avg, df_L1, df_L3, df_L5), ~merge(.x, .y, by = c("team_name", "athlete_display_name"), all.x = TRUE)) %>%
        select(team_name, athlete_display_name,
               poss_avg, FP_avg, poss_L1, poss_L3, poss_L5,
               FP_L1, FP_L3, FP_L5, DEF_L1, DEF_L3, DEF_L5, ORTG_L1, ORTG_L3, ORTG_L5,, DRTG_L1, DRTG_L3, DRTG_L5, TO_L1, TO_L3, TO_L5) %>%
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
    ortg_cols  <- names(df)[grepl("^ORTG_", names(df))]
    drtg_cols <- names(df)[grepl("^DRTG_", names(df))]
    to_cols  <- names(df)[grepl("^TO_", names(df))]
    
    # Define color palettes for each category
    poss_palette <- c("#e0f7fa", "#80deea", "#26c6da", "#00838f")  # Blue-green
    fp_palette   <- c("#e8f5e9", "#a5d6a7", "#66bb6a", "#2e7d32")  # Greens
    stl_palette  <- c("#fff3e0", "#ffcc80", "#ffa726", "#ef6c00")  # Orange
    ortg_palette  <- c("#f3e5f5", "#ce93d8", "#ab47bc", "#6a1b9a")  # Purple
    drtg_palette <- c("#0d47a1", "#42a5f5", "#90caf9", "#e3f2fd")  # Dark blue to light blue
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
    dt <- apply_format(dt, ortg_cols,  ortg_palette)
    dt <- apply_format(dt, drtg_cols,  drtg_palette)
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



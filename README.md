# wnba-nba-analyzer
# 🏀 Hoops Explorer

An interactive **Shiny dashboard** for exploring **NBA** and **WNBA** player performance using advanced basketball metrics, fantasy scoring, and player correlation analysis.

## Live App

🚀 **Try the app here:**  
**https://clutchdata.shinyapps.io/NBA-lytics/**

---

## Features

### League Support
- WNBA
- NBA

### Team Matchup Analysis
Compare any two teams and analyze players based on recent performances.

### Advanced Metrics
The app calculates player statistics over multiple rolling windows:

- Last Game
- Last 3 Games
- Last 5 Games
- Season Average

Metrics include:

- Possessions
- Fantasy Points
- Offensive Rating (ORtg)
- Defensive Rating (DRtg)
- Defensive Contribution %
- Turnover %

### Interactive Tables

- Color-coded performance tables
- Easy comparison across players
- Rolling averages for quick analysis

### Player Correlation Heatmap

Visualize player correlations using:

- Fantasy Points
- Possessions

Useful for:

- DFS lineup research
- Identifying player stacks
- Understanding teammate relationships

---

## Data Source

The application uses:

- **hoopR**
- **wehoop**
- ESPN game and box score data

Data is retrieved dynamically whenever a matchup is analyzed.

---

## Built With

- R
- Shiny
- tidyverse
- hoopR
- wehoop
- DT
- plotly
- heatmaply

---

## Installation

Clone the repository.

```r
git clone https://github.com/kranthi98/wnba-nba-analyzer.git
```

Install packages.

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "DT",
  "plotly",
  "heatmaply",
  "shinydashboard",
  "shinythemes",
  "purrr",
  "rlang"
))
```

Install sports packages.

```r
remotes::install_github("sportsdataverse/hoopR")
remotes::install_github("SportsDataverse/wehoop")
```

Run the application.

```r
shiny::runApp()
```

---



## Roadmap

- [ ] Player similarity search
- [ ] Team comparison dashboard
- [ ] Shot charts
- [ ] Player trend visualizations
- [ ] Export tables to CSV
- [ ] Lineup analysis
- [ ] Advanced possession metrics
- [ ] Fantasy projections

---

## Acknowledgements

- ESPN
- hoopR
- wehoop
- SportsDataverse

---

## License

MIT License

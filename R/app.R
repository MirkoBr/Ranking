### ============================================================================
###
###                       TFC Ranking Dashboard App
###
### ----------------------------------------------------------------------------
###


### ============================================================================
### Load libs & modules
### ----------------------------------------------------------------------------
###
library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(readxl)
library(viridis)
library(dplyr)
library(ggrepel)
library(dashboardthemes)
library(stringi)
library(ggplot2)
library(ggbeeswarm)
library(tidytext)
library(tidyverse)
library(readxl)
library(DT)
source("functions.R")

### ============================================================================
### Initialize data globally
### ----------------------------------------------------------------------------
###

# DATA = importData("./data/Ranking.xlsx")
MATCHDATA = parsePoolFromExcel(read_excel("./data/Ranking.xlsx", sheet = "Pools", col_names = FALSE))

# Tests - start
# DATA = importData("R/data/Ranking.xlsx")
# MATCHDATA = parsePoolFromExcel(read_excel("R/data/Ranking.xlsx", sheet = "Pools", col_names = FALSE))
# Test - end

# initial formatting of input data
MATCHDATALONG = lapply(MATCHDATA, function(x){
  df = clearPool(x)
  df = melt(df, id.vars = c("Self", "Date", "MaxHit"))
  df = df[df$value != "-",]
  return(df)
})
MATCHDATALONG = do.call(rbind, MATCHDATALONG)
MATCHDATALONG$value = as.numeric(MATCHDATALONG$value)

# extract global stats over all recorded pools
STAT_ALLFENCER = unique(MATCHDATALONG$Self)
STAT_MAXHITS = unique(MATCHDATALONG$MaxHit)
STAT_DATES = unique(MATCHDATALONG$Date)

### ============================================================================
### Define shiny ui
### ----------------------------------------------------------------------------
###

ui <- dashboardPage(

  dashboardHeader(title = "Fechtclub Marburg"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Rangliste", tabName = "leaderboard", icon = icon("dashboard")),
      menuItem("Punkteverteilung", tabName = "details", icon = icon("user")),
      menuItem("Gefechtsanalyse", tabName = "matches", icon = icon("microscope")),
      menuItem(div(tags$img(src = "/fmc_logo_black.jpg", width = "180px")))
    )
  ),

  dashboardBody(
    ### set backgorund color of valueBoxes
    tags$style(".small-box.bg-yellow { background-color: #323E47 !important; color: #000000 !important; }"),
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tabItems(
      #-------------
      # leaderboard tab
      #--------------
      tabItem(
        tabName = "leaderboard",
        h4("Rangliste"),
        br(),
        tabBox(
          width = 12,
          tabPanel(
            title = "",
            icon = icon("trophy"),
            box(width = 12,
                column(width = 12,
                       fluidRow(
                         infoBoxOutput("boxFirst", width = 12)
                       ),
                       fluidRow(
                         infoBoxOutput("boxSecond", width = 10)
                       ),
                       fluidRow(
                         infoBoxOutput("boxThird", width = 8)
                       ),
                       fluidRow(
                         infoBoxOutput("boxFour", width = 6)
                       ),
                       fluidRow(
                         infoBoxOutput("boxFive", width = 6)
                       )
                )
            )
          ),
          tabPanel(
            title = "",
            icon = icon("clipboard-list"),
            tableOutput("tableRes")
          ),
          tabPanel(
            title = "",
            icon = icon("table"),
            box(DT::dataTableOutput("table"),  width = 12)
          ),
          tabPanel(width = 12,
                   title = "",
                   icon = icon("info"),
                   h5("Erklärungen"),
                   br(),

                   icon("trophy"),
                   p("Zeigt die ersten 5 Plätze in der Rangliste.
                    Wenn du hier bist, arbeite weiter viel, um auch oben zu bleiben.
                    Wenn du noch nicht hier bist, dann gib extra Gas, um die anderen einzuholen."),
                   icon("clipboard-list"),
                   p("Zeigt die komplette Rangliste aller Teilnehmer*innen.
                     Der Rang bestimmt sich durch die nach den absolvierten Gefechten gewichteten gewonnenen Gefechten.
                     Beispiel 1: Max gewinnt 5 und verliert 5 Gefechte -> 5/10 Siege * 100% Gefechte gemacht = 5 * 1 = 5 Punkte.
                     Beispiel 2: Tom gewinnt 5 Gefechte und hat die anderen 5 nicht gefochten -> 5/10 Siege * 50% Gefechte gemacht = 5 * 0.5 = 2.5 Punkte.
                     Ergebnis: Max und Tom haben beide gleich viel gewonnen, aber weil Max mehr gefochten hat bekommt er mehr Punkte."),
                   icon("table"),
                   p("Hier siehst du eine Tabelle aller Pools mit Datum und den gewichteten Punkten."),
                   icon("calendar"),
                   p("Letztes Update: ", textOutput("lastDate"))


          )
        )
      ),
      #-------------
      # details tab
      #--------------
      tabItem(
        tabName = "details",
        h4("Punkteverteilung"),
        br(),
        tabBox(width = 12,
               tabPanel(width = 12,
                        title = "",
                        icon = icon("id-badge"),
                        checkboxGroupInput(inputId = "fencer",
                                           choices = unique(MATCHDATALONG$Self),
                                           selected = unique(MATCHDATALONG$Self),
                                           label = "Alle Fechter*innen" )
               ),
               tabPanel(width = 12,
                        title = "",
                        icon = icon("chart-line"),
                        box(plotOutput("performance", height = 600), width = 12)
               ),

               tabPanel(width = 12,
                        title = "",
                        icon = icon("chart-bar"),
                        box(plotOutput("points", height = 600), width = 12)
               ),
               tabPanel(width = 12,
                        title = "",
                        icon = icon("user"),
                        box(width = 12,
                            fluidRow(
                              column(width = 4,
                                     selectInput(inputId = "fencer01",
                                                 label = "Fechter*in 1",
                                                 choices = unique(MATCHDATALONG$Self)
                                     )
                              ),
                              column(width = 4,
                                     h1(icon("bolt"), align = "center")

                              ),
                              column(width = 4,
                                     selectInput(inputId = "fencer02",
                                                 label = "Fechter*in 2",
                                                 choices = unique(MATCHDATALONG$Self)
                                     )
                              )
                            ),
                            fluidRow(
                              box(plotOutput("headToHeadDetail"), width = 12)
                            )
                        )
               ),
               tabPanel(width = 12,
                        title = "",
                        icon = icon("info"),
                        h5("Erklärungen"),
                        br(),
                        icon("id-badge"),
                        p("Hier kannst du einen oder mehrere Fechter*innen auswählen um auf den nächsten Seiten ihre Leistungen in den Pools anzuzeigen."),
                        icon("chart-line"),
                        p("Die Grafik zeigt, wie sich deine Punkte über die Zeit summieren.
                          Du siehst, wie schnell deine Punkte steigen, je mehr Trainingseinheiten du besuchst und je mehr du fichst."),
                        icon("chart-bar"),
                        p("Die Grafik zeigt, wie viele Punkte du in jedem Training gesammelt hast.
                          Jeder Punkt steht für die Punkte, die du in einem einzigen Training bekommen hast.
                          Je enger die Punkte zusammenliegen, desto konstanter ist deine Leistung.
                          Die Box zeigt wie sich alle deine Punkte verteilen."),
                        icon("user"),
                        p("Die Grafik zeigt die Punkte, die du in jedem Training gegen einen bestimmten Gegner aufgeholt oder verloren hast.
                        Du siehst, ob du regelmäßig mehr oder weniger Punkte holst als deine Trainingspartner*innen.")
               )


        )
      ),
      #-------------
      # matches tab
      #--------------
      tabItem(
        tabName = "matches",
        h4("Gefechtsanalyse"),
        br(),
        tabBox(width = 12,
               tabPanel(
                 width = 12,
                 title = "",
                 icon = icon("id-badge"),
                 selectInput(inputId = "fencerDetail",
                             label = "Ausgewählte Fechter*in:",
                             choices = STAT_ALLFENCER),
                 selectInput(inputId = "maxHitDetail",
                             label = "Trefferzahl:",
                             choices = STAT_MAXHITS)
               ),
               tabPanel(
                 width = 12,
                 title = "",
                 icon = icon("table"),
                 box(plotOutput("poolOverview", height = 500), width = 12)
               ),
               tabPanel(
                 width = 12,
                 title = "",
                 icon = icon("chart-line"),
                 tabBox(width = 12,
                        tabPanel(width = 12,
                                 title = "",
                                 icon = icon("search-minus"),
                                 box(plotOutput("matchTimeFlow", height = 500), width = 12)),
                        tabPanel(width = 12,
                                 title = "",
                                 icon = icon("search-plus"),
                                 box(plotOutput("poolOverviewAll", height = 500), width = 12))
                        ),
               ),
               tabPanel(
                 width = 12,
                 title = "",
                 icon = icon("chart-bar"),
                 box(plotOutput("matchPerformance", height = 500), width = 12)
               ),
               tabPanel(width = 12,
                        title = "",
                        icon = icon("info"),
                        h5("Erklärungen"),
                        br(),
                        icon("id-badge"),
                        p("Hier kannst du einen Fechter auswählen um dir auf den folgenden Seiten die einzelnen Gefechte anzuschauen."),
                        icon("table"),
                        p("
                        Diese Grafik zeigt einen zusammengefassten Pool über alle Trainingseinheiten hinweg.
                        Je heller eine Feld, desto mehr Punkte hast du gemacht. Der ausgewählte Fechter ist schwarz umrandet. "),
                        icon("chart-line"),
                        p("
                        Die Grafik zeigt dir die Anzahl an gesetzten und erhaltenen Treffern pro Trainingseinheit.
                        Einmal von der gesamten Trainingseinheit(-) und einmal aufgelöst auf den jeweiligen Trainingsparter*in (+)."),
                        icon("chart-bar"),
                        p("Die Grafik zeigt dir die Anzahl an gesetzten und erhaltenen Treffern für jeden Trainingspartner*in.
                          Der Balken gibt den Durchschnitt über alle Trainingseinheiten an.
                          Der Punkt zeigt immer das Ergebnis der letzten Trainingseinheit.")
               )
        )
      )
    )
  )
)




### ============================================================================
### Define shiny server
### ----------------------------------------------------------------------------
###
server <- function(input, output) {

  output$performance <- renderPlot({
    # input = NULL
    # input$fencer = unique(MATCHDATALONG$Self)

    sel_data = MATCHDATALONG %>%
      filter(Self %in% input$fencer) %>%
      group_by(Self, Date) %>%
      summarize(n_matches = n(), n_points = sum(value)) %>%
      mutate(cum_n_matches = cumsum(n_matches), cum_n_points = cumsum(n_points))

    ggplot(sel_data, aes(x = Date, y = cum_n_matches, color = Self)) +
      geom_line(size = 1) +
      geom_point(size = 4, alpha = 0.7) +
      theme_bw() +
      scale_color_viridis_d() +
      dark_theme +
      labs(x = NULL, y = "Anzahl an Gefechten (kumuliert)", color = NULL) +
      theme(legend.position = "top") +
      ylim(0,max(sel_data$cum_n_matches)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      guides(color=guide_legend(nrow=3)) +
      theme(legend.text=element_text(size=14)) +
      theme(axis.text=element_text(size=14))
  })

  output$table <- DT::renderDataTable({
    sel_data = MATCHDATALONG

    n_matches = sel_data %>%
      group_by(Date) %>%
      summarise(max_matches = n_distinct(Self))

    df = sel_data %>%
      as_tibble() %>%
      mutate(win = ifelse(MaxHit == value, 1, 0)) %>%
      group_by(Self, Date) %>%
      summarize(n_wins = sum(win), n_points = sum(value), n_matches = n()) %>%
      left_join(n_matches, by = "Date") %>%
      mutate(match_ratio = n_matches / max_matches) %>%
      mutate(weighted_wins = round(n_wins * match_ratio, digits = 1)) %>%
      select(Self, Date, weighted_wins) %>%
      pivot_wider(names_from = Date, values_from = weighted_wins) %>%
      rename(Name = Self)

    colors <- c("#ff4d4d", "#ffb366", "#ffff99", "#b3ff99", "#33cc33")
    num_cols <- names(df)[-1]

    dt <- datatable(
      df,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )

    for (col in num_cols) {
      vals <- df[[col]]

      if (all(is.na(vals))) {
        dt <- dt %>%
          formatStyle(
            col,
            backgroundColor = styleEqual(NA, "lightgrey"),
            color = "black",
            fontWeight = "bold"
          )
        next
      }

      minv <- min(vals, na.rm = TRUE)
      maxv <- max(vals, na.rm = TRUE)

      if (identical(minv, maxv)) {
        delta <- ifelse(minv == 0, 1, abs(minv) * 0.1)
        cuts <- seq(minv - 2 * delta, minv + 2 * delta, length.out = length(colors) - 1)
      } else {
        cuts <- seq(minv, maxv, length.out = length(colors) - 1)
      }

      dt <- dt %>%
        formatStyle(
          col,
          backgroundColor = styleInterval(cuts, colors),
          color = "black",
          fontWeight = "bold"
        ) %>%
        formatStyle(
          col,
          backgroundColor = styleEqual(NA, "lightgrey"),
          color = "black",
          fontWeight = "bold"
        )
    }

    dt
    # DT::datatable(df, options = list(scrollX = TRUE))
  })

  output$tableRes <- renderTable({
    sel_data = MATCHDATALONG

    n_matches = sel_data %>%
      group_by(Date) %>%
      summarise(max_matches = n_distinct(Self))

    df = sel_data %>%
      as_tibble() %>%
      mutate(win = ifelse(MaxHit == value, 1, 0)) %>%
      group_by(Self, Date) %>%
      summarize(n_wins = sum(win), n_points = sum(value), n_matches = n()) %>%
      left_join(n_matches, by = "Date") %>%
      group_by(Self) %>%
      summarize(total_wins = sum(n_wins), total_points = sum(n_points), total_matches = sum(n_matches), total_max_matches = sum(max_matches)) %>%
      mutate(match_ratio = total_matches / total_max_matches) %>%
      mutate(weighted_wins = total_wins * match_ratio) %>%
      arrange(desc(weighted_wins)) %>%
      mutate(place = row_number()) %>%
      select(Platz = place, Name = Self, `Siege (gewichtet)` = weighted_wins, `Treffer (Anzahl)` = total_points )
      # select(Platz = place, Name = Self, `Siege (gewichtet)` = weighted_wins, `Siege (Anzahl)` = total_wins , `Treffer (Anzahl)` = total_points )
    df

  })

  output$points <- renderPlot({
    sel_data = MATCHDATALONG

    n_matches = sel_data %>%
      group_by(Date) %>%
      summarise(max_matches = n_distinct(Self))

    df = sel_data %>%
      as_tibble() %>%
      mutate(win = ifelse(MaxHit == value, 1, 0)) %>%
      group_by(Self, Date) %>%
      summarize(n_wins = sum(win), n_points = sum(value), n_matches = n()) %>%
      left_join(n_matches, by = "Date") %>%
      mutate(match_ratio = n_matches / max_matches) %>%
      mutate(weighted_wins = round(n_wins * match_ratio, digits = 1)) %>%
      select(Self, Date, weighted_wins) %>%
      arrange(desc(weighted_wins))

    ggplot(df, aes(x = Self, y = weighted_wins, color = Date)) +
      geom_boxplot(alpha = 0.3, color = "grey") +
      geom_beeswarm(size = 4) +
      coord_flip() +
      dark_theme +
      scale_y_continuous(breaks = c(0:ceiling(max(df$weighted_wins)))) +
      labs(
        title = "Gewichtete Siege pro Training und Sportler",
        x = "Siege (Gewichtet nach Anzahl Gefechten)",
        y = NULL
      ) +
      scale_color_viridis() +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=14))

  })

  output$boxFirst <- renderInfoBox({
    sel_data = MATCHDATALONG

    n_matches = sel_data %>%
      group_by(Date) %>%
      summarise(max_matches = n_distinct(Self))

    df = sel_data %>%
      as_tibble() %>%
      mutate(win = ifelse(MaxHit == value, 1, 0)) %>%
      group_by(Self, Date) %>%
      summarize(n_wins = sum(win), n_points = sum(value), n_matches = n()) %>%
      left_join(n_matches, by = "Date") %>%
      group_by(Self) %>%
      summarize(total_wins = sum(n_wins), total_points = sum(n_points), total_matches = sum(n_matches), total_max_matches = sum(max_matches)) %>%
      mutate(match_ratio = total_matches / total_max_matches) %>%
      mutate(weighted_wins = total_wins * match_ratio) %>%
      arrange(desc(weighted_wins)) %>%
      slice_head(n = 1)

        infoBox(
      title = tags$p("First", style = "font-size: 100%;"),
      value = tags$p(df$Self[1], style = "font-size: 150%;"),
      icon = icon("medal"), color = "yellow", fill = TRUE)
  })

  output$boxSecond <- renderInfoBox({
    sel_data = MATCHDATALONG

    n_matches = sel_data %>%
      group_by(Date) %>%
      summarise(max_matches = n_distinct(Self))

    df = sel_data %>%
      as_tibble() %>%
      mutate(win = ifelse(MaxHit == value, 1, 0)) %>%
      group_by(Self, Date) %>%
      summarize(n_wins = sum(win), n_points = sum(value), n_matches = n()) %>%
      left_join(n_matches, by = "Date") %>%
      group_by(Self) %>%
      summarize(total_wins = sum(n_wins), total_points = sum(n_points), total_matches = sum(n_matches), total_max_matches = sum(max_matches)) %>%
      mutate(match_ratio = total_matches / total_max_matches) %>%
      mutate(weighted_wins = total_wins * match_ratio) %>%
      arrange(desc(weighted_wins)) %>%
      slice_head(n = 2) %>%
      arrange(weighted_wins)

    infoBox(
      title = tags$p("Second", style = "font-size: 100%;"),
      value = tags$p(df$Self[1], style = "font-size: 150%;"),
      icon = icon("medal"), color = "yellow", fill = TRUE)
  })

  output$boxThird <- renderInfoBox({
    sel_data = MATCHDATALONG

    n_matches = sel_data %>%
      group_by(Date) %>%
      summarise(max_matches = n_distinct(Self))

    df = sel_data %>%
      as_tibble() %>%
      mutate(win = ifelse(MaxHit == value, 1, 0)) %>%
      group_by(Self, Date) %>%
      summarize(n_wins = sum(win), n_points = sum(value), n_matches = n()) %>%
      left_join(n_matches, by = "Date") %>%
      group_by(Self) %>%
      summarize(total_wins = sum(n_wins), total_points = sum(n_points), total_matches = sum(n_matches), total_max_matches = sum(max_matches)) %>%
      mutate(match_ratio = total_matches / total_max_matches) %>%
      mutate(weighted_wins = total_wins * match_ratio) %>%
      arrange(desc(weighted_wins)) %>%
      slice_head(n = 3) %>%
      arrange(weighted_wins)

    infoBox(
      title = tags$p("Third", style = "font-size: 100%;"),
      value = tags$p(df$Self[1], style = "font-size: 150%;"),
      icon = icon("medal"), color = "yellow", fill = TRUE)
  })

  output$boxFour <- renderInfoBox({
    sel_data = MATCHDATALONG

    n_matches = sel_data %>%
      group_by(Date) %>%
      summarise(max_matches = n_distinct(Self))

    df = sel_data %>%
      as_tibble() %>%
      mutate(win = ifelse(MaxHit == value, 1, 0)) %>%
      group_by(Self, Date) %>%
      summarize(n_wins = sum(win), n_points = sum(value), n_matches = n()) %>%
      left_join(n_matches, by = "Date") %>%
      group_by(Self) %>%
      summarize(total_wins = sum(n_wins), total_points = sum(n_points), total_matches = sum(n_matches), total_max_matches = sum(max_matches)) %>%
      mutate(match_ratio = total_matches / total_max_matches) %>%
      mutate(weighted_wins = total_wins * match_ratio) %>%
      arrange(desc(weighted_wins)) %>%
      slice_head(n = 4) %>%
      arrange(weighted_wins)

    infoBox(
      title = tags$p("4.", style = "font-size: 100%;"),
      value = tags$p(df$Self[1], style = "font-size: 100%;"),
      icon = icon("medal"), color = "navy", fill = TRUE)
  })

  output$boxFive <- renderInfoBox({
    sel_data = MATCHDATALONG

    n_matches = sel_data %>%
      group_by(Date) %>%
      summarise(max_matches = n_distinct(Self))

    df = sel_data %>%
      as_tibble() %>%
      mutate(win = ifelse(MaxHit == value, 1, 0)) %>%
      group_by(Self, Date) %>%
      summarize(n_wins = sum(win), n_points = sum(value), n_matches = n()) %>%
      left_join(n_matches, by = "Date") %>%
      group_by(Self) %>%
      summarize(total_wins = sum(n_wins), total_points = sum(n_points), total_matches = sum(n_matches), total_max_matches = sum(max_matches)) %>%
      mutate(match_ratio = total_matches / total_max_matches) %>%
      mutate(weighted_wins = total_wins * match_ratio) %>%
      arrange(desc(weighted_wins)) %>%
      slice_head(n = 5) %>%
      arrange(weighted_wins)

    infoBox(
      title = tags$p("5.", style = "font-size: 100%;"),
      value = tags$p(df$Self[1], style = "font-size: 100%;"),
      icon = icon("medal"), color = "navy", fill = TRUE)
  })

  output$lastDate <- renderText({
    sel_data = MATCHDATALONG %>%
      as_tibble() %>%
      select(Date) %>%
      distinct() %>%
      arrange(desc(Date)) %>%
      slice_head(n = 1)

    as.character(sel_data$Date)
  })

  output$headToHeadDetail <- renderPlot({
    # input = NULL
    # input$fencer01 = "Levin"
    # input$fencer02 = "Alexandra"
    #
    # input$fencer02 = "Levin"
    # input$fencer01 = "Alexandra"

    sel_data = MATCHDATALONG

    n_matches = sel_data %>%
      group_by(Date) %>%
      summarise(max_matches = n_distinct(Self))

    df = sel_data %>%
      as_tibble() %>%
      mutate(win = ifelse(MaxHit == value, 1, 0)) %>%
      group_by(Self, Date) %>%
      summarize(n_wins = sum(win), n_points = sum(value), n_matches = n()) %>%
      left_join(n_matches, by = "Date") %>%
      mutate(match_ratio = n_matches  / max_matches) %>%
      mutate(scaled_wins = n_wins * match_ratio) %>%
      select(Self, Date, scaled_wins) %>%
      filter(Self %in% c(input$fencer01, input$fencer02))

      df_plot = df %>%
      pivot_wider(names_from = Self, values_from = scaled_wins) %>%
      mutate(diff = .[[2]] - .[[3]]) %>%
      mutate(win = ifelse(diff > 0, paste0(as.character(input$fencer02), " hat mehr Punkte"),
                          paste0(as.character(input$fencer01), " hat mehr Punkte"))) %>%
      mutate(Date = as.factor(Date)) %>%
      select(Date, matches(input$fencer01), matches(input$fencer02), everything())


    ggplot(df_plot, aes(x = diff, y = Date, fill = win)) +
      geom_col() +
      scale_fill_brewer(palette = "Set3") +
      theme(legend.position = "top") +
      dark_theme +
      scale_x_continuous(
        limits = c(-max(df$scaled_wins), max(df$scaled_wins)),
        labels = function(x) abs(x)
      ) +
      labs(
        title = "Unterschied in gewichteten Punkten pro Training",
        x = "Unterschied in gewichteten Punkten",
        y = NULL,
        fill = NULL) +
      theme(axis.text=element_text(size=14))

  })


  # Outputs for the match details

  output$matchPerformance <- renderPlot({
    # input = NULL
    # input$fencerDetail = "Levin"
    # input$maxHitDetail = 5

    dfTotal = preparePoolSummary(MATCHDATALONG,
                                 fencer = as.character(input$fencerDetail),
                                 hits = as.numeric(input$maxHitDetail))

    # dfTotal = preparePoolSummary(MATCHDATALONG, fencer = "Sophia", hits = 10)

    dfAll = data.frame(name = dfTotal$variable,
                       opponent = dfTotal$Self,
                       received = dfTotal$received * -1,
                       scored = dfTotal$scored,
                       date = dfTotal$Date)
    dfAll$ord = (abs(dfAll$received) + abs(dfAll$scored))

    # prepare data for plotting - latest pool result
    dfAllLong = melt(dfAll, id.vars = c("name", "opponent", "date", "ord"))
    recentDf = dfAllLong[dfAllLong$date == dfAllLong$date[which.max(dfAllLong$date)],]

    # prepare data for plotting - summary pool result
    df = group_by(dfAll, name, opponent) %>% summarize(received = mean(received), scored = mean(scored))
    df$ord = (abs(df$received) + abs(df$scored))
    df = melt(df, id.vars = c("name", "opponent", "ord"))

    ggplot(df, aes(x = reorder(opponent, ord), y = value, fill = variable)) +
      geom_hline(yintercept = -unique(dfTotal$MaxHit)) +
      geom_hline(yintercept = unique(dfTotal$MaxHit)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(breaks = seq(-max(abs(min(df$value)),max(df$value)),max(abs(min(df$value)),max(df$value)))) +
      dark_theme +
      scale_fill_brewer(palette = "Set3") +
      scale_x_reordered() +
      geom_point(data = recentDf, aes(x = opponent, y = value, fill = variable), shape = 21, size = 4, stroke = 2) +
      labs(title = "Gesetzte und erhaltene Treffer pro Trainingspartner*in",
           subtitle = paste0("Fechter*in: ", input$fencerDetail),
           x = NULL,
           y = "Treffer",
           caption = "Balken = Durchschnitt über alle Trainingseinheiten \n Punkt = Ergebnis der letzte Trainingseinheit") +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=14))


  })

  output$matchTimeFlow <- renderPlot({
    # input = NULL
    # input$fencerDetail = "Levin"
    # input$maxHitDetail = 5

    dfTotal = preparePoolSummary(MATCHDATALONG,
                                 fencer = as.character(input$fencerDetail),
                                 hits = as.numeric(input$maxHitDetail))

    df = data.frame(name = dfTotal$variable,
                    opponent = dfTotal$Self,
                    received = dfTotal$received * -1,
                    scored = dfTotal$scored,
                    date = dfTotal$Date)
    df = group_by(df, date) %>% summarise(received = mean(received), scored = mean(scored))

    df$ord = (abs(df$received) + abs(df$scored))
    df = melt(df, id.vars = c("date", "ord"))

    ggplot(df, aes(x = date, y = value, color = variable)) +
      geom_hline(yintercept = 0, color = "lightgrey") +
      geom_hline(yintercept = -unique(dfTotal$MaxHit)) +
      geom_hline(yintercept = unique(dfTotal$MaxHit)) +
      geom_point(size = 4) +
      geom_line(lwd = 1) +
      scale_y_continuous(breaks = seq((-unique(dfTotal$MaxHit)),(unique(dfTotal$MaxHit))),
                         limits = c(-unique(dfTotal$MaxHit), unique(dfTotal$MaxHit))) +
      dark_theme +
      scale_color_brewer(palette = "Set3") +
      labs(title = paste0("Gesetzte und erhaltene Treffer pro Trainingseinheit"),
           subtitle = paste0("Fechter*in: ", input$fencerDetail),
           x = NULL, y = "(erhalten) Treffer (gesetzt)") +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=14))
  })

  output$poolOverview <- renderPlot({
    # input = NULL
    # input$fencerDetail = "Levin"
    # input$maxHitDetail = 5

    m = MATCHDATALONG %>%
      filter(MaxHit == as.numeric(input$maxHitDetail)) %>%
      # filter(MaxHit == 5) %>%
      group_by(Self, variable) %>%
      summarize(m = mean(value)) %>%
      spread(Self, m)

    m = as.data.frame(m)
    rownames(m) = m$variable
    m$variable = NULL

    m = m[,order(colSums(m, na.rm = T), decreasing = F)]
    m  = t(m)
    m = m[,order(colSums(m, na.rm = T), decreasing = F)]
    idx = match(rownames(m), colnames(m))
    m = m[,idx]

    m = m[,c(ncol(m):1)]

    df = melt(m)
    colnames(df) = c("v1", "v2", "Touches")

    ggplot(df, aes(x = v2, y = v1, fill = Touches)) +
      geom_tile(colour = "grey", lwd = 0.3, alpha = 0.9) +
      scale_fill_viridis(option = "B") +
      dark_theme +
      xlab("") + ylab("") +
      scale_x_discrete(position = "top") +
      geom_tile(data = df[df$v1 == as.character(input$fencerDetail),], colour = "black", lwd = 1) +
      labs(title = paste0("Meta Pool mit durchschnittlichen Treffern"),
           subtitle = paste0("Fechter*in: ", input$fencerDetail),
           x = NULL, y = NULL, fill = "Treffer") +
      theme(legend.position = "top") +
      theme(axis.text.x=element_text(angle=90, hjust=1)) +
      theme(axis.text=element_text(size=14)) +
      # coord_fixed() +
      guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5))
  })

  output$poolOverviewAll <- renderPlot({
    # input = NULL
    # input$fencerDetail = "Levin"
    # input$maxHitDetail = 5

    dfTotal = preparePoolSummary(MATCHDATALONG,
                                 fencer = as.character(input$fencerDetail),
                                 hits = as.numeric(input$maxHitDetail))
    # dfTotal = preparePoolSummary(MATCHDATALONG, fencer = "Sophia", hits = 10)

    df = data.frame(name = dfTotal$variable,
                    opponent = dfTotal$Self,
                    received = dfTotal$received * -1,
                    scored = dfTotal$scored,
                    date = dfTotal$Date)


      df = melt(df, id.vars = c("date", "opponent", "name"))

      ggplot(df, aes(x = date, y = value, color = variable)) +
        geom_hline(yintercept = 0, color = "lightgrey") +
        geom_hline(yintercept = -unique(dfTotal$MaxHit)) +
        geom_hline(yintercept = unique(dfTotal$MaxHit)) +
        geom_point(size = 2) +
        geom_line(lwd = 0.5) +
        scale_y_continuous(breaks = seq((-unique(dfTotal$MaxHit)),(unique(dfTotal$MaxHit))),
                           limits = c(-unique(dfTotal$MaxHit), unique(dfTotal$MaxHit))) +
        dark_theme +
        scale_color_brewer(palette = "Set3") +
        labs(title = paste0("Gesetzte und erhaltene Treffer pro Trainingseinheit"),
             subtitle = paste0("Fechter*in: ", input$fencerDetail),
             x = NULL, y = "(erhalten) Treffer (gesetzt)") +
        facet_wrap(~opponent) +
        theme(legend.position = "none") +
        theme(axis.text.x=element_text(angle=90, hjust=1))

  })

}

shinyApp(ui, server)

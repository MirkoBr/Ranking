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
# library(gganimate)
library(ggbeeswarm)
library(tidytext)
library(tidyverse)
library(readxl)
source("functions.R")

### ============================================================================
### Initialize data globally
### ----------------------------------------------------------------------------
###

DATA = importData("./data/Ranking.xlsx")
MATCHDATA = parsePoolFromExcel(read_excel("./data/Ranking.xlsx", sheet = "Pools", col_names = FALSE))

# DATA = importData("/Users/mirko/Projects/Private/Ranking/R/data/Ranking.xlsx")
# MATCHDATA = parsePoolFromExcel(read_excel("/Users/mirko/Projects/Private/Ranking/R/data/Ranking.xlsx", sheet = "Pools", col_names = FALSE))

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

  dashboardHeader(title = "TFC Ranking"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Leaderboard", tabName = "leaderboard", icon = icon("dashboard")),
      menuItem("Details", tabName = "details", icon = icon("microscope")),
      menuItem("Matches", tabName = "matches", icon = icon("user"))
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
        h4("Current leaderboard"),
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
                   h5("Tab infos"),
                   br(),

                   icon("trophy"),
                   p("Displays the first 5 positions in the ranking.
                     If you're up here make sure to work hard to stay here.
                     If you're not up here, work harder and hunt them down."),
                   icon("clipboard-list"),
                   p("Displays the full ranking. Draws are handled randomly."),
                   icon("table"),
                   p("Displays a searchable table of all pools up the indicated date."),
                   icon("calendar"),
                   p("Last pool included: ", textOutput("lastDate"))


          )
        )
      ),
      #-------------
      # details tab
      #--------------
      tabItem(
        tabName = "details",
        h4("Detailed progress"),
        br(),
        tabBox(width = 12,
               tabPanel(width = 12,
                        title = "",
                        icon = icon("id-badge"),
                        checkboxGroupInput(inputId = "fencer",
                                           choices = unique(colnames(DATA[-c(1)])),
                                           selected = unique(colnames(DATA[-c(1)])),
                                           label = "Select fencers" )
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
                                                 label = "Fencer 1",
                                                 choices = unique(colnames(DATA[-c(1)]))
                                     )
                              ),
                              column(width = 4,
                                     h1(icon("bolt"), align = "center")

                              ),
                              column(width = 4,
                                     selectInput(inputId = "fencer02",
                                                 label = "Fencer 2",
                                                 choices = unique(colnames(DATA[-c(1)]))
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
                        h5("Tabs info"),
                        br(),
                        icon("id-badge"),
                        p("Select one to many fencers to show detailed pool performance."),
                        icon("chart-line"),
                        p("Displays the cumulative pool points over time.
                        You can see how fast your points increases with every pool you attend.
                          The more pools you join, the faster you climb."),
                        icon("chart-bar"),
                        p("Displays the number of points you gained per pool.
                        Each dot represents the number of points you scored at the given training.
                          The closer the points are together to more consitent is your performance."),
                        icon("user"),
                        p("Displays the number of points gained or lost per training betweent two fencers.
                          You can see if you're consitently outscoring someone, or if you just landed that one luck punch.")
               )


        )
      ),
      #-------------
      # matches tab
      #--------------
      tabItem(
        tabName = "matches",
        h4("Individual match results"),
        br(),
        tabBox(width = 12,
               tabPanel(
                 width = 12,
                 title = "",
                 icon = icon("id-badge"),
                 selectInput(inputId = "fencerDetail",
                             label = "Selected fencer:",
                             choices = STAT_ALLFENCER),
                 selectInput(inputId = "maxHitDetail",
                             label = "Number of Toches:",
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
                        h5("Tabs info"),
                        br(),
                        icon("id-badge"),
                        p("Select the fencer you want to see the details for and the number of touches fenced per pool."),
                        icon("table"),
                        p("Displays a summarized overview pool chart.
                        Touches are summairzed by the mean for each pool.
                        The color scale indicates the number touches scored.
                        The lighter the color the more touches you scored. "),
                        icon("chart-line"),
                        p("Displays the number of touches scored and received per pool.
                        The first plot show a summerized version over all opponents relative to the selected fencer.
                        The second plot shows the same curve, but seperatly for each opponent."),
                        icon("chart-bar"),
                        p("Displays the performance of the selected fencer for each opponent, summarized over all pools.
                        The bars indicated the mean number of touches scored and received for each opponent.
                        Circled points indicate the number of touches scored and received of the most recent pool.")
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
    selData = select(DATA, c("Date", input$fencer))
    df = melt(selData, id.vars = "Date")
    df = df %>% group_by(variable) %>% mutate(cumsum = cumsum(value))
    p = ggplot(df, aes(x = Date, y = cumsum, color = variable)) +
      geom_line(size = 1) +
      geom_point(size = 4, alpha = 0.7) +
      theme_bw() +
      scale_color_manual(values = colScaled_Dark2(13)) +
      dark_theme +
      labs(x = NULL, y = "Points", color = "Name") +
      theme(legend.position = "top") +
      ylim(0,max(df$cumsum)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    p
  })

  output$table <- DT::renderDataTable({
    d = t(DATA[-c(1)]) %>% as.data.frame()
    DT::datatable(d, options = list(scrollX = TRUE))
  })

  output$tableRes <- renderTable({
    d = t(DATA[-c(1)]) %>% as.data.frame()
    df = data.frame(Place = paste0(1:nrow(d), "."),
                    Name = rownames(d)[order(rowSums(d), decreasing = T)],
                    Points = rowSums(d[order(rowSums(d), decreasing = T),]))
    df
  })

  output$points <- renderPlot({
    selData = select(DATA, c("Date", input$fencer))
    df = melt(selData, id.vars = "Date")
    df$Date = as.factor(df$Date)

    ggplot(df, aes(x = variable, y = value, group = variable, color = Date)) +
      geom_boxplot(alpha = 0.3, color = "grey") +
      geom_beeswarm(size = 4) +
      coord_flip() +
      dark_theme +
      scale_y_continuous(breaks = c(0:max(DATA[-c(1)]))) +
      # theme(axis.text.y = element_text(size=24),
      #       axis.text.x = element_text(size=16)) +
      xlab(NULL) + ylab("Points") +
      scale_color_brewer(palette = "Set3") +
      theme(legend.position = "none")
  })

  output$boxFirst <- renderInfoBox({
    d = DATA[-c(1)]
    first = which.max(colSums(d))
    name = as.character(names(first))
    infoBox(
      title = tags$p("First", style = "font-size: 100%;"),
      value = tags$p(name, style = "font-size: 150%;"),
      icon = icon("medal"), color = "yellow", fill = TRUE)
  })

  output$boxSecond <- renderInfoBox({
    d = DATA[-c(1)]
    first = which.max(colSums(d))
    dd = d[-c(first)]
    second = which.max(colSums(dd))
    name = as.character(names(second))

    infoBox(
      title = tags$p("Second", style = "font-size: 100%;"),
      value = tags$p(name, style = "font-size: 150%;"),
      icon = icon("medal"), color = "yellow", fill = TRUE)
  })

  output$boxThird <- renderInfoBox({
    d = DATA[-c(1)]
    first = which.max(colSums(d))
    dd = d[-c(first)]
    second = which.max(colSums(dd))
    ddd = dd[-c(second)]
    thrid = which.max(colSums(ddd))
    name = as.character(names(thrid))

    infoBox(
      title = tags$p("Third", style = "font-size: 100%;"),
      value = tags$p(name, style = "font-size: 150%;"),
      icon = icon("medal"), color = "yellow", fill = TRUE)
  })

  output$boxFour <- renderInfoBox({
    d = DATA[-c(1)]
    first = which.max(colSums(d))
    dd = d[-c(first)]
    second = which.max(colSums(dd))
    ddd = dd[-c(second)]
    thrid = which.max(colSums(ddd))
    dddd = ddd[-c(thrid)]
    four = which.max(colSums(dddd))
    name = as.character(names(four))

    infoBox(
      title = tags$p("4.", style = "font-size: 100%;"),
      value = tags$p(name, style = "font-size: 100%;"),
      icon = icon("medal"), color = "navy", fill = TRUE)
  })

  output$boxFive <- renderInfoBox({
    d = DATA[-c(1)]
    first = which.max(colSums(d))
    dd = d[-c(first)]
    second = which.max(colSums(dd))
    ddd = dd[-c(second)]
    thrid = which.max(colSums(ddd))
    dddd = ddd[-c(thrid)]
    four = which.max(colSums(dddd))
    ddddd = dddd[-c(four)]
    five = which.max(colSums(ddddd))
    name = as.character(names(five))

    infoBox(
      title = tags$p("5.", style = "font-size: 100%;"),
      value = tags$p(name, style = "font-size: 100%;"),
      icon = icon("medal"), color = "navy", fill = TRUE)
  })

  output$lastDate <- renderText({
    as.character(max(DATA$Date))
  })

  output$headToHeadDetail <- renderPlot({
    selData = select(DATA, c("Date", input$fencer01, input$fencer02))
    overhang = (select(selData, input$fencer01) - select(selData, input$fencer02))
    selData$overhang = overhang[,1]

    # selData = select(DATA, c("Date", "Malina", "Thea"))
    # overhang = (selData$Luna - selData$Pia)
    # selData$overhang = overhang
    # df = selData
    # df$win = ifelse(df$overhang < 0, "Pia win", "Luna win")

    df = selData
    df$win = ifelse(df$overhang < 0,
                    paste0(as.character(input$fencer02), " win"),
                    paste0(as.character(input$fencer01), " win"))
    df$Date = as.factor(df$Date)

    ggplot(df, aes(x = overhang, y = Date, fill = win)) +
      geom_col() +
      scale_fill_brewer(palette = "Set3") +
      theme(legend.position = "top") +
      dark_theme +
      xlab("Point difference") +
      ylab(NULL) +
      xlim(-max(DATA[-c(1)]), max(DATA[-c(1)]))
  })


  # Outputs for the match details

  output$matchPerformance <- renderPlot({

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
      xlab(NULL) +
      ylab("Touches") +
      ggtitle(paste0(input$fencerDetail, "; ", input$maxHitDetail, " touches")) +
      theme(legend.position = "none")

  })

  output$matchTimeFlow <- renderPlot({
    dfTotal = preparePoolSummary(MATCHDATALONG,
                                 fencer = as.character(input$fencerDetail),
                                 hits = as.numeric(input$maxHitDetail))
    # dfTotal = preparePoolSummary(MATCHDATALONG, fencer = "Sophia", hits = 10)

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
      ylab("Touches") +
      ggtitle(paste0(input$fencerDetail, "; ", input$maxHitDetail, " touches")) +
      theme(legend.position = "none")
  })

  output$poolOverview <- renderPlot({
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
      ggtitle(paste0(input$fencerDetail, "; ", input$maxHitDetail, " touches")) +
      theme(legend.position = "top") +
      theme(axis.text.x=element_text(angle=90, hjust=1))
  })

  output$poolOverviewAll <- renderPlot({
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
        ylab("Touches") +
        facet_wrap(~opponent) +
        ggtitle(paste0(input$fencerDetail, "; ", input$maxHitDetail, " touches")) +
        theme(legend.position = "none") +
        theme(axis.text.x=element_text(angle=90, hjust=1))

  })

}

shinyApp(ui, server)

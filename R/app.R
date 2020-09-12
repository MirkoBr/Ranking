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
library(gganimate)
library(ggbeeswarm)
source("functions.R")

### ============================================================================
### Initialize data globally
### ----------------------------------------------------------------------------
###

DATA = importData("./data/Ranking.xlsx")
# DATA = importData("/Users/mirko/Projects/Private/Ranking/R/data/Ranking.xlsx")
# x = "/Users/mirko/Projects/Private/Ranking/R/data/Ranking.xlsx"
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
      menuItem("Head-to-Head", tabName = "head", icon = icon("adjust"))
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
                   h5("Context info"),
                   br(),
                   p("The first 5 positions in the ranking are awarded and extra space.
                     If you're up here make sure to work hard to stay.
                     If you're not up here, work hard and hunt them down."),
                   p("The second tab shows a full list of the current state.
                     Draws are handled randomly."),
                   p("The last tab is a searchable table of all pools up the indicated date."),
                   br(),
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
                        icon = icon("info"),
                        h5("Context info"),
                        br(),
                        p("Select one to many fencers to show detailed pool performance."),
                        p("The first graphs shows the cumlative pool points over time.
                        You can see how fast your points increases with every pool you attend.
                          The more pools you join, the faster you climb."),
                        p("    In the second graph shows the individual points you gained per pool.
                        Each dot represents the number of points you scored at the given training.
                          The closer the points are together to more consitent is your performance.")
               )


        )
      ),
      #-------------
      # head tab
      #--------------
      tabItem(
        tabName = "head",
        h4("Head - to - Head"),
        br(),
        p("Choose two fencers and let them battle an epic battle."),
        tabBox(width = 12,
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
      geom_point(size = 5, alpha = 0.7) +
      theme_bw() +
      scale_color_manual(values = colScaled_Dark2(11)) +
      dark_theme +
      labs(x = "Date", y = "Points", color = "Name") +
      theme(legend.position = "top") +
      ylim(0,max(df$cumsum))
    p
  })

  output$table <- DT::renderDataTable({
    d = t(DATA[-c(1)]) %>% as.data.frame()
    DT::datatable(d)
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
      geom_beeswarm(size = 5) +
      coord_flip() +
      dark_theme +
      scale_y_continuous(breaks = c(0:max(DATA[-c(1)]))) +
      theme(axis.text.y = element_text(size=24),
            axis.text.x = element_text(size=16)) +
      xlab(NULL) + ylab("Points") +
      scale_color_brewer(palette = "Set3") +
      theme(legend.position = "top")
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
    # f1 = "Luna"
    # f2 = "Pia"
    selData = select(DATA, c("Date", input$fencer01, input$fencer02))
    # selData = select(DATA, c("Date", f1, f2))

    overhang = (select(selData, input$fencer01) - select(selData, input$fencer02))
    selData$overhang = overhang[,1]

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
      xlab("Domination score") +
      ylab(NULL)

  })

}

shinyApp(ui, server)




# output$performanceAnim <- renderImage({
#   df = melt(DATA, id.vars = "Date")
#   df = df %>% group_by(variable) %>% mutate(cumsum = cumsum(value))
#   p = ggplot(df, aes(x = Date, y = cumsum, color = variable)) +
#     geom_line(size = 1) +
#     geom_point(size = 5, alpha = 0.7) +
#     theme_bw() +
#     scale_color_manual(values = colScaled_Dark2(10)) +
#     dark_theme +
#     labs(x = "Date", y = "Score", color = "Name")
#   p = p + transition_reveal(Date)
#   anim_save("perform.gif", animate(p, nframes = 10, device = "png", fps = 1))
#
#   list(src = "perform.gif",
#        contentType = 'image/gif')
#
# })

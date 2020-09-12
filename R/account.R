### ============================================================================
###
###                       Basic Account Module
###
### ----------------------------------------------------------------------------
### 
library(shiny)

makeAccountPageUI <- function(id, name){
  ns = NS(id)
  tabItem(
    tabName = name,
    box(plotlyOutput(ns("plotSelectedMonthFlow"), height = 250), width = 12),
    fluidRow(
      column(
        width = 4,
        tabBox(
          width = 12,
          title = ("Six Month"),
          id = "last12month",
          tabPanel("Spents", plotOutput(ns("plotSelectedMonthCosts"), height = 400)),
          tabPanel("Groceries", plotlyOutput(ns("plotSelectedMonthGroceries"), height = 400)),
          tabPanel("Gas", plotlyOutput(ns("plotSelectedMonthGas"), height = 400)),
          column(width = 2, h3(icon("coins"))),
          column(width = 2, h5(textOutput(ns("selectedMonthIncome")))),
          column(width = 2, h5(textOutput(ns("selectedMonthSpent")))),
          column(width = 2, h5(textOutput(ns("selectedMonthBalance"))))
        )
      ),
      column(
        width = 4,
        tabBox(
          width = 12,
          title = ("Last Month"),
          id = "lastMonth",
          tabPanel("Spents", plotOutput(ns("plotLastMonthCosts"), height = 400)),
          tabPanel("Groceries", plotlyOutput(ns("plotLastMonthGroceries"), height = 400)),
          tabPanel("Gas", plotlyOutput(ns("plotLastMonthGas"), height = 400)),
          column(width = 2, h3(icon("coins"))),
          column(width = 2, h5(textOutput(ns("lastMonthIncome")))),
          column(width = 2, h5(textOutput(ns("lastMonthSpent")))),
          column(width = 2, h5(textOutput(ns("lastMonthBalance"))))
        )
      ),
      column(
        width = 4,
        tabBox(
          width = 12,
          title = ("This Month"),
          id = "thisMonth",
          tabPanel("Spents", plotOutput(ns("plotThisMonthCosts"), height = 400)),
          tabPanel("Groceries", plotlyOutput(ns("plotThisMonthGroceries"), height = 400)),
          tabPanel("Gas", plotlyOutput(ns("plotThisMonthGas"), height = 400)),
          column(width = 2, h3(icon("coins"))),
          column(width = 2, h5(textOutput(ns("thisMonthIncome")))),
          column(width = 2, h5(textOutput(ns("thisMonthSpent")))),
          column(width = 2, h5(textOutput(ns("thisMonthBalance"))))
          
        )
      )
    )
  )
}

makeAccountPageSV <- function(input, output, session, DATA, daterange,
                              upperThreshold, lowerThreshold, 
                              topXThreshold, discounterList, stationsList){
  
  ### Info numbers
  ### --------------------------------------------------------------------------
  output$thisMonthIncome <- renderText({ 
    df = data.frame(date = DATA$sendDay[DATA$amount >0], val = DATA$amount[DATA$amount >0])
    df = df[month(df$date) == month(Sys.time()) & year(df$date) == year(Sys.time()),]
    paste0("Income: ", format(sum(df$val), digits = 2, big.mark = ".", small.mark = ","))
  })
  output$thisMonthSpent <- renderText({ 
    df = data.frame(date = DATA$sendDay[DATA$amount <= 0], val = DATA$amount[DATA$amount <= 0])
    df = df[month(df$date) == month(Sys.time()) & year(df$date) == year(Sys.time()),]
    paste0("Spent: ", format(sum(df$val)*-1, digits = 2, big.mark = ".", small.mark = ","))
  })
  output$thisMonthBalance <- renderText({
    df = data.frame(date = DATA$sendDay[DATA$amount >0], val = DATA$amount[DATA$amount >0])
    df = df[month(df$date) == month(Sys.time()) & year(df$date) == year(Sys.time()),]
    income = df
    df = data.frame(date = DATA$sendDay[DATA$amount <= 0], val = DATA$amount[DATA$amount <= 0])
    df = df[month(df$date) == month(Sys.time()) & year(df$date) == year(Sys.time()),]
    spent = df
    balance = sum(income$val) + sum(spent$val)
    paste0("Balance: ", format(balance, digits = 2, big.mark = ".", small.mark = ","))
  })
  
  output$lastMonthIncome <- renderText({ 
    df = data.frame(date = DATA$sendDay[DATA$amount >0], val = DATA$amount[DATA$amount >0])
    if(month(Sys.time())-1 == 0){
      df = df[month(df$date) == 12 & year(df$date) == year(Sys.time())-1,]
    } else {
      df = df[month(df$date) == month(Sys.time())-1 & year(df$date) == year(Sys.time()),]
    }
    paste0("Income: ", format(sum(df$val), digits = 2, big.mark = ".", small.mark = ","))
  })
  output$lastMonthSpent <- renderText({ 
    df = data.frame(date = DATA$sendDay[DATA$amount <= 0], val = DATA$amount[DATA$amount <= 0])
    if(month(Sys.time())-1 == 0){
      df = df[month(df$date) == 12 & year(df$date) == year(Sys.time())-1,]
    } else {
      df = df[month(df$date) == month(Sys.time())-1 & year(df$date) == year(Sys.time()),]
    }
    paste0("Spent: ", format(sum(df$val)*-1, digits = 2, big.mark = ".", small.mark = ","))
  })
  output$lastMonthBalance <- renderText({
    df = data.frame(date = DATA$sendDay[DATA$amount >0], val = DATA$amount[DATA$amount >0])
    if(month(Sys.time())-1 == 0){
      df = df[month(df$date) == 12 & year(df$date) == year(Sys.time())-1,]
    } else {
      df = df[month(df$date) == month(Sys.time())-1 & year(df$date) == year(Sys.time()),]
    }
    income = df
    df = data.frame(date = DATA$sendDay[DATA$amount <= 0], val = DATA$amount[DATA$amount <= 0])
    if(month(Sys.time())-1 == 0){
      df = df[month(df$date) == 12 & year(df$date) == year(Sys.time())-1,]
    } else {
      df = df[month(df$date) == month(Sys.time())-1 & year(df$date) == year(Sys.time()),]
    }
    spent = df
    balance = sum(income$val) + sum(spent$val)
    paste0("Balance: ", format(balance, digits = 2, big.mark = ".", small.mark = ","))
  })
  
  output$selectedMonthIncome <- renderText({ 
    df = data.frame(date = DATA$sendDay[DATA$amount >0], val = DATA$amount[DATA$amount >0])
    df = subset(df, df$date >= daterange()[1] & df$date <= daterange()[2])
    paste0("Income: ", format(sum(df$val), digits = 2, big.mark = ".", small.mark = ","))
  })
  output$selectedMonthSpent <- renderText({ 
    df = data.frame(date = DATA$sendDay[DATA$amount <= 0], val = DATA$amount[DATA$amount <= 0])
    df = subset(df, df$date >= daterange()[1] & df$date <= daterange()[2])
    paste0("Spent: ", format(sum(df$val)*-1, digits = 2, big.mark = ".", small.mark = ","))
  })
  output$selectedMonthBalance <- renderText({
    df = data.frame(date = DATA$sendDay[DATA$amount >0], val = DATA$amount[DATA$amount >0])
    df = subset(df, df$date >= daterange()[1] & df$date <= daterange()[2])
    income = df
    df = data.frame(date = DATA$sendDay[DATA$amount <= 0], val = DATA$amount[DATA$amount <= 0])
    df = subset(df, df$date >= daterange()[1] & df$date <= daterange()[2])
    spent = df
    balance = sum(income$val) + sum(spent$val)
    paste0("Balance: ", format(balance, digits = 2, big.mark = ".", small.mark = ","))
  })
  
  ### Flow plots
  ### --------------------------------------------------------------------------
  output$plotSelectedMonthFlow <- renderPlotly({
    ### combine transactions from the same day
    df = data.frame(date = DATA$sendDay,
                    val = DATA$amount,
                    comp = DATA$comp)
    df = group_by(df, date, comp) %>% summarize(val = sum(val))
    df = data.frame(date = (df$date),
                    val = cumsum((df$val)),
                    trans = df$val,
                    comp = df$comp)
    ### select time frame from input
    df$date = as.POSIXct(df$date) + sort(sample(10000:100000, length(df$date), replace = F), decreasing = F)
    df = subset(df, df$date >= daterange()[1] & df$date <= daterange()[2])
    ### make plot
    plot_ly(df, x = ~date) %>%
      add_trace(y = ~val, name = 'balance', type = "scatter", mode = 'lines+markers',
                fill = "tozeroy", fillcolor = "2E757A", line = list(color = "cyan"),
                marker = list(color = "cyan"),
                hovertext = df$comp,
                text = df$trans,
                hovertemplate = paste(
                  "<b>%{hovertext}</b><br>",
                  "Transaction: %{text}<br>",
                  "%{yaxis.title.text}: %{y:€,.0f}",
                  "<extra></extra>"
                )
      ) %>% 
      add_trace(data = data.frame(val = mean(df$val), date = df$date), x = ~date, y = ~val, type = "scatter", mode = "lines",
                line = list(color = "cyan", dash = 'dot')) %>%
      add_trace(data = tail(subset(df, date == max(date)),1), x = ~date, y = ~val, type = "scatter", mode = "markers",
                marker = list(color = "cyan", size = 30, opacity = .2)) %>% 
      add_trace(data = tail(subset(df, date == max(date)),1), x = ~date, y = ~val, type = "scatter", mode = "markers",
                marker = list(color = "cyan", size = 20, opacity = .2)) %>% 
      add_trace(data = tail(subset(df, date == max(date)),1), x = ~date, y = ~val, type = "scatter", mode = "markers",
                marker = list(color = "cyan", size = 15, opacity = .2)) %>% 
      add_trace(data = tail(subset(df, date == max(date)),1), x = ~date, y = ~val, type = "scatter", mode = "markers",
                marker = list(color = "cyan", size = 10, opacity = .2)) %>%
      layout(paper_bgcolor='#323E47',plot_bgcolor='#323E47', font = list(color = "lightgrey"),
             yaxis = list(title = "Balance", gridcolor = "#4d5566"),
             xaxis = list(title = NA),
             showlegend = FALSE)
    
  })
  
  ### Cost plots
  ### --------------------------------------------------------------------------
  output$plotSelectedMonthCosts <- renderPlot({
    ### select relevant data and summarize by company
    df = data.frame(date = DATA$sendDay,
                    amount = DATA$amount,
                    comp = DATA$comp)
    df = subset(df, df$date >= daterange()[1] & df$date <= daterange()[2])
    df = subset(df, amount < 0)
    df$amount = df$amount * -1
    df = group_by(df, comp) %>% summarize(amount = sum(amount))
    
    ### apply upper limit
    df = subset(df, amount < upperThreshold()) 
    
    ### group by lower limit
    dfClear = df[df$amount > lowerThreshold(),]
    dfRest = df[df$amount <= lowerThreshold(),]
    restValue = data.frame(comp = "OTHERS", amount = sum(dfRest$amount))
    dfClear = rbind.data.frame(dfClear, restValue)
    
    ### reorder 
    dfClear = dfClear[order(dfClear$amount, decreasing = T),]
    dfClear = dfClear[c(1:topXThreshold()),] ### top xx threshold
    dfClear = dfClear[order(dfClear$amount, decreasing = F),]
    dfClear$comp = factor(dfClear$comp, levels = dfClear$comp)
    
    ggplot(dfClear, aes(x = comp, y = amount )) +
      geom_bar(stat = "identity", fill = "cyan") +
      geom_text(aes(y = 0, label = paste0(format(dfClear$amount, digits = 2, big.mark = "\\."), ",-")), hjust = -.1, color = "black") + 
      coord_flip() + 
      xlab(NULL) + 
      ylab(NULL) +
      dark_theme
  })
  output$plotThisMonthCosts <- renderPlot({
    ### select relevant data and summarize by company
    df = data.frame(date = DATA$sendDay,
                    amount = DATA$amount,
                    comp = DATA$comp)
    df = df[month(df$date) == month(Sys.time()) & year(df$date) == year(Sys.time()),]
    df = subset(df, amount < 0)
    df$amount = df$amount * -1
    df = group_by(df, comp) %>% summarize(amount = sum(amount))
    
    ### reorder 
    dfClear = df
    dfClear = dfClear[order(dfClear$amount, decreasing = F),]
    dfClear$comp = factor(dfClear$comp, levels = dfClear$comp)
    
    ggplot(dfClear, aes(x = comp, y = amount )) +
      geom_bar(stat = "identity", fill = "cyan") +
      geom_text(aes(y = 0, label = paste0(format(dfClear$amount, digits = 2, big.mark = "\\."), ",-")), hjust = -.1, color = "black") + 
      coord_flip() + 
      xlab(NULL) + 
      ylab(NULL) +
      dark_theme
  })
  output$plotLastMonthCosts <- renderPlot({
    ### select relevant data and summarize by company
    df = data.frame(date = DATA$sendDay,
                    amount = DATA$amount,
                    comp = DATA$comp)
    if(month(Sys.time())-1 == 0){
      df = df[month(df$date) == 12 & year(df$date) == year(Sys.time())-1,]
    } else {
      df = df[month(df$date) == month(Sys.time())-1 & year(df$date) == year(Sys.time()),]
    }
    df = subset(df, amount < 0)
    df$amount = df$amount * -1
    df = group_by(df, comp) %>% summarize(amount = sum(amount))
    
    ### reorder 
    dfClear = df
    dfClear = dfClear[order(dfClear$amount, decreasing = F),]
    dfClear$comp = factor(dfClear$comp, levels = dfClear$comp)
    
    ggplot(dfClear, aes(x = comp, y = amount )) +
      geom_bar(stat = "identity", fill = "cyan") +
      geom_text(aes(y = 0, label = paste0(format(dfClear$amount, digits = 2, big.mark = "\\."), ",-")), hjust = -.1, color = "black") + 
      coord_flip() + 
      xlab(NULL) + 
      ylab(NULL) +
      dark_theme
  })
  
  ### Deatils plots
  ### --------------------------------------------------------------------------
  output$plotThisMonthGroceries <- renderPlotly({
    df = data.frame(date = DATA$sendDay,
                    amount = DATA$amount,
                    comp = DATA$comp)
    df = df[month(df$date) == month(Sys.time()) & year(df$date) == year(Sys.time()),]
    df = subset(df, amount < 0)
    df$amount = df$amount * -1
    df = group_by(df, comp) %>% summarize(amount = sum(amount))
    df = subset(df, comp %in% discounterList)
    df = df[order(df$amount, decreasing = T),]
    df$comp = factor(df$comp, levels = df$comp)
    
    plot_ly(df, labels = ~comp, values = ~amount, type = 'pie',
            marker = list(colors = (viridis(7)),
                          line = list(color = '#FFFFFF', width = .5))) %>% 
      layout(paper_bgcolor='#323E47',plot_bgcolor='#323E47',
             font = list(color = "#FFFFFF"),
             title = list(text = paste0("Total: ", format(sum(df$amount),
                                                          big.mark = ".",
                                                          decimal.mark = ",")),
                          y = 0.99)
      )
  })
  output$plotThisMonthGas <- renderPlotly({
    df = data.frame(date = DATA$sendDay,
                    amount = DATA$amount,
                    comp = DATA$comp)
    df = df[month(df$date) == month(Sys.time()) & year(df$date) == year(Sys.time()),]
    df = subset(df, amount < 0)
    df$amount = df$amount * -1
    df = group_by(df, comp) %>% summarize(amount = sum(amount))
    df = subset(df, comp %in% stationsList)
    df = df[order(df$amount, decreasing = T),]
    df$comp = factor(df$comp, levels = df$comp)
    
    plot_ly(df, labels = ~comp, values = ~amount, type = 'pie',
            marker = list(colors = (viridis(7)),
                          line = list(color = '#FFFFFF', width = .5))) %>% 
      layout(paper_bgcolor='#323E47',plot_bgcolor='#323E47',
             font = list(color = "#FFFFFF"),
             title = list(text = paste0("Total: ", format(sum(df$amount),
                                                          big.mark = ".",
                                                          decimal.mark = ",")),
                          y = 0.99)
      )
  })
  
  output$plotLastMonthGroceries <- renderPlotly({
    df = data.frame(date = DATA$sendDay,
                    amount = DATA$amount,
                    comp = DATA$comp)
    if(month(Sys.time())-1 == 0){
      df = df[month(df$date) == 12 & year(df$date) == year(Sys.time())-1,]
    } else {
      df = df[month(df$date) == month(Sys.time())-1 & year(df$date) == year(Sys.time()),]
    }
    df = subset(df, amount < 0)
    df$amount = df$amount * -1
    df = group_by(df, comp) %>% summarize(amount = sum(amount))
    df = subset(df, comp %in% discounterList)
    df = df[order(df$amount, decreasing = T),]
    df$comp = factor(df$comp, levels = df$comp)
    
    plot_ly(df, labels = ~comp, values = ~amount, type = 'pie',
            marker = list(colors = (viridis(7)),
                          line = list(color = '#FFFFFF', width = .5))) %>% 
      layout(paper_bgcolor='#323E47',plot_bgcolor='#323E47',
             font = list(color = "#FFFFFF"),
             title = list(text = paste0("Total: ", format(sum(df$amount),
                                                          big.mark = ".",
                                                          decimal.mark = ",")),
                          y = 0.99)
             )
  })
  output$plotLastMonthGas <- renderPlotly({
    df = data.frame(date = DATA$sendDay,
                    amount = DATA$amount,
                    comp = DATA$comp)
    if(month(Sys.time())-1 == 0){
      df = df[month(df$date) == 12 & year(df$date) == year(Sys.time())-1,]
    } else {
      df = df[month(df$date) == month(Sys.time())-1 & year(df$date) == year(Sys.time()),]
    }
    df = subset(df, amount < 0)
    df$amount = df$amount * -1
    df = group_by(df, comp) %>% summarize(amount = sum(amount))
    df = subset(df, comp %in% stationsList)
    df = df[order(df$amount, decreasing = T),]
    df$comp = factor(df$comp, levels = df$comp)
    
    plot_ly(df, labels = ~comp, values = ~amount, type = 'pie',
            marker = list(colors = (viridis(7)),
                          line = list(color = '#FFFFFF', width = .5))) %>% 
      layout(paper_bgcolor='#323E47',plot_bgcolor='#323E47',
             font = list(color = "#FFFFFF"),
             title = list(text = paste0("Total: ", format(sum(df$amount),
                                                          big.mark = ".",
                                                          decimal.mark = ",")),
                          y = 0.99)
      )
  })
  
  output$plotSelectedMonthGroceries <- renderPlotly({
    df = data.frame(date = DATA$sendDay,
                    amount = DATA$amount,
                    comp = DATA$comp)
    df = subset(df, df$date >= daterange()[1] & df$date <= daterange()[2])
    df = subset(df, amount < 0)
    df$amount = df$amount * -1
    df = group_by(df, comp) %>% summarize(amount = sum(amount))
    df = subset(df, comp %in% discounterList)
    df = df[order(df$amount, decreasing = T),]
    df$comp = factor(df$comp, levels = df$comp)
    
    plot_ly(df, labels = ~comp, values = ~amount, type = 'pie',
            marker = list(colors = (viridis(7)),
                          line = list(color = '#FFFFFF', width = .5))) %>% 
      layout(paper_bgcolor='#323E47',plot_bgcolor='#323E47',
             font = list(color = "#FFFFFF"),
             title = list(text = paste0("Total: ", format(sum(df$amount),
                                                          big.mark = ".",
                                                          decimal.mark = ",")),
                          y = 0.99)
      )
  })
  output$plotSelectedMonthGas <- renderPlotly({
    df = data.frame(date = DATA$sendDay,
                    amount = DATA$amount,
                    comp = DATA$comp)
    df = subset(df, df$date >= daterange()[1] & df$date <= daterange()[2])
    df = subset(df, amount < 0)
    df$amount = df$amount * -1
    df = group_by(df, comp) %>% summarize(amount = sum(amount))
    df = subset(df, comp %in% stationsList)
    df = df[order(df$amount, decreasing = T),]
    df$comp = factor(df$comp, levels = df$comp)
    
    plot_ly(df, labels = ~comp, values = ~amount, type = 'pie',
            marker = list(colors = (viridis(7)),
                          line = list(color = '#FFFFFF', width = .5))) %>% 
      layout(paper_bgcolor='#323E47',plot_bgcolor='#323E47',
             font = list(color = "#FFFFFF"),
             title = list(text = paste0("Total: ", format(sum(df$amount),
                                                          big.mark = ".",
                                                          decimal.mark = ",")),
                          y = 0.99)
      )
  })
}


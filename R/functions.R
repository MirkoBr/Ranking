### ============================================================================
###
###                       Functions
###
### ----------------------------------------------------------------------------
###

importData <- function(x){
  data = read_excel(x)
  data = as.data.frame(data)
  rownames(data) = data$Date
  data[is.na(data)] = 0
  return(data)
}


colScaled_Dark2 = colorRampPalette(brewer.pal(8, "Dark2"))

dark_theme = theme(text = element_text(family = 'Gill Sans', color = "#444444"),
                    panel.background = element_rect(fill = '#323e47'),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.line = element_line(colour = "lightgrey"),
                    plot.title = element_text(size = 12, color = "lightgrey"),
                    axis.title = element_text(size = 12, color = 'lightgrey'),
                    plot.background = element_rect(fill="#323e47"),
                    panel.border = element_blank(),
                    axis.text.y = element_text(color = "lightgrey"),
                    axis.text.x = element_text(color = "lightgrey"),
                   legend.key = element_rect(colour = NA, fill = "lightgrey"),
                   legend.background = element_rect(fill = 'lightgrey')
)


parsePoolFromExcel <- function(x){
  ws = x
  # find the blank rows
  blankrows  <- data_frame(
    blanks = which(is.na(ws[1]))
  ) %>%
    mutate(
      dif = blanks - lag(blanks)
      , rownum = row_number()

      # maybe someone can suggest a better way to handle using dplyr::lag()
      , startrow = ifelse(rownum == 1, 1, NA)
      , startrow = coalesce(ifelse(dif == 1, lag(startrow, default =1), lag(blanks + 1)), 1)
    )

  # get the end rows of each table
  endrows  <- blankrows %>%
    group_by(startrow) %>%
    summarize(
      endrow = min(blanks)
    )

  # combine start and end rows into a single table
  tableindex <- blankrows %>%
    left_join(endrows, by = 'startrow') %>%
    distinct(startrow, endrow)

  # the last blank row is probably just before the last table in the sheet
  if(nrow(ws) > max(blankrows$blanks)) {

    lasttable  <- data_frame(startrow = max(blankrows$blanks) + 1, endrow = nrow(ws))
    tableindex  <- tableindex %>%
      bind_rows(lasttable)
  }

  # split your tables up into a list of tables
  tableList  <- map(1:nrow(tableindex), ~ ws[tableindex$startrow[.x]:tableindex$endrow[.x] , ]  )
  return(tableList)
}

clearPool <- function(x){
  # drop na cols from dataframe and reformat
  df = x %>% as.data.frame()
  df = Filter(function(x)!all(is.na(x)), df)
  df = t(df)
  df = as.data.frame(df)
  df = Filter(function(x)!all(is.na(x)), df)
  df = t(df)
  df = as.data.frame(df)

  poolDate = as.Date(as.numeric(df[[1]][1]), origin = "1899-12-30")

  rName = df[,1][-1]
  cName = c("Self", rName)

  df = df[2:nrow(df),]
  rownames(df) = rName
  colnames(df) = cName

  df$Date = poolDate
  df$MaxHit = max(df[1,c(3:ncol(df)-1)] %>% as.numeric %>% max(., na.rm = TRUE),
                  df[2,c(3:ncol(df)-1)] %>% as.numeric %>% max(., na.rm = TRUE),
                  df[3,c(3:ncol(df)-1)] %>% as.numeric %>% max(., na.rm = TRUE),
                  df[4,c(3:ncol(df)-1)] %>% as.numeric %>% max(., na.rm = TRUE))

  return(df)
}

# extract performance summary per fencer
preparePoolSummary <- function(x, fencer, hits){
  d1 = filter(x, variable == fencer & MaxHit == hits)
  d2 = filter(x, Self == fencer & MaxHit == hits)

  df = d1
  df$received = d1$value
  df$scored = d2$value
  df$index = df$scored - df$received
  df$value = NULL
  return(df)
}

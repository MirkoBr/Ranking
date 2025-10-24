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
                    plot.subtitle = element_text(size = 12, color = "lightgrey"),
                    axis.title = element_text(size = 12, color = 'lightgrey'),
                    plot.background = element_rect(fill="#323e47"),
                    panel.border = element_blank(),
                    axis.text.y = element_text(color = "lightgrey"),
                    axis.text.x = element_text(color = "lightgrey"),
                   legend.key = element_rect(colour = NA, fill = "lightgrey"),
                   legend.background = element_rect(fill = 'lightgrey'),
                   plot.caption = element_text(color = "lightgrey")
                   # panel.background = element_blank(),
                   # plot.background = element_blank()
)


parsePoolFromExcel <- function(x) {
  ws <- x

  # Find blank rows
  blankrows <- tibble(
    blanks = which(is.na(ws[1]))
  ) %>%
    mutate(
      dif = blanks - lag(blanks),
      rownum = row_number(),
      startrow = ifelse(rownum == 1, 1, NA),
      startrow = coalesce(ifelse(dif == 1, lag(startrow, default = 1), lag(blanks + 1)), 1)
    )

  # Get end rows of each table
  endrows <- blankrows %>%
    group_by(startrow) %>%
    summarize(endrow = min(blanks), .groups = "drop")

  # Combine start and end rows
  tableindex <- blankrows %>%
    left_join(endrows, by = "startrow") %>%
    distinct(startrow, endrow)

  # Add final table if needed
  if (nrow(ws) > max(blankrows$blanks)) {
    lasttable <- tibble(startrow = max(blankrows$blanks) + 1, endrow = nrow(ws))
    tableindex <- bind_rows(tableindex, lasttable)
  }

  # Split and clean tables
  tableList <- map(1:nrow(tableindex), function(i) {
    tbl <- ws[tableindex$startrow[i]:tableindex$endrow[i], ]

    # Remove rows where all values are NA
    tbl <- tbl[rowSums(is.na(tbl)) < ncol(tbl), ]

    # Remove columns where all values are NA
    tbl <- tbl[, colSums(is.na(tbl)) < nrow(tbl)]

    return(tbl)
  })

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


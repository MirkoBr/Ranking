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
                    plot.title = element_text(size = 28, color = "lightgrey"),
                    axis.title = element_text(size = 18, color = 'lightgrey'),
                    plot.background = element_rect(fill="#323e47"),
                    panel.border = element_blank(),
                    axis.text.y = element_text(color = "lightgrey"),
                    axis.text.x = element_text(color = "lightgrey"),
                   legend.key = element_rect(colour = NA, fill = "lightgrey"),
                   legend.background = element_rect(fill = 'lightgrey')
)

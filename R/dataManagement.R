### ============================================================================
###
###                       Manage data import for dashboard
###
### ----------------------------------------------------------------------------
### 

### ============================================================================
### Top level imports
### ----------------------------------------------------------------------------
### 

importDataFamily <- function(file){
  ### read history
  hist1 = importDataFromDKB("./Data/Archive/grio/giroFamily2017.csv")
  hist2 = importDataFromDKB("./Data/Archive/grio/giroFamily2018.csv")
  hist3 = importDataFromDKB("./Data/Archive/grio/giroFamily2019.csv")
  hist1 = cleanDataFromDKB(hist1)
  hist2 = cleanDataFromDKB(hist2)
  hist3 = cleanDataFromDKB(hist3)
  hist = rbind(hist1,hist2,hist3)
  ### read current
  current = importDataFromDKB(file)
  current = cleanDataFromDKB(current)
  ### combine and return
  all = rbind(hist, current)
  all = all[order(all$sendDay, decreasing = T),]
  return(all)
}

importDataJana <- function(file){
  ### read history
  hist = read.csv("./Data/Archive/giroJana.csv",
                  colClasses = c(NA,"Date","character","character","numeric", "character"))[,2:6]
  ### update history with new file
  current = importDataFromDKB(file)
  current = cleanDataFromDKB(current)
  all = rbind(hist, current)
  ### remove umlaute
  all$type = stringi::stri_replace_all_fixed(all$type,c("ä", "ö", "ü", "Ä", "Ö", "Ü"), c("ae", "oe", "ue", "Ae", "Oe", "Ue"), vectorize_all = FALSE)
  all$sendTo = stringi::stri_replace_all_fixed(all$sendTo,c("ä", "ö", "ü", "Ä", "Ö", "Ü"), c("ae", "oe", "ue", "Ae", "Oe", "Ue"), vectorize_all = FALSE)
  all$comp = stringi::stri_replace_all_fixed(all$comp,c("ä", "ö", "ü", "Ä", "Ö", "Ü"), c("ae", "oe", "ue", "Ae", "Oe", "Ue"), vectorize_all = FALSE)
  all = unique(all)
  all = all[order(all$sendDay, decreasing = T),]
  
  ### return updated file and save as new history
  write.csv(all, "./Data/Archive/grio/giroJana.csv")
  return(all)
}

importDataMirko <- function(file){
  ### read history
  hist2018 = cleanDataFromSPK(importDataFromSPK("./Data/Archive/grio/giroMirko2018.csv"))
  hist2019 = cleanDataFromSPK(importDataFromSPK("./Data/Archive/grio/giroMirko2019.csv"))
  hist = rbind(hist2018, hist2019)
  # hist = cleanDataFromSPK(hist)
  ### read current file
  current = importDataFromSPK(file)
  current = cleanDataFromSPK(current)
  ### combine and return
  all = rbind(hist, current)
  all = all[order(all$sendDay, decreasing = T),]
  return(all)
}


importDepotDataFamily <- function(file){
  ### read history
  hist1 = importDepotDataFromDKB("./Data/Archive/depot/depotFamily2019.csv")
  hist1 = cleanDepotDataFromDKB(hist1)
  ### read current year data
  current = importDepotDataFromDKB(file)
  current = cleanDepotDataFromDKB(current)
  all = rbind(hist1, current)
  all = all[order(all$sendDay, decreasing = T),]
  return(all)
}

importDayDataMirko <- function(file){
  current = importDayDataFromSPK(file)
  current = cleanDayDataFromSPK(current)
  all = current
  all = all[order(all$sendDay, decreasing = T),]
  return(all)
}

importCreditDataMirko <- function(file){
  current = importCreditDataFromSPK(file)
  current = cleanCreditDataFromSPK(current)
  all = current
  all = all[order(all$sendDay, decreasing = T),]
  return(all)
}

### ============================================================================
### DKB imports
### ----------------------------------------------------------------------------
### 
importDataFromDKB <- function(path){
  data = read.csv2(file = path, header = TRUE,
                   skip = 6, sep = ";", dec = ",",
                   colClasses = "character",
                   encoding = "latin1")
  return(data)
}

cleanDataFromDKB <- function(df){
  cleanDF = df[c(1,3,4,8)]
  names(cleanDF) = c("sendDay", "type", "sendTo", "amount")
  cleanDF$amount = gsub("\\.","", cleanDF$amount)
  cleanDF$amount = gsub(",","\\.", cleanDF$amount)
  cleanDF$amount = as.numeric(cleanDF$amount)
  cleanDF$sendDay = gsub("\\.", "-",cleanDF$sendDay)
  cleanDF$sendDay = as.Date(cleanDF$sendDay, format = "%d-%m-%Y")
  cleanDF$sendTo = as.character(cleanDF$sendTo)
  cleanDF$sendTo = toupper(cleanDF$sendTo)
  cleanDF$comp = formatCompanies(cleanDF$sendTo)
  return(cleanDF)
}

importDepotDataFromDKB <- function(path){
  # data = read.csv2(file = path, header = T, skip = 6, sep = ";",
  #                  dec = ",", colClasses = rep("character",5), NULL,
  #                  fileEncoding = "utf-8")
  data = read.csv2(file = path, header = F, skip = 7, sep = ";")
  return(data)
}

cleanDepotDataFromDKB <- function(df){
  cleanDF = df[c(2,5)]
  names(cleanDF) = c("sendDay", "amount")
  # cleanDF$amount = gsub("\\.","", cleanDF$amount)
  cleanDF$amount = gsub(",","\\.", cleanDF$amount)
  cleanDF$amount = as.numeric(cleanDF$amount)
  cleanDF$sendDay = as.character(cleanDF$sendDay)
  cleanDF$sendDay = as.Date(cleanDF$sendDay, format = "%d.%m.%Y")
  return(cleanDF)  
}

### ============================================================================
### SPK imports
### ----------------------------------------------------------------------------
### 
importDataFromSPK <- function(path){
  data = read.csv2(file = path, header = TRUE,
                   skip = 0, sep = ";", dec = ",",
                   colClasses = "character")
  return(data)
}

cleanDataFromSPK <- function(df){
  cleanDF = df[c(2,4,12,15)]
  names(cleanDF) = c("sendDay", "type", "sendTo", "amount")
  cleanDF$amount = gsub("\\.","", cleanDF$amount)
  cleanDF$amount = gsub(",","\\.", cleanDF$amount)
  cleanDF$amount = as.numeric(cleanDF$amount)
  cleanDF$sendDay = as.Date(cleanDF$sendDay, format = "%d.%m.%y")
  cleanDF = subset(cleanDF, type != c("Abschluss"))
  cleanDF$sendTo = as.character(cleanDF$sendTo)
  cleanDF$sendTo = toupper(cleanDF$sendTo)
  cleanDF$comp = formatCompanies(cleanDF$sendTo)
  return(cleanDF)
}

importDayDataFromSPK <- function(path){
  data = read.csv2(file = path, header = T, colClasses = "character")  
  return(data)
}

cleanDayDataFromSPK <- function(df){
  cleanDF = df[c(2,15)]
  names(cleanDF) = c("sendDay", "amount")
  cleanDF$amount = gsub("\\.","", cleanDF$amount)
  cleanDF$amount = gsub(",","\\.", cleanDF$amount)
  cleanDF$amount = as.numeric(cleanDF$amount)
  cleanDF$sendDay = as.Date(cleanDF$sendDay, format = "%d.%m.%y")
  return(cleanDF)
}


importCreditDataFromSPK <- function(path){
  data = read.csv2(file = path, header = T, skip = 1, colClasses = "character")
  return(data)  
}

cleanCreditDataFromSPK <- function(df){
  cleanDF = df[c(3,4,9)]
  names(cleanDF) = c("sendDay", "amount", "sendTo")
  cleanDF$amount = gsub("\\.","", cleanDF$amount)
  cleanDF$amount = gsub(",","\\.", cleanDF$amount)
  cleanDF$amount = as.numeric(cleanDF$amount)
  cleanDF$sendDay = as.Date(cleanDF$sendDay, format = "%d.%m.%y")
  cleanDF$sendTo = as.character(cleanDF$sendTo)
  cleanDF$sendTo = toupper(cleanDF$sendTo)
  cleanDF$comp = formatCompanies(cleanDF$sendTo)
  return(cleanDF)
}

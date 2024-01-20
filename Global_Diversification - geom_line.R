### This R script has been created to make line graphs of equity indices
#   and conversion factors for equities, FX rates, and CPI time series.
library(ggalt)
library(ggplotify)
library(ggpmisc)
library(gridExtra)
library(scales)
library(tidyverse)
library(readr)

### Set working directory
setwd("C:/Users/Kunal Kunde/OneDrive/Documents/SOM 660/Lec 3")

### Read in equity, forex and inflation data
Equity_Index_Dates_n_Values <- read_tsv("equity_index_values.txt", col_names = TRUE) 
Equity_Index_Values <- subset(Equity_Index_Dates_n_Values, select = -c(Date))
FX_Rate_Dates_n_Values <- read_tsv("FX_rates.txt", col_names = TRUE)
FX_Rate_Values <- subset(FX_Rate_Dates_n_Values, select = -c(Date))
Inflation_Index_Dates_n_Values <- read_tsv("inflation_index_values.txt", col_names = TRUE)
Inflation_Index_Values <- subset(Inflation_Index_Dates_n_Values, select = -c(Date))

### Calculate corresponding conversion factors
Inflation_Conversion_Factors <- data.frame(Inflation_Index_Dates_n_Values$Date,t(t(Inflation_Index_Values)/t(Inflation_Index_Values)[,1]))
colnames(Inflation_Conversion_Factors)[1] <- "Date"
Equity_Conversion_Factors <- data.frame(Equity_Index_Dates_n_Values$Date,t(t(Equity_Index_Values)/t(Equity_Index_Values)[,1])) 
colnames(Equity_Conversion_Factors)[1] <- "Date"
FX_Conversion_Factors <- data.frame(FX_Rate_Dates_n_Values$Date,t(t(FX_Rate_Values)/t(FX_Rate_Values)[,1]))
colnames(FX_Conversion_Factors)[1] <- "Date"

### Calculate dollarized equity index values
Equity_Index_Nominal_USD_Dates_n_Values <- data.frame(Equity_Index_Dates_n_Values$Date, as.data.frame(Equity_Index_Values/FX_Rate_Values))
colnames(Equity_Index_Nominal_USD_Dates_n_Values)[1] <- "Date"
### Calculate dollarized equity index values also adjusted for inflation
Equity_Index_Real_USD_Dates_n_Values <- data.frame(Equity_Index_Dates_n_Values$Date, as.data.frame(subset(Equity_Index_Nominal_USD_Dates_n_Values, select = -c(Date))/subset(Inflation_Conversion_Factors, select = -c(Date)) ) )
colnames(Equity_Index_Real_USD_Dates_n_Values)[1] <- "Date"

listOfDataFrames <- list(Equity_Index_Dates_n_Values, FX_Rate_Dates_n_Values, Inflation_Index_Dates_n_Values, Equity_Index_Nominal_USD_Dates_n_Values, Equity_Index_Real_USD_Dates_n_Values)
countryNames <- c("Australia", "Japan", "China", "Canada","Switzerland", "USA", "UK", "HK", "France", "Germany", "South Korea", "Brazil", "Ireland", "Taiwan", "India", "Netherlands", "Russia", "South Africa")
currencyNames <- c("AUD", "JPY", "CNY", "CAD","CHF", "USD", "GBP", "HKD", "EUR", "EUR", "KRW", "BRL", "EUR", "TWD", "INR", "EUR", "RUB", "ZAR")
eqIndexNames <- c("ASX200", "Nikkei", "Shanghai Comp.", "TSX Comp.","SMI", "S&P 500", "FTSE 100", "Hang Seng", "CAC 40", "DAX", "Kospi", "Bovespa", "ISEQ", "Taiex", "Nifty 50", "AEX", "MOEX", "Top 40")
names(listOfDataFrames)=c("EQ","FX","CPI","EQ_nom_USD","EQ_real_USD")

index <- 15
dataForPlot <- data.frame(listOfDataFrames[[1]][1], listOfDataFrames[[1]][index+1], listOfDataFrames[[2]][index+1], listOfDataFrames[[4]][index+1], listOfDataFrames[[5]][index+1])
colnames(dataForPlot)[3] <- "FX_rate"
colnames(dataForPlot)[4] <- "Eq_index_USD_nominal"
colnames(dataForPlot)[5] <- "Eq_index_USD_real"

ggplot(dataForPlot, aes(x = dmy(Date))) + geom_line(aes(y = dataForPlot[,2]), color = "blue", size = 1,linetype = "solid")+xlab("Year")+ylab("Equity Index in local ccy")+labs(title = paste0(countryNames[index],": ",eqIndexNames[index]," (",currencyNames[index],")"))
ggplot(dataForPlot, aes(x = dmy(Date))) + geom_line(aes(y = dataForPlot[,3]), color = "green", size = 1,linetype = "solid")+xlab("Year")+ylab("FX rate: units of local ccy per USD")+labs(title = paste0(countryNames[index], ":USD",currencyNames[index]))
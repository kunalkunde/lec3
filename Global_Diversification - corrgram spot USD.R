### This R script has been created to make a corrgram
# of global equity indices' monthly dollarized returns   
library(ggalt)
library(ggpmisc)
library(grid)
library(gridExtra)
library(corrgram)
library(showtext)
library(tidyverse)
library(readr)

### Set working directory
setwd("C:/Users/Kunal Kunde/OneDrive/Documents/SOM 660/Lec 3")
source("computeMthRetFn.R")

# showtext package enables addition of fonts
font_add(family = "Trebuchet MS", regular = "trebuc.ttf")
showtext_auto() # makes fonts visible to ggplot

### Read in equity and forex data
Equity_Index_Dates_n_Values <- read_tsv("equity_index_values.txt", col_names = TRUE)
Equity_Index_Values <- subset(Equity_Index_Dates_n_Values, select = -c(Date))
FX_Rate_Dates_n_Values <- read_tsv("FX_rates.txt", col_names = TRUE)
FX_Rate_Values <- subset(FX_Rate_Dates_n_Values, select = -c(Date))

### Calculate dollarized equity index values
Equity_Index_Nominal_USD_Dates_n_Values <- data.frame(Equity_Index_Dates_n_Values$Date, as.data.frame(Equity_Index_Values/FX_Rate_Values))
colnames(Equity_Index_Nominal_USD_Dates_n_Values)[1] <- "Date"
Equity_Index_Nominal_USD_Values <- subset(Equity_Index_Nominal_USD_Dates_n_Values, select = -c(Date))
Equity_Index_Nominal_USD_Conversion_Factors <- data.frame(Equity_Index_Nominal_USD_Dates_n_Values$Date,t(t(Equity_Index_Nominal_USD_Values)/t(Equity_Index_Nominal_USD_Values)[,1])) 
colnames(Equity_Index_Nominal_USD_Conversion_Factors)[1] <- "Date"

mthNominalDollarRetDataForCorrgram <- compute_monthly_returns(Equity_Index_Nominal_USD_Conversion_Factors)
colnames(mthNominalDollarRetDataForCorrgram) <- c("Aus","Jpn","Chn","Can","Sui","USA","UK","HK","Fra","Ger","S Kor","Bra","Ire","Twn","Ind","Ned","Rus","SA")
mthNominalDollarRetDataForCorrgram <- data.frame(mthNominalDollarRetDataForCorrgram[,order(colnames(mthNominalDollarRetDataForCorrgram))])
correlationMatrix <- cor(mthNominalDollarRetDataForCorrgram[,-1])
correlationMatrix = format(round(correlationMatrix[,1:18],2),nsmall=2)
write.table(correlationMatrix, file = "correlation_matrix_eq_ret_conv_per_spot_USD.txt", sep ="\t", row.names = TRUE, col.names = TRUE )

corrgram(mthNominalDollarRetDataForCorrgram, order=FALSE, lower.panel=panel.cor, upper.panel=panel.ellipse, text.panel=panel.txt, main="Corrgram of monthly dollarized returns\n31-Dec-1999 to 15-Dec-2021")
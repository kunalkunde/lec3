### Dumbbell plot to see lcl-vs-glbl performance
library(ggalt)
library(ggpmisc)
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

### Read in global portfolio values based on yearly re-balance to MSCI ACWI weights (every December-end)
Global_Portfolio_Dates_n_Values <- read_tsv("global_ann_rebal_portfolio_values_in_USD.txt", col_names = TRUE)
Global_Portfolio_Values <- subset(Global_Portfolio_Dates_n_Values, select = -c(Date))
# these are USD values and their computation is laid out the following Excel file 
# <Annually_rebalanced_global_portfolio.xlsx> in the same "Lec 3" working directory

### Calculate conversion factors
Inflation_Conversion_Factors <- data.frame(Inflation_Index_Dates_n_Values$Date,t(t(Inflation_Index_Values)/t(Inflation_Index_Values)[,1])) 
colnames(Inflation_Conversion_Factors)[1] <- "Date"
# second arg in the data.frame() constructor above is a scaling that can be done only via column division, hence successive transposes on the RHS
# source = https://www.geeksforgeeks.org/divide-each-row-of-matrix-by-vector-elements-in-r/
Equity_Conversion_Factors <- data.frame(Equity_Index_Dates_n_Values$Date,t(t(Equity_Index_Values)/t(Equity_Index_Values)[,1])) 
colnames(Equity_Conversion_Factors)[1] <- "Date"
FX_Conversion_Factors <- data.frame(FX_Rate_Dates_n_Values$Date,t(t(FX_Rate_Values)/t(FX_Rate_Values)[,1])) 
colnames(FX_Conversion_Factors)[1] <- "Date"
Global_Portfolio_Conversion_Factors <- data.frame(Global_Portfolio_Dates_n_Values$Date,t(t(Global_Portfolio_Values)/t(Global_Portfolio_Values)[,1])) 
colnames(Global_Portfolio_Conversion_Factors)[1] <- "Date"

### Calculate dollarized equity index values
Equity_Index_Nominal_USD_Dates_n_Values <- data.frame(Equity_Index_Dates_n_Values$Date, as.data.frame(Equity_Index_Values/FX_Rate_Values))
colnames(Equity_Index_Nominal_USD_Dates_n_Values)[1] <- "Date"
### Calculate dollarized equity index values also adjusted for inflation
Equity_Index_Real_USD_Dates_n_Values <- data.frame(Equity_Index_Dates_n_Values$Date, as.data.frame(subset(Equity_Index_Nominal_USD_Dates_n_Values, select = -c(Date))/subset(Inflation_Conversion_Factors, select = -c(Date)) ) )
colnames(Equity_Index_Real_USD_Dates_n_Values)[1] <- "Date"

lcl_ccy_return_1_mth <- as.data.frame(Equity_Index_Values/lag(Equity_Index_Values,1)-1)
min_1_mth_lcl_ccy_returns <- as.data.frame(apply(lcl_ccy_return_1_mth, 2, min, na.rm=TRUE))
min_1_mth_lcl_ccy_ret_indices <- as.data.frame(apply(lcl_ccy_return_1_mth, 2, which.min))
min_1_mth_lcl_ccy_ret_dates <- as.data.frame(Equity_Index_Dates_n_Values$Date[min_1_mth_lcl_ccy_ret_indices[,1]])
glbl_pf_return_1_mth <- as.data.frame(Global_Portfolio_Values/lag(Global_Portfolio_Values,1)-1)
glbl_pf_return_1_mth_contemporaneous <- as.data.frame(glbl_pf_return_1_mth$Global_portfolio_USD[min_1_mth_lcl_ccy_ret_indices[,1]])
Country_Date <- paste(as.vector(rownames(min_1_mth_lcl_ccy_returns)),as.vector(as.character(min_1_mth_lcl_ccy_ret_dates[,1])),sep="\n")
dataForPlot<-data.frame(Country_Date, Min_Lcl_Ccy_Ret=min_1_mth_lcl_ccy_returns[,1], Corr_Glbl_Pf_USD_Ret=glbl_pf_return_1_mth_contemporaneous[,1])
ggplot() + geom_dumbbell(data = dataForPlot, aes(y=Country_Date, x=Min_Lcl_Ccy_Ret, xend=Corr_Glbl_Pf_USD_Ret), size_x = 3, size_xend = 4,colour_x = "red", colour_xend = "blue") + geom_text(color="red", size=3.5, hjust=0, vjust=1, aes(x=dataForPlot$Min_Lcl_Ccy_Ret, y=dataForPlot$Country_Date, label=format(scales::percent(dataForPlot$Min_Lcl_Ccy_Ret, accuracy=.11, trim = FALSE),nsmall=2))) + geom_text(color="blue", size=3.5, hjust=0, vjust=1, aes(x=dataForPlot$Corr_Glbl_Pf_USD_Ret, y=dataForPlot$Country_Date, label=format(scales::percent(dataForPlot$Corr_Glbl_Pf_USD_Ret, accuracy=.11, trim = FALSE),nsmall=2)))+xlab("Worst 1M local-ccy ret (red) vs. contemporaneous return on global portfolio (blue)")+ylab("Country, date for worst 1M local-ccy ret")
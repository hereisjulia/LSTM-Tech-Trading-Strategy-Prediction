library(readxl)
library(tidyverse)
stock_data <- read_excel("Raw data from BBG.xlsx", sheet = 2)
colnames(stock_data) <- as.character(unlist(stock_data[1, ]))
stock_data <- stock_data[-1,]

SNPS_data <- stock_data[1:5]
IBM_data <- stock_data[6:10]
MSFT_data <- stock_data[11:15]
GOOGL_data <- stock_data[16:20]
NVDA_data <- stock_data[21:25]

comp_list <- list(SNPS_data, IBM_data, MSFT_data) #, GOOGL_data, NVDA_data)
list_names <- c("SNPS", "IBM", "MSFT") #, "GOOGL", "NVDA")
names(comp_list) <- list_names


library(TTR) # for EWM

origin_date <- as.Date("1899-12-30")

convert_data <- function(data, origin_date) {
  data$Date <- as.numeric(as.character(data$Date))
  data$Date <- origin_date + data$Date - 2
  data$Close <- as.numeric(as.character(data$Close))
  
  return(data)
}

for (n in seq_along(comp_list)){
  comp_list[[n]] <- convert_data(comp_list[[n]], origin_date)
}

### Calculate indicators #######################################
calculate_indicators <- function(data){
  
  
  # MACD # 移動平均
  EMA12 <- EMA(data$Close, n=12) # 12 days of moving average
  EMA26 <- EMA(data$Close, n=26) # 26 days of moving average
  fast <- EMA12 - EMA26  # 快線
  slow <- EMA(fast, n=9) # 慢線 (快線取9天移動平均)
  data$MACD <- fast - slow # diff between fast and slow

  
  # RSI 
  ##上漲的下跌的分開算移動平均
  data$diff <- c(NA, diff(data$Close))
  up <- ifelse(is.na(data$diff), 0, ifelse(data$diff >= 0, 1*data$diff, 0))
  down <- ifelse(is.na(data$diff), 0, ifelse(data$diff < 0, -1*data$diff, 0))
  
  up_EMA14 <- EMA(up, n=14)
  down_EMA14 <- EMA(down, n=14)
  RS14 <- up_EMA14 / down_EMA14 #快線平均除以慢線平均
  data$RSI14 <- 100 * (RS14 / (1 + RS14))
  
  up_EMA7 <- EMA(up, n=7) #只是用來看黃金&死亡交叉
  down_EMA7 <- EMA(down, n=7)
  RS7 <- up_EMA7 / down_EMA7
  data$RSI7 <- 100 * (RS7 / (1 + RS7))
  #以50為分水嶺
  
  # KD 
  ## 近期最小及最大值
  rolling_min <- rep(NA, length(data$Close))
  rolling_max <- rep(NA, length(data$Close))
  data$RSV <- rep(NA, length(data$Close))
  
  for(i in 1:length(data$Close)) {
    ## find min/max in rolling 9 days
    rolling_min[i] <- ifelse(i < 9, min(data$Close[1:i], na.rm = TRUE), min(data$Close[(i-8):i], na.rm = TRUE))
    rolling_max[i] <- ifelse(i < 9, max(data$Close[1:i], na.rm = TRUE), max(data$Close[(i-8):i], na.rm = TRUE))
    data$RSV[i] <- ((data$Close[i] - rolling_min[i])/(rolling_max[i] - rolling_min[i]))*100
  } # RSV是最近9天的最大值最小值
    
  data$K <- rep(50, nrow(data))
  data$D <- rep(50, nrow(data))
    
  for(i in 2:nrow(data)) {
    data$K[i] <- (2/3 * data$K[i-1]) + (1/3 * data$RSV[i])
    data$D[i] <- (2/3 * data$D[i-1]) + (1/3 * data$K[i])
  }

  return(data)
}

for (n in seq_along(comp_list)){
  comp_list[[n]] <- calculate_indicators(comp_list[[n]])
}

SNPS_data <- comp_list$SNPS
IBM_data <- comp_list$IBM
MSFT_data <- comp_list$MSFT

### Labeling Trading Signal #######################################
## MACD
MACD_signal <- function(data){
  
  changes <- c(0, diff(sign(data$MACD)))
  temp_signal <- with(data, ifelse(changes == -2, -1, ifelse(changes == 2, 1, 0)))
  temp_signal[is.na(changes)] <- NA
  
  data$signal_MACD <- NA
  last_mark <- NA
  
  for(i in 1:nrow(data)) {
    if(!is.na(temp_signal[i])) {
      if(temp_signal[i] != 0) {
        last_mark <- temp_signal[i]
      }
      data$signal_MACD[i] <- last_mark
    }
  }
  
  return(data)
}

for (n in seq_along(comp_list)){
  comp_list[[n]] <- MACD_signal(comp_list[[n]])
}

## RSI
RSI_signal <- function(data){
  
  DIFF <- data$RSI7 - data$RSI14
  changes <- c(0, diff(sign(DIFF)))
  temp_signal <- with(data, ifelse(changes == -2, -1, ifelse(changes == 2, 1, 0)))
  temp_signal[is.na(changes)] <- NA
  
  data$RSI_cross <- NA
  last_mark <- NA
  
  for(i in 1:nrow(data)) {
    if(!is.na(temp_signal[i])) {
      if(temp_signal[i] != 0) {
        last_mark <- temp_signal[i]
      }
      data$RSI_cross[i] <- last_mark
    }
  }
  
  data$signal_RSI <- ifelse(data$RSI14 < 30 & data$RSI_cross == 1, 1,
                     ifelse(data$RSI14 > 70 & data$RSI_cross == -1, -1, 0))
  
  last_signal <- NA
  for(i in 1:nrow(data)) {
    if(!is.na(data$signal_RSI[i])) {
      if(data$signal_RSI[i] == 1) {
        last_signal <- 1
      } else if(data$signal_RSI[i] == -1) {
        last_signal <- -1
      }
      data$signal_RSI[i] <- ifelse(is.na(last_signal), data$signal_RSI[i], last_signal)
    }
  }
  
  return(data)
}

for (n in seq_along(comp_list)){
  comp_list[[n]] <- RSI_signal(comp_list[[n]])
}

## KD
KD_signal <- function(data){
  
  DIFF <- data$K - data$D
  changes <- c(0, diff(sign(DIFF)))
  temp_signal <- with(data, ifelse(changes == -2, -1, ifelse(changes == 2, 1, 0)))
  temp_signal[is.na(changes)] <- NA
  
  data$signal_KD <- NA
  last_mark <- NA
  
  for(i in 1:nrow(data)) {
    if(!is.na(temp_signal[i])) {
      if(temp_signal[i] != 0) {
        last_mark <- temp_signal[i]
      }
      data$signal_KD[i] <- last_mark
    }
  }
  
  return(data)
}

for (n in seq_along(comp_list)){
  comp_list[[n]] <- KD_signal(comp_list[[n]])
}

SNPS_data <- comp_list$SNPS
IBM_data <- comp_list$IBM
MSFT_data <- comp_list$MSFT

### Export to Excel for LSTM model #######################################
library(dplyr)
library(writexl)

trading_signal_list <- list()
Macro_Indicators <- read_excel("Macro Indicators_Clean.xlsx")

for (i in seq_along(comp_list)) {
  trading_signal <- comp_list[[i]] %>% 
    select(Date, signal_MACD, signal_RSI, signal_KD)
  
  input_for_LSTM <- left_join(trading_signal, Macro_Indicators, by = "Date")
  trading_signal_list[[list_names[i]]] <- input_for_LSTM
}

write_xlsx(trading_signal_list, "Trading Signal & Macro Indicators.xlsx")



### Check #######################################
#table(SNPS_data$signal_MACD)
#table(SNPS_data$signal_RSI)
#table(SNPS_data$signal_KD)

#max(MSFT_data$RSI14[!is.na(MSFT_data$RSI14)])
#min(MSFT_data$RSI14[!is.na(MSFT_data$RSI14)])
#max(MSFT_data$RSI7[!is.na(MSFT_data$RSI7)])
#min(MSFT_data$RSI7[!is.na(MSFT_data$RSI7)])

#max(MSFT_data$K)
#min(MSFT_data$K)
#max(MSFT_data$D)
#min(MSFT_data$D)

### Visualization #######################################
library(ggplot2)

SNPS_data$Source <- 'SNPS'
IBM_data$Source <- 'IBM'
MSFT_data$Source <- 'MSFT'
combined_data <- rbind(SNPS_data, IBM_data, MSFT_data)

start_date <- as.Date("2010-01-01")

ggplot(combined_data[!is.na(combined_data$MACD) & combined_data$Date >= start_date,], aes(x = Date, y = MACD, fill = Source)) + geom_col() + 
       facet_wrap(~ Source, scales = 'free_x', nrow=3) +
       ggtitle("MACD Chart") + xlab("Date") + ylab("MACD")

ggplot(combined_data[!is.na(combined_data$RSI14) & combined_data$Date >= start_date,], 
       aes(x = Date, fill = Source)) + 
       geom_line(aes(y = RSI14, color = "RSI14")) +
       geom_line(aes(y = RSI7, color = "RSI7")) +
       facet_wrap(~ Source, scales = 'free_x', nrow=3) +
       ggtitle("RSI Chart") + xlab("Date") + ylab("RSI") +
       scale_color_manual(values = c("RSI14" = "blue", "RSI7" = "red"))

ggplot(combined_data[!is.na(combined_data$K) & combined_data$Date >= start_date,], aes(x = Date, fill = Source)) + 
       geom_line(aes(y = K, color = "K")) +
       geom_line(aes(y = D, color = "D")) +
       facet_wrap(~ Source, scales = 'free_x', nrow=3) +
       ggtitle("KD Chart") + xlab("Date") + ylab("KD") +
       scale_color_manual(values = c("K" = "blue", "D" = "red"))

## Price + Indicators
start_date <- as.Date("2019-01-01")

# MACD
plot_MACD_signal_with_price <- function(data, start_date, list_name) {
  data_clean <- data %>% 
    filter(!is.na(signal_MACD)) %>%
    mutate(Start = lag(signal_MACD) != signal_MACD | row_number() == 1) %>%
    group_by(Group = cumsum(Start)) %>%
    mutate(Start_Date = min(Date), End_Date = max(Date)) %>%
    ungroup()
  
  plot <- ggplot(data_clean[data_clean$Date >= start_date,]) +
    geom_rect(data = subset(data_clean[data_clean$Date >= start_date,], signal_MACD == 1),
              aes(xmin = Start_Date, xmax = End_Date, ymin = -Inf, ymax = Inf),
              fill = "green", alpha = 0.1) +
    geom_rect(data = subset(data_clean[data_clean$Date >= start_date,], signal_MACD == -1),
              aes(xmin = Start_Date, xmax = End_Date, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = 0.1) +
    geom_line(aes(x = Date, y = Close)) +
    ggtitle(paste(list_name, "with MACD Trading Signals")) + xlab("Date") + ylab("Price")
  
  return(plot)
}

MACD_plot_list <- list()

for (n in seq_along(comp_list)){
  list_name <- list_names[n]
  MACD_plot_list[[n]] <- plot_MACD_signal_with_price(comp_list[[n]], start_date, list_name)
}

print(MACD_plot_list)

# RSI
plot_RSI_signal_with_price <- function(data, start_date, list_name) {
  data_clean <- data %>% 
    filter(!is.na(signal_RSI)) %>%
    mutate(Start = lag(signal_RSI) != signal_RSI | row_number() == 1) %>%
    group_by(Group = cumsum(Start)) %>%
    mutate(Start_Date = min(Date), End_Date = max(Date)) %>%
    ungroup()
  
  plot <- ggplot(data_clean[data_clean$Date >= start_date,]) +
    geom_rect(data = subset(data_clean[data_clean$Date >= start_date,], signal_RSI == 1),
              aes(xmin = Start_Date, xmax = End_Date, ymin = -Inf, ymax = Inf),
              fill = "green", alpha = 0.1) +
    geom_rect(data = subset(data_clean[data_clean$Date >= start_date,], signal_RSI == -1),
              aes(xmin = Start_Date, xmax = End_Date, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = 0.1) +
    geom_line(aes(x = Date, y = Close)) +
    ggtitle(paste(list_name, "with RSI Trading Signals")) + xlab("Date") + ylab("Price")
  
  return(plot)
}

RSI_plot_list <- list()

for (n in seq_along(comp_list)){
  list_name <- list_names[n]
  RSI_plot_list[[n]] <- plot_RSI_signal_with_price(comp_list[[n]], start_date, list_name)
}

print(RSI_plot_list)

# KD
plot_KD_signal_with_price <- function(data, start_date, list_name) {
  data_clean <- data %>% 
    filter(!is.na(signal_KD)) %>%
    mutate(Start = lag(signal_KD) != signal_KD | row_number() == 1) %>%
    group_by(Group = cumsum(Start)) %>%
    mutate(Start_Date = min(Date), End_Date = max(Date)) %>%
    ungroup()
  
  plot <- ggplot(data_clean[data_clean$Date >= start_date,]) +
    geom_rect(data = subset(data_clean[data_clean$Date >= start_date,], signal_KD == 1),
              aes(xmin = Start_Date, xmax = End_Date, ymin = -Inf, ymax = Inf),
              fill = "green", alpha = 0.1) +
    geom_rect(data = subset(data_clean[data_clean$Date >= start_date,], signal_KD == -1),
              aes(xmin = Start_Date, xmax = End_Date, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = 0.1) +
    geom_line(aes(x = Date, y = Close)) +
    ggtitle(paste(list_name, "with KD Trading Signals")) + xlab("Date") + ylab("Price")
  
  return(plot)
}

KD_plot_list <- list()

for (n in seq_along(comp_list)){
  list_name <- list_names[n]
  KD_plot_list[[n]] <- plot_KD_signal_with_price(comp_list[[n]], start_date, list_name)
}

print(KD_plot_list)

library(readxl)

list_names <- excel_sheets("Trading Signal & Macro Indicators.xlsx")

comp_list <- list()

for(name in list_names) {
  comp_list[[name]] <- read_excel("Trading Signal & Macro Indicators.xlsx", sheet = name)
}

for (n in seq_along(list_names)){
  comp_list[[n]]$signal_RSI[comp_list[[n]]$signal_RSI == 0] <- NA
  comp_list[[n]] <- na.omit(comp_list[[n]])
}

list2env(comp_list, envir = .GlobalEnv)


### Backtest #######################################
start_date <- as.Date("2010-04-19")
initial_capital <- 10000

# Returns from Actual Trading Signals
signal_return <- function(data, start_date, signal_name){
  data$Date <- as.Date(data$Date)
  data <- data[data$Date >= start_date, ]
  current_capital <- initial_capital
  stock_held <- 0
  previous_signal <- 0
  
  for (i in 1:nrow(data)){
    current_signal <- data[[signal_name]][i]
    current_price <- data$Close[i]
    
    if (current_signal == 1 & previous_signal != 1){
      stock_held <- floor(current_capital / current_price)
      current_capital <- current_capital - stock_held * current_price
    }
    
    if (current_signal == -1 & stock_held > 0){
      current_capital <- current_capital + stock_held * current_price
      stock_held <- 0
    }
    
    previous_signal <- current_signal
  }
  
  latest_capital <- current_capital + (stock_held * data$Close[nrow(data)])
  
  signal_return <- round((latest_capital - initial_capital) / initial_capital * 100,2)
  
  return(signal_return)
}

signals <- c("signal_MACD", "signal_RSI", "signal_KD")
signal_returns <- matrix(0, nrow = length(signals), ncol = length(list_names))
rownames(signal_returns) <- list_names
colnames(signal_returns) <- signals

for (j in 1:length(signals)){
  for(i in 1:length(list_names)) {
    current_data <- comp_list[[list_names[i]]]
    signal_returns[i, j] <- signal_return(current_data, start_date, signals[j])
  }
}

signal_returns <- t(signal_returns)
print(signal_returns)

##########################################################################
####### 待改 #############################################################
# Returns from Predicted Trading Signals
pred_signal_return <- function(data, start_date, signal_name){
  data$Date <- as.Date(data$Date)
  data <- data[data$Date >= start_date, ]
  current_capital <- initial_capital
  stock_held <- 0
  previous_signal <- 0
  
  for (i in 1:nrow(data)){
    current_signal <- data[[signal_name]][i]
    current_price <- data$Close[i]
    
    if (current_signal == 1 & previous_signal != 1){
      stock_held <- floor(current_capital / current_price)
      current_capital <- current_capital - stock_held * current_price
    }
    
    if (current_signal == -1 & stock_held > 0){
      current_capital <- current_capital + stock_held * current_price
      stock_held <- 0
    }
    
    previous_signal <- current_signal
  }
  
  latest_capital <- current_capital + (stock_held * data$Close[nrow(data)])
  
  pred_signal_return <- round((latest_capital - initial_capital) / initial_capital * 100,2)
  
  return(pred_signal_return)
}

signals <- c("signal_MACD", "signal_RSI", "signal_KD")
pred_signal_returns <- matrix(0, nrow = length(list_names), ncol = length(signals))
rownames(pred_signal_returns) <- list_names
colnames(pred_signal_returns) <- signals

for (i in 1:length(list_names)){
  for(j in 1:length(signals)) {
    current_data <- comp_list[[list_names[i]]]
    pred_signal_returns[i, j] <- signal_return(current_data, start_date, signals[j])
  }
}

pred_signal_returns <- t(pred_signal_returns)
print(pred_signal_returns)

# Returns from Lump-sum Investment(單筆投資)
inv_return <- function(data, start_date){
  data$Date <- as.Date(data$Date)
  data <- data[data$Date >= start_date, ]
  current_capital <- initial_capital
  stock_held <- 0
  current_price <- data$Close[i]
    
  stock_held <- floor(current_capital / current_price)
  current_capital <- current_capital - stock_held * current_price
  
  latest_capital <- current_capital + (stock_held * data$Close[nrow(data)])
  
  inv_return <- round((latest_capital - initial_capital) / initial_capital * 100,2)
  
  return(inv_return)
}

inv_returns <- vector("numeric", length(list_names))
names(inv_returns) <- list_names

for (name in list_names){
  current_data <- comp_list[[name]]
  inv_returns[name] <- inv_return(current_data, start_date)
}

print(inv_returns)

 # Returns from Dollar-Cost-Averaging
avg_return <- function(data, start_date, monthly_invest){
  data$Date <- as.Date(data$Date)
  data <- data[data$Date >= start_date, ]

  current_capital <- initial_capital
  stock_held <- 0
  last_investment_month <- NA
  
  for (i in 1:nrow(data)){
    current_year <- format(data$Date[i], "%Y")
    current_month <- format(data$Date[i], "%m")
    
    if(is.na(last_investment_month) || current_month != last_investment_month){
      new_stock_held <- 0
      current_price <- data$Close[i]
      new_stock_held <- floor(monthly_invest / current_price)
      stock_held <- stock_held + new_stock_held 
      current_capital <- current_capital - new_stock_held * current_price
      
      last_investment_month <- current_month
    }
  }
  
  latest_capital <- current_capital + (stock_held * data$Close[nrow(data)])
  
  avg_return <- round((latest_capital - initial_capital) / initial_capital * 100,2)
  
  return(avg_return)
}

avg_returns <- vector("numeric", length(list_names))
names(avg_returns) <- list_names

for (name in list_names){
  current_data <- comp_list[[name]]
  avg_returns[name] <- avg_return(current_data, start_date, monthly_invest=1000)
}

print(avg_returns)

# Final Result
combined_returns <- rbind(signal_returns, Lump_sum_Investment = inv_returns, Dollar_Cost_Averaging = avg_returns)
print(combined_returns)


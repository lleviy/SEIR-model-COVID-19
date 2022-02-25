setwd("C:/Users/admin/Desktop/R/VKRv2")
library(deSolve)   
library(ggplot2)   
library(forecast)
library(plumber)

SEIR <- function(region, start_date, end_date, predict_up_to_date) {
  russia_data = read.csv("data/regions.csv", sep = ";", fileEncoding = "utf-8")
  names(russia_data)[names(russia_data) == 'Заражений'] <- 'Infected'
  names(russia_data)[names(russia_data) == 'Выздоровлений'] <- 'Recovered'
  names(russia_data)[names(russia_data) == 'Смертей'] <- 'Deaths'
  names(russia_data)[names(russia_data) == 'Дата'] <- 'Date'
  names(russia_data)[names(russia_data) == 'Регион'] <- 'Region'
  names(russia_data)[names(russia_data) == 'Регион.население'] <- 'Population'
  region_data = russia_data[russia_data$Region == region,]
  region_data = region_data[order(as.Date(region_data$Date, format="%d.%m.%Y")),]
  
  region_data$Date <- as.Date(region_data$Date, "%d.%m.%Y")
  
  start_date = as.Date(start_date, "%d.%m.%Y")
  end_date = as.Date(end_date, "%d.%m.%Y")
  predict_up_to_date = as.Date(predict_up_to_date, "%d.%m.%Y")
  
  dates = seq(start_date, end_date, "days")
  times = seq(1, length(dates), 1)
  
  fitted_data = region_data[region_data$Date>=start_date & region_data$Date<=end_date,]
  fitted_data$Date <- as.Date(fitted_data$Date, "%d.%m.%Y")
  fitted_data$Infected = fitted_data$Infected - fitted_data$Recovered - fitted_data$Deaths
  fitted_data$time <- times
  
  test_data <- region_data[region_data$Date>end_date & region_data$Date<=predict_up_to_date,]
  test_data$Infected = test_data$Infected - test_data$Recovered - test_data$Deaths
  test_data$Date <- as.Date(test_data$Date, "%d.%m.%Y")
  
  test_dates = seq(end_date + 1, predict_up_to_date, "days")
  test_times = seq(length(dates) + 1, length(dates)+length(test_data$Date), 1)
  test_data$time <- test_times
  model_times = seq(length(dates), length(dates)+length(test_dates) - 1, 1)
  
  full_dates = seq(start_date, predict_up_to_date, "days")
  full_times = seq(1, length(full_dates), 1)
  
  population <- 20000
  I0 <- fitted_data$Infected[1]
  R0 <- fitted_data$Recovered[1]
  D0 <- fitted_data$Deaths[1]
  E0 <- fitted_data$Infected[1]
  S0 <- population - I0 - R0 - D0 - E0
  
  initial_values <- c(S = S0, E = E0, I = I0, R = R0, D = D0)
  
  seir_model <- function(time, state, parms) {  
    with(as.list(c(state, parms)), {  
      N <- S+I+E+R
      
      dS <- -(beta * I/N) * S 
      dE <- (beta * I/N) * S - teta * E
      dI <- teta * E - (gamma + mu) * I            
      dR <- gamma * I
      dD <- mu * I        
      
      return(list(c(dS, dE, dI, dR, dD))) 
    })
  }
  
  distance <- function(parameters, fitted_data) { 
    # Calculate model output
    result <- as.data.frame(ode(y = initial_values, 
                                times = times, 
                                func = seir_model,
                                parms = parameters))
    
    # Calculate sum-of-squares (SSQ) of model fit
    clean_data <- na.omit(fitted_data) # within the function, 
    #select complete cases only from dat
    
    #select from result$I where the times match the times in the data  
    deltasI <- (result$I[result$time %in% clean_data$time] - clean_data$Infected)^2
    deltasR <- (result$R[result$time %in% clean_data$time] - clean_data$Recovered)^2
    deltasM <- (result$D[result$time %in% clean_data$time] - clean_data$Deaths)^2
    
    distance <- sum(deltasI) + sum(deltasR) + sum(deltasM) 
    
    return(distance)
  }
  
  #initial_parameters <- c(beta = 0, gamma = 0.006, teta = 0, mu = 0)
  
  initial_parameters <- c(beta = 0, gamma = 0.008, teta = 0, mu = 0)
  
  # run optim
  optimised <- optim(par = initial_parameters
                     , fn = distance,
                     fitted_data = fitted_data,
                     method = "L-BFGS-B",
                     lower = 0.0009, upper = 2# this argument comes under "..." 
                     # "Further arguments to be passed to fn and gr"
  )
  
  # examine optim() output and plot "best" model against example dataset
  
  print(optimised)
  
  parameters <- c(optimised$par)
  
  model_times <- c(times, test_times)
  model <- as.data.frame(ode(y = initial_values 
                               , times = full_times  
                               ,  func = seir_model 
                               , parms = parameters))
  
  full_data <- rbind(fitted_data, test_data)
  model$Date <- full_dates
  
  library(ggplot2)
  opt_plot <- ggplot()
  opt_plot <- opt_plot + geom_point(aes(x = Date, y = Infected)
                                    , colour = "red"
                                    , shape  = "x" 
                                    , data = fitted_data)+
    geom_point(aes(x=Date, y = Recovered)
               , colour = "green"
               , shape  = "x"
               , data   = fitted_data)+
    geom_point(aes(x=Date, y = Deaths)
               , colour = "blue"
               , shape  = "x"
               , data   = fitted_data)
  
  opt_plot <- opt_plot + geom_line(aes(x = Date, y = I)
                                   , colour = "red"
                                   , data   = model) +
    geom_line(aes(x=Date, y = R)
              , colour = "green"
              , data   = model)+
    geom_line(aes(x=Date, y = D)
              , colour = "blue"
              , data   = model)
  
  opt_plot <- opt_plot + geom_point(aes(x = Date, y = Infected)
                                    , colour = "red"
                                    , data   = test_data) +
    geom_point(aes(x=Date, y = Recovered)
               , colour = "green"
               , data   = test_data)+
    geom_point(aes(x=Date, y = Deaths)
               , colour = "blue"
               , data   = test_data)
  
  opt_plot <- opt_plot +
    labs(title = "Динамика распространения COVID-19 SEIR\n", x = "Дата", y = "Случаи") 
  
  print(opt_plot)
  
  total <- merge(model,full_data, by="time", all=TRUE)
  accuracy <- accuracy(model$I, full_data$Infected)
  print(accuracy)
  return(total)
}

mod <- SEIR("Москва","12.03.2020", "20.03.2020", "03.04.2020")

saveRDS(SEIR, "seir_model.rds")
 
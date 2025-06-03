########################################
## Diurnal Sin Fit #####################
########################################
## D.Hare
## modified 2024-01-19

##Helper Functions for therm_analysis_diurnal()##

#convert datetime to radians 
rad_hour <- function(dateTime){ #input date vector
  require(lubridate)
  dateTime <- as.POSIXlt(dateTime, "")
h <- hour(dateTime) + minute(dateTime)/60 #create decimal hour
  rad_h <- 2*pi*h/24
  
  return(rad_h)
}

# linearized sinsoid fit for either air or stream temperature 
fit_TDAS_hour <- function(dateTime, temp){
  df <- as.data.frame(temp) %>%
    cbind(., dateTime) %>%#has to be done second to keep format (?)
    dplyr::rename("temp" = 1)
  
  #convert to radian date for sinsoidal extract
  df$rday <- rad_hour(df$date)
  
  #to convert back to Phase hours
  units_hour <- 24
  
  #conduct linear fit to a sinsddial function 
  Tfit.lm <- lm(temp ~ sin(rday) + cos(rday), data = df)
  
  #extract equation for the fit 
  Tsin.lm <- coef(Tfit.lm)['sin(rday)']*sin(df$rday) +
    coef(Tfit.lm)['cos(rday)']*cos(df$rday) +
    coef(Tfit.lm)['(Intercept)']#T0 or mean
  
  Phase <- (units_hour/(2*pi))*(pi - atan(coef(Tfit.lm)['cos(rday)']/
                                            coef(Tfit.lm)['sin(rday)']))
  
  #Calculate Amplitude of the signal 
  Amp <- sqrt((coef(Tfit.lm)['sin(rday)']^2) + (coef(Tfit.lm)['cos(rday)']^2))
  
  #remove names to make single values
  names(Phase) <- NULL; names(Amp) <- NULL
  
  #create dataframe output summary data
  lmStats <- data.frame(amplitude_C = Amp,
                        phase_h = Phase,
                        AdjRsqr=summary(Tfit.lm)$adj.r.squared,
                        RMSE=sqrt(mean(resid(Tfit.lm)^2)),
                        sinSlope=coef(Tfit.lm)['sin(rday)'],
                        cosSlope=coef(Tfit.lm)['cos(rday)'],
                        YInt=coef(Tfit.lm)['(Intercept)'])#; rownames(lmStats) <- "Air" #would like site_no
  
  
  return(lmStats)
  
  
}

therm_analysis_diurnal <- function(df_tem){ 
  
  #Require this order to streamline all variable column names. 
  df_temp <- df_tem %>%
    dplyr::rename("site_id" =1, "date" = 2 , "tavg_wat_C"=3, "tavg_air_C" = 4)
  
  #make station ids factors for grouping
  df_temp$site_id <- as.factor(df_temp$site_id)
  
  #create list of each unique station id dataframe 
  df_temp_l <- split(df_temp, df_temp$site_id)%>%
    discard(., function(z) nrow(z) == 0) #remove any empty site_ids   
  
  #####----------Paired Air SW Temperature Annual Signal Analysis ----######## 
  
  ## Calculate Temperature Diel Signal Fit for both Air and Water Temperature 
  TAS_sin_fit <- lapply(names(df_temp_l), function(x){
    
    Tair_fit <- fit_TDAS_hour(df_temp_l[[x]]["date"], df_temp_l[[x]]["tavg_air_C"])
    Tair_fit$medium <- "air"
    Twat_fit <- fit_TDAS_hour(df_temp_l[[x]]["date"], df_temp_l[[x]]["tavg_wat_C"])
    Twat_fit$medium <- "water"
    
    #combine the water and air fits into one dataframe
    T_sin_fit <- rbind(Tair_fit, Twat_fit)
    T_sin_fit$site_id <- x
    
    return(T_sin_fit)
  }
  )%>%do.call("rbind", .)
  
}


### Add Fitted Value to the input Dataframe ###
fitted_sinValue_hour <- function(fit_TDAS_hour_output, df_tem){
  
  df_tem %>%
    mutate(sin_fit = (fit_TDAS_hour_output$sinSlope * sin(rad_hour(dateTime))) + 
             (fit_TDAS_hour_output$cosSlope * cos(rad_hour(dateTime))) + 
             fit_TDAS_hour_output$YInt)
  
}


#######################################
### Function ##########################
#Thermal Metric Daily Analysis for days. 
TMd_output <- function(T.y){

    T.y$date <- as.factor(lubridate::as_date(T.y$DateTime_r))
    T.yl <- lapply(levels(T.y$date), function(x){
      df.y <- T.y %>%
        dplyr::filter(date == x)%>%
        dplyr::select(-date)#remove to suit analysis requirements
      
      df.j <-therm_analysis_diurnal(df.y) ## left_join(therm_analysis_diurnal(df.y), data_gap_check(df.y), by = "site_id")
      
      df.j$date <- x # add date as a valuBe in table
      df.j$year <- year(df.j$date) # for interyear analysis
      
      df.j
    }) #end of lapply
    
    #Water Year
  } %>% do.call(rbind.data.frame, .)

    
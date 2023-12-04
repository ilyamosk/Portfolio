# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere) 
library(janitor)


# Read files into environment
dispensary_df <- read_csv("dispensary_master.csv") 
participant_df <- read_csv("participant_master.csv") 


str(dispensary_df)
str(participant_df)
#--------------------------------------------------------------------------------------------------
## Spatial Analysis Function Code Begins

spatial_analysis <- function(participant_df, dispensary_df, start_column_name, end_column_name, buffer_1=1000, buffer_2=2000, buffer_3=5000) {
  
  # Linking user inputs to objects within function 
  dispensary_df <- dispensary_df 
  participant_df <- participant_df
  
  # Initializing Columns
  participant_df$pd_buff_1 <- 0
  participant_df$pd_buff_2 <- 0
  participant_df$pd_buff_3 <- 0  
  participant_df$n_buff_1 <- 0
  participant_df$n_buff_2 <- 0
  participant_df$n_buff_3 <- 0 
  participant_df$min_dist_dispensary <- '' 
  participant_df$min_dist <- 0
  
  # I LOOP: for loop to iterate through each participant
  for (i in 1:nrow(participant_df)) {
    cat('On participant #', i, '\n') # this is the output which displays the loop progress in console
    p_lat <- participant_df$lat[i] # iterating through participant latitudes
    p_long <- participant_df$long[i] # iterating through participant longitudes
    p_sample <- as.Date(participant_df$enroll_date[i]) # iterating through participant enrollment dates
    start_date <- as.Date(participant_df[,start_column_name][[1]][i]) # iterating through participant enrollment dates
    end_date <- as.Date(participant_df[,end_column_name][[1]][i]) # iterating through participant enrollment dates
    
    # ----------------------------------------------------------------------------------------------------
    # RESETTING COLUMNS
    pd_buff_1 <- 0
    pd_buff_2 <- 0
    pd_buff_3 <- 0
    n_buff_1 <- 0
    n_buff_2 <- 0
    n_buff_3 <- 0
    min_dist_disp <- ''
    min_dist <- Inf
    
    # ----------------------------------------------------------------------------------------------------
    
    # J LOOP OPEN: nesting a second for loop to iterate through each dispensary in respect to each participant
    for (j in 1:nrow(dispensary_df)) {
      
      d_lat <- dispensary_df$lat[j] # iterating through dispensary latitudes
      d_long <- dispensary_df$long[j] # iterating through dispensary longitudes
      d_issue <- as.Date(dispensary_df$issue_date[j]) # iterating through dispensary issue date
      d_exp <- as.Date(dispensary_df$expiration_date[j]) # iterating through dispensary expiration date
      d_name <- dispensary_df$corporation_name[j] # iterating through dispensary expiration date
      
      
      # setting the ceiling date for the study
      if (d_exp > as.Date("2022-01-01")) { d_exp <- as.Date("2022-01-01") } 
      
      #-----------------------------------------------------------------------------------------------------
      # TEMPORALITY CONDITIONS
      
      #------------------------------------------------------
      # MIN DIST
      
      if ((d_issue <= start_date && end_date <= d_exp) || 
          (start_date <= d_issue && d_exp <= end_date) ||
          (d_issue <= start_date && d_exp <= end_date && start_date < d_exp) ||
          (start_date <= d_issue && end_date <= d_exp && d_issue < end_date)) { 
        
        calc_dist <- distm(c(p_long, p_lat), c(d_long, d_lat))
        if (calc_dist <= min_dist) {
          min_dist <- calc_dist
          min_dist_disp <- d_name
        } 
        
      }
     
      
      #---------------------------------------------------------------------------------------------------
      # 1 KM BUFFER
      # IF THE DISPENSARY IS OPEN THE WHOLE 1ST TRIMESTER
      if (d_issue <= start_date && end_date <= d_exp) { 
        # 1 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_1) {
          # Person time calculation using time_length and difftime
          pd_buff_1 <- pd_buff_1 + time_length(difftime(end_date,start_date), "days")
          n_buff_1 <- n_buff_1 + 1
        } 
      } 
      
      # IF THE DISPENSARY OPENS AND CLOSES WITHIN THE 1ST TRIMESTER
      else if (start_date <= d_issue && d_exp <= end_date) {
        # 1 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_1) { 
          # Person time calculation using time_length and difftime
          pd_buff_1 <- pd_buff_1 + time_length(difftime(d_exp, d_issue), "days")
          n_buff_1 <- n_buff_1 + 1
        } 
      } 
      
      # IF THE DISPENSARY OPENS BEFORE CONCEPTION AND CLOSES DURING THE 1ST TRIMESTER
      else if (d_issue <= start_date && d_exp <= end_date && start_date < d_exp) { 
        # 1 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_1) { 
          # Person time calculation using time_length and difftime 
          pd_buff_1 <- pd_buff_1 + time_length(difftime(d_exp, start_date), "days") 
          n_buff_1 <- n_buff_1 + 1
        } 
        
        # IF THE DISPENSARY OPENED DURING THE 1ST TRIMESTER AND CLOSES AFTER THE 1ST TRIMESTER
      } else if (start_date <= d_issue && end_date <= d_exp && d_issue < end_date) { 
        # 1 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_1) { 
          # Person time calculation using time_length and difftime 
          pd_buff_1 <- pd_buff_1 + time_length(difftime(end_date, d_issue), "days")
          n_buff_1 <- n_buff_1 + 1
        }
      }
      
      #---------------------------------------------------------------------------------------------------
      # 2 KM BUFFER
      
      # IF THE DISPENSARY IS OPEN THE WHOLE 1ST TRIMESTER
      if (d_issue <= start_date && end_date <= d_exp) { 
        # 2 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_2) { 
          # Person time calculation using time_length and difftime  
          pd_buff_2 <- pd_buff_2 + time_length(difftime(end_date, start_date), "days")
          n_buff_2 <- n_buff_2 + 1
        } 
        
        # IF THE DISPENSARY OPENS AND CLOSES WITHIN THE 1ST TRIMESTER
      } else if (start_date <= d_issue && d_exp <= end_date) { # If the dispensary opens and closes within the 1st trimester,
        # 2 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_2) {
          # Person time calculation using time_length and difftime 
          pd_buff_2 <- pd_buff_2 + time_length(difftime(d_exp, d_issue), "days")
          n_buff_2 <- n_buff_2 + 1
        } 
        
        # IF THE DISPENSARY OPENS BEFORE CONCEPTION AND CLOSES DURING THE 1ST TRIMESTER
      } else if (d_issue <= start_date && d_exp <= end_date && start_date < d_exp) { 
        # 2 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_2) { 
          # Person time calculation using time_length and difftime
          pd_buff_2 <- pd_buff_2 + time_length(difftime(d_exp, start_date), "days") 
          n_buff_2 <- n_buff_2 + 1
        } 
        
        # IF THE DISPENSARY OPENED DURING THE 1ST TRIMESTER AND CLOSES AFTER THE 1ST TRIMESTER
      } else if (start_date <= d_issue && end_date <= d_exp && d_issue < end_date) { 
        # 2 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_2) { 
          # Person time calculation using time_length and difftime
          pd_buff_2 <- pd_buff_2 + time_length(difftime(end_date, d_issue), "days") 
          n_buff_2 <- n_buff_2 + 1
        }
      }
      
      #---------------------------------------------------------------------------------------------------
      # 5 KM BUFFER
      
      # IF THE DISPENSARY IS OPEN THE WHOLE 1ST TRIMESTER
      if (d_issue <= start_date && end_date <= d_exp) { 
        # 5 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_3) { 
          # Person time calculation using time_length and difftime
          pd_buff_3 <- pd_buff_3 + (time_length(difftime(end_date, start_date), "days"))
          n_buff_3 <- n_buff_3 + 1
        } 
        
        # IF THE DISPENSARY OPENS AND CLOSES WITHIN THE 1ST TRIMESTER
      } else if (start_date <= d_issue && d_exp <= end_date) { 
        # 5 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_3) { # 1 km buffer
          # Person time calculation using time_length and difftime
          pd_buff_3 <- pd_buff_3 + time_length(difftime(d_exp, d_issue), "days")
          n_buff_3 <- n_buff_3 + 1
        } 
        
        # IF THE DISPENSARY OPENS BEFORE CONCEPTION AND CLOSES DURING THE 1ST TRIMESTER
      } else if (d_issue <= start_date && d_exp <= end_date && start_date < d_exp) { 
        # 5 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_3) { 
          # Person time calculation using time_length and difftime
          pd_buff_3 <- pd_buff_3 + (time_length(difftime(d_exp, start_date), "days"))
          n_buff_3 <- n_buff_3 + 1
        } 
        
        # IF THE DISPENSARY OPENED DURING THE 1ST TRIMESTER AND CLOSES AFTER THE 1ST TRIMESTER
      } else if (start_date <= d_issue && end_date <= d_exp && d_issue < end_date) { 
        # 5 km buffer
        if (distm(c(p_long, p_lat), c(d_long, d_lat)) <= buffer_3) { 
          # Person time calculation using time_length and difftime
          pd_buff_3 <- pd_buff_3 + (time_length(difftime(end_date, d_issue), "days"))
          n_buff_3 <- n_buff_3 + 1
        }
      }
    } # J loop closing bracket
    
    # ----------------------------------------------------------------------------------------------------# ADDING LOOP RESULTS BACK TO PARTICPANT DATAFRAME
    
    # Adding person days
    participant_df$pd_buff_1[i] <- pd_buff_1
    participant_df$pd_buff_2[i] <- pd_buff_2
    participant_df$pd_buff_3[i] <- pd_buff_3
    
    # Adding number of dispensaries open
    participant_df$n_buff_1[i] <- n_buff_1
    participant_df$n_buff_2[i] <- n_buff_2
    participant_df$n_buff_3[i] <- n_buff_3
    
    # min_dist_disp <- ''
    # min_dist <- Inf
    # Adding name and distance to nearest dispensary
    participant_df$min_dist_dispensary[i] <- min_dist_disp
    participant_df$min_dist[i] <- min_dist

  } # I loop closing bracket
  return (participant_df)
}

## Spatial Analysis Function Code Stops
#--------------------------------------------------------------------------------------------------

# Run
output_df <- spatial_analysis(participant_df, dispensary_df, 'two_weeks_before_enroll', 'enroll_date', 1000, 2000, 5000)

output_df <- output_df %>%
  arrange(desc(pd_buff_1))

# Create a CSV 
write_csv(output_df, # the name of the resulting dataframe
          'conception_minus_90_enroll_v2.csv') # the name of the resulting csv file


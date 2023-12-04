# Load Libraries
library(tidyverse)
library(lubridate)

# Sample data frame
df <- data.frame(
  business_id = c(1, 2, 3, 4, 5),
  address = c("3560 W Century Blvd", "2901 Los Feliz Blvd", "13463 Washington Blvd", "3560 W Century Blvd", "2901 Los Feliz Blvd"),
  start_date = as.Date(c("2022-01-01", "2022-03-15", "2022-05-01", "2022-06-01", "2022-07-01")),
  end_date = as.Date(c("2022-07-06", "2022-07-04", "2022-06-30", "2022-12-31", "2022-08-31"))
)

# My clean dates function
clean_dates <- function(input_df, address, start_date, end_date) {
  # Ensure the input data frame is not modified directly
  df <- input_df
  
  # Sort the data frame by address and start_date
  df <- df[order(df[, address], df[, start_date]), ]
  
  # Iterate over each row
  for (i in 2:nrow(df)) {
    # Check if the address is the same as the previous row
    if (df[, address][i] == df[, address][i-1]) {
      # Check for overlapping dates
      if (df[, start_date][i] <= df[, end_date][i-1]) {
        # Set end_date equal to start_date
        df[, start_date][i] <- df[, end_date][i-1] + days(1)
      }
    }
  }
  
  # Return the modified data frame
  return(df)
}

# Clean overlapping dates using the function with user-specified columns
df_cleaned <- clean_dates(df, address= "address", start_date= "start_date", end_date= "end_date")

# Print cleaned df
df_cleaned
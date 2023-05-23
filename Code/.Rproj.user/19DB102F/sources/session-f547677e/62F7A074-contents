library(readxl)

# Load the dataset
people_data <- read_excel("..\\Outliers.xlsx") 

# Outliers Detection 
boxplot(people_data)

library(dplyr)

# Loop through each column in the data frame
for (i in 1:ncol(people_data)) {
  
  # Check if the column has outliers
  if (sum(boxplot.stats(people_data[[i]])$out) > 0) {
    
    # Calculate upper and lower limits for the column
    q1 <- quantile(people_data[[i]], 0.30)
    q3 <- quantile(people_data[[i]], 0.75)
    iqr <- q3 - q1
    upper_limit <- q3 + 1.5 * iqr
    lower_limit <- q1 - 1.5 * iqr
    
    # Filter the data based on the limits
    people_data <- people_data %>% 
      filter(people_data[[i]] >= lower_limit & 
               people_data[[i]] <= upper_limit)
  }
}

# After Outliers Detection 
boxplot(people_data)

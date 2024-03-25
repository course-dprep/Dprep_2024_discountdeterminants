library(data.table) #for variable operationalization
library(ggplot2) #modelfree

#########################
###### Objective ########
#########################
# Input: scanner data, aggregate data
# Output: summary statistics, model free evidence plot

df <- fread("../data/df.csv")

df_bybrand_formats <- fread("../gen/data/df_aggregate.csv")
categorycharacteristic <- fread("../gen/data/categorycharacteristic.csv")
brandcharacteristic <- fread("../gen/data/brandcharacteristic.csv")

dir.create('../gen/audit')

##############################
###### Explore Raw Data ######
##############################

paste0("The number of household in dataset:",uniqueN(as.factor(df$cust)))
paste0("The total week period in dataset:",uniqueN(as.factor(df$week_nr)))
paste0("The number of brand in dataset:",uniqueN(as.factor(df$brand)))
paste0("The number of category in dataset:",uniqueN(as.factor(df$category)))
paste0("The number of format in dataset:",uniqueN(as.factor(df$format)))

summary_var <- df_bybrand_formats[, .(avg_regprice = round(mean(avgregprice), 2), 
                                      avg_finalprice = round(mean(avgfinalprice), 2), 
                                      avg_discount = round(mean(avgdiscount), 2),
                                      sd_regprice = round(sd(avgregprice), 2), 
                                      sd_finalprice = round(sd(avgfinalprice), 2), 
                                      sd_discount = round(sd(avgdiscount), 2)), 
                                  by = .(brand, format)]

summary_var <- summary_var[order(brand, format)]


#####################
######  Export ######
#####################
output_directory <- "../gen/audit/"

summary_output <- paste0(output_directory, "summary_stat.csv")
write.csv(summary_var, summary_output)

#Plot model-free (by brand and format)

  # Loop through each brand and create a plot
  unique_brands <- unique(df_bybrand_formats$brand)
  
  # Loop through each brand and create a plot
  for (brands in unique_brands) {
    # Subset the data for the current brand
    data_subset <- df_bybrand_formats[df_bybrand_formats$brand == brands, ]
    
    # Generate the plot for the current brand
    p <- ggplot(data_subset, aes(x = davgdiscount, y = dtotalvolume, color = format)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(title = paste("Effect of discounts on sales for brand", brands), 
           x = "Change of discounts", 
           y = "Change of volume sold", 
           color = "Format")
    
    # Define the filename based on the brand, including the directory path
    filename <- paste0(output_directory, "discounts_sales_brand_", gsub("[[:punct:]]| ", "_", tolower(brands)), ".png")
    
    # Save the plot to the specified file
    ggsave(filename, plot = p, width = 10, height = 8, dpi = 300)
  }




    # Loop through each format and create a plot
    unique_formats <- unique(df_bybrand_formats$format)
    for (formats in unique_formats) {
      # Subset the data for the current format
      data_subset <- df_bybrand_formats[df_bybrand_formats$format == formats, ]
      
      # Generate the plot for the current format
      p <- ggplot(data_subset, aes(x = davgdiscount, y = dtotalvolume, color = brand)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        theme_minimal() +
        labs(title = paste("Effect of discounts on sales in", formats), 
             x = "Change of discounts", 
             y = "Change of volume sold", 
             color = "Brand")
      
      # Define the filename based on the format, including the directory path
      filename <- paste0(output_directory, "discounts_sales_in_", gsub(" ", "_", tolower(formats)), ".png")
      
      # Save the plot to the specified file
      ggsave(filename, plot = p, width = 10, height = 8, dpi = 300)
    }
    
rm(list = ls())
gc()

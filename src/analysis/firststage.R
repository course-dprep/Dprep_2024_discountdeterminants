library(data.table) #for variable operationalization
library(ggplot2) #modelfree

#########################
###### Objective ########
#########################
# Input: Aggregate data, data regarding category and brand factors
# Output: Plot and Table of estimated discounts elasticities 


df_bybrand_formats <- fread("../../data/../gen/data/df_aggregate.csv")
categorycharacteristic <- fread("../../gen/data/categorycharacteristic.csv")
brandcharacteristic <- fread("../../gen/data/brandcharacteristic.csv")


#####################################
###### Elasticity Estimation ########
#####################################

#Run analysis by format
  # Function to perform linear regression and extract coefficients
  MM_lm <- function(df, selected_brand, selected_format) {
    lm_model <- lm(dtotalvolume ~ dLL + davgregprice + davgdiscount + davgcompLL + 
                     davgcompfinalprice + davgcompdiscount + totalvolume1 + LL1 +
                     avgregprice1 + avgdiscount1 + holiday + cop_avgregprice + cop_avgdiscount, 
                   data = df[brand == selected_brand & format == selected_format])
    
    coeffs <- summary(lm_model)$coefficients
    data.table(brand = selected_brand, format = selected_format, 
               Short_Coef_Discount = coeffs["davgdiscount", 1], 
               Short_Std_Discount = coeffs["davgdiscount", 2], 
               Long_Coef_Discount = coeffs["avgdiscount1", 1], 
               Long_Std_Discount = coeffs["avgdiscount1", 2])
  }
  
  # Initialize an empty data.table
  df_linear_withdiscount <- data.table(brand = character(), format = character(), 
                                       Short_Coef_Discount = numeric(), 
                                       Short_Std_Discount = numeric(), 
                                       Long_Coef_Discount = numeric(), 
                                       Long_Std_Discount = numeric())
  
  # Loop through each brand and format
  distinctformat<-unique(as.factor(df_bybrand_formats$format))
  distinctbrand<-unique(as.factor(df_bybrand_formats$brand))
  for (brand in distinctbrand) {
    for (format in distinctformat) {
      df_linear_withdiscount <- rbindlist(list(df_linear_withdiscount, MM_lm(df_bybrand_formats, brand, format)), use.names = TRUE)
    }
  }

#calculate elasticities short term effect by normalizing with brand sales
  df_elasticities <- df_linear_withdiscount[brandcharacteristic, on = .(brand, format)]
  
  df_elasticities[, `:=` (
    meanelasticity = `Short_Coef_Discount` * (avgbranddiscount / avgbrandweeksales) * 100,
    sdelasticity = `Short_Std_Discount` * (avgbranddiscount / avgbrandweeksales) * 100
  )]
  

#Merge with category characteristics
  #We will finally obtain data from first-stage
  df_firststage <- df_elasticities[categorycharacteristic, on = "format"]
  
  # Create a scatter plot for results
  df_firststage[, observation := .I]
  elasticity_plot <- ggplot(df_firststage, aes(x = observation, y = meanelasticity, color = brand, shape = format)) +
    geom_point(size = 4) +
    theme_minimal() +
    labs(x = "Observation Number", y = "Mean Elasticity", color = "Brand", shape = "Format")
  
  

#generate and store output
  dir.create('../../gen/firststage')
  write.csv(df_firststage,'../../gen/firststage/df_firststage.csv')
  ggsave('../../gen/firststage/plot_mean_elasticity.png', plot = elasticity_plot, width = 10, height = 8, dpi = 300)

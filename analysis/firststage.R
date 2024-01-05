library(data.table) #for variable operationalization
library(ggplot2) #modelfree


df <- fread("../data/df.csv")

#Assuming we are interested in category z
df <- df[category == "z",]

#Generate (market-weighted average) brand variables
df[, finalspending := regprice - discount]
df[, regpriceperunit := regprice/quantity]
df[, finalpriceperunit := finalspending /quantity]
df[, discountperunit :=  discount/quantity]

# Calculate weekly sales by brand
weeklysalesbybrand <- df[, .(totalsales = sum(quantity), LL = .N), by = .(week_nr,holiday, brand, format)]
  #LL can be calculated by uniqueN(product_id) to count distinct product id

# Merge df_complete with weeklysalesbybrand and calculate weights
df_weightcalc <- merge(df, weeklysalesbybrand, by = c("week_nr","holiday", "brand", "format"))
df_weightcalc[, wp := quantity/totalsales]

# Calculate weighted averages
df_weightcalc[, avgregpriceperunit := regpriceperunit * wp]
df_weightcalc[, avgfinalprice := finalpriceperunit * wp]
df_weightcalc[, avgdiscount := discountperunit * wp]

# Aggregate by week_nr, brand, and format
df_bybrand <- df_weightcalc[, .(totalvolume = sum(quantity), 
                                totalvalue = sum(finalspending), 
                                LL = mean(LL), 
                                avgregprice = sum(avgregpriceperunit), 
                                avgfinalprice = sum(avgfinalprice), 
                                avgdiscount = sum(avgdiscount)), by = .(week_nr,holiday, brand, format)]

rm(weeklysalesbybrand,df_weightcalc)


df_bybrand[, depth := avgdiscount/avgregprice]

#Calculate (market-weighted average) related variables of competitors and lag variable
  #We need for loop to construct competitor of each brand, specify further
  distinctformat<-unique(as.factor(df_bybrand$format))
  distinctbrand<-unique(as.factor(df_bybrand$brand))
  distinctweek<-unique(as.factor(df_bybrand$week_nr))
  
  nbrand<-length(distinctbrand)
  nweek<-length(distinctweek)
  nformat <- length(distinctformat)
  
  #Set up function for calculate lag variable
  lag_1 <- function(x, k = 1) head(c(rep(NA, k), x), length(x))
  
  #Function for copula correction term  following Park and Gupta 2012 see more https://github.com/hannesdatta/marketingtools
  make_copula <- function(x) {
    if (length(unique(x)) == 1) return(as.numeric(rep(NA, length(x))))
    return(ifelse(ecdf(x)(x) == 1, qnorm(1 - .0000001), qnorm(ecdf(x)(x))))
  }
  
  
  #We calculate (market-weighted average) competitors info for each brand

  gen_comp_lag_format <- function(df_bybrand, distinctbrand, format, nbrand, nweek) {
    results_list <- list()
    
    for (i in 1:length(distinctbrand)) {
      # Creating a copy of the relevant subset of df_bybrand
      df_own = copy(df_bybrand[brand == distinctbrand[i] & format == format])
      df_competitor = df_bybrand[brand != distinctbrand[i] & format == format]
      
      # create copula to mitigate potential endogeneity by format and brand
      df_own[, `:=` (
        cop_avgregprice = make_copula(avgregprice),
        cop_avgdiscount = make_copula(avgdiscount),
        cop_avgfinalprice = make_copula(avgfinalprice)
      )]
      
      
      # Calculate weekly sales by competing brand
      weeklysalesbycompetingbrand = df_competitor[, .(totalmarketvolume = sum(totalvolume)), by = .(week_nr)]
      # Calculate competitor weight and weighted averages
      df_competitorweightcalc = df_competitor[weeklysalesbycompetingbrand, on = "week_nr"]
      df_competitorweightcalc[, wp := totalvolume / totalmarketvolume]
      df_competitorweightcalc[, `:=` (
        avgcompLL = LL * wp,
        avgcompregprice = avgregprice * wp,
        avgcompfinalprice = avgfinalprice * wp,
        avgcompdiscount = avgdiscount * wp
      )]
      
      # Summarize competitor info
      df_competitorinfo = df_competitorweightcalc[, .(
        avgcompLL = sum(avgcompLL),
        avgcompregprice = sum(avgcompregprice),
        avgcompfinalprice = sum(avgcompfinalprice),
        avgcompdiscount = sum(avgcompdiscount)
      ), by = .(week_nr)]
      
      # Merge with own brand data
      df_own = df_own[df_competitorinfo, on = "week_nr"]
      
      # Add lagged variables
      df_own <- df_own[order(week_nr)] #Make sure that week_nr is ordered correctly before applying the lag
      lag_vars = c("totalvolume", "totalvalue", "LL", "avgfinalprice", "avgregprice", "avgdiscount", "avgcompLL", "avgcompfinalprice", "avgcompregprice", "avgcompdiscount")
      for (var in lag_vars) {
        df_own[, paste0(var, "1") := shift(get(var), 1, type = "lag"), by = .(brand, format)]
      }
      
      df_own <- na.omit(df_own)
      
      # Store the processed df_own in the results list
      results_list[[i]] <- df_own
    }
    
    # Combine all processed df_own into one data.table
    return(rbindlist(results_list, use.names = TRUE))
  }
  
  df_bybrand_formats <- gen_comp_lag_format(df_bybrand, distinctbrand, format, nbrand, nweek)
  
  # calculate first difference for estimation
  df_bybrand_formats[, `:=` (
    dtotalvolume = totalvolume - totalvolume1,
    davgfinalprice = avgfinalprice - avgfinalprice1,
    davgregprice  = avgregprice - avgregprice1,
    davgdiscount =avgdiscount - avgdiscount1,
    dLL = LL - LL1,
    davgcompLL = avgcompLL - avgcompLL1,
    davgcompfinalprice = avgcompfinalprice - avgcompfinalprice1,
    davgcompdiscount = avgcompdiscount - avgcompdiscount1
  )]

#Plot model-free (by brand and format)
  ggplot(df_bybrand_formats[brand == "A",], aes(x = davgdiscount, y = dtotalvolume, color = format)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    labs(title = "Effect of discounts on sales for brand A", x = "Change of discounts", y = "Change of volume sold", color = "Format")
  
  ggplot(df_bybrand_formats[format == "supermarket",], aes(x = davgdiscount, y = dtotalvolume, color = brand)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    labs(title = "Effect of discounts on sales in supermarket",x = "Change of discounts", y = "Change of volume sold", color = "Brand")

#Calculate Brand Level Factor  
  
  # Calculate weekly market by week
  weeklymarketbyweek <- df[, .(totalformatsales = sum(quantity), totalformatLL = .N), by = .(week_nr, format)]
  # Calculate weekly market share by brand
  weeklymarketsharebybrand <- df[, .(totalsales = sum(quantity)), by = .(week_nr, brand, format)]
  # Merge using data.table join
  weeklymarketsharemerge <- weeklymarketsharebybrand[weeklymarketbyweek, on = .(week_nr, format)]
  # Calculate brand market share format
  brandmarketshareformat <- weeklymarketsharemerge[, .(avgbrandweeksales = mean(totalsales), avgmarketshare = mean(totalsales / totalformatsales)), by = .(brand, format)]
  # Filter by brands with discounts
  df_bybrand_discount <- df_bybrand[avgdiscount > 0]
  # Calculate brand discount depth
  branddiscountdepth <- df_bybrand_discount[, .(avgbranddiscount = mean(avgdiscount), avgbranddiscountdepth = mean(avgdiscount / avgregprice)), by = .(brand, format)]
  
  # Filter by units with discounts
  df_discount <- df[discountperunit > 0]
  # Calculate LL discount
  LLdiscount <- df_discount[, .(LLdiscount = .N), by = .(week_nr, brand, format)]
  # Merge and calculate discount breadth
  LLdiscountmerge <- LLdiscount[df_bybrand_discount, on = .(week_nr, brand, format)]
  LLdiscountmerge[, discountbreadth := LLdiscount / LL]
  # Calculate brand discount breadth
  branddiscountbreadth <- LLdiscountmerge[, .(avgbranddiscountbreath = mean(discountbreadth)), by = .(brand, format)]
  # Filter for high discounts
  df_highdiscount <- df[discountperunit / regpriceperunit > 0.05]
  
  # Count high discounts
  counthighdiscount <- df_highdiscount[, .(SKUdiscount = .N), by = .(week_nr, brand, format)]
  counthighdiscount[, weekdisc := 1]
  
  # Calculate brand discount frequency
  branddiscountfrequency <- counthighdiscount[, .(frequency = sum(weekdisc)), by = .(brand, format)]
  
  # Merge to create brand characteristics
  brandcharacteristic <- merge(merge(merge(brandmarketshareformat, branddiscountdepth, by = c("format", "brand")), branddiscountbreadth, by = c("format", "brand")), branddiscountfrequency, by = c("format", "brand"))
  
  rm(weeklymarketsharebybrand, weeklymarketsharemerge, df_bybrand_discount, LLdiscount, LLdiscountmerge, df_highdiscount, counthighdiscount,brandmarketshareformat, branddiscountdepth,branddiscountbreadth,branddiscountfrequency)
  
  #Calculate Category Level Factor  

  # Calculate category depth
  categorydepth <- df_discount[, .(avgcatediscdept = mean(discountperunit/ regpriceperunit)), by = .(format)]
  
  # Calculate category deal
  categorydeal <- df_discount[, .(totaldiscvolume = sum(quantity)), by = .(week_nr, format)]
  
  # Calculate category total
  categorytotal <- df[, .(totalvolume = sum(quantity)), by = .(week_nr, format)]
  
  # Calculate category proportion using data.table join
  categoryproportion <- categorytotal[categorydeal, on = .(format, week_nr)]
  #categoryproportion[, totaldiscvolume := fcoalesce(totaldiscvolume, 0)] in case there is NA
  
  # Calculate category proportion format
  categoryproportionformat <- categoryproportion[, .(averagedealprop = mean(totaldiscvolume / totalvolume)), by = .(format)]
  
  # Calculate category breadth week
  categorybreadthweek <- df_discount[, .(Ncatediscount = .N), by = .(week_nr, format)]
  
  # Merge and calculate category breadth using data.table join
  categorybreadthweekmerge <- categorybreadthweek[weeklymarketbyweek, on = .(week_nr, format)]
  categorybreadth <- categorybreadthweekmerge[, .(avgcatediscbreath = mean(Ncatediscount / totalformatLL)), by = .(format)]
  
  # Calculate category competition structure
  categorycompstructure <- brandcharacteristic[, .(varmarketshare = var(avgmarketshare)), by = .(format)]
  
  # Combine all category characteristics
  categorycharacteristic <- Reduce(function(x, y) merge(x, y, by = "format"), list(categorydepth, categorybreadth, categoryproportionformat, categorycompstructure))
  
  # Optionally, remove intermediate variables
  rm(weeklymarketbyweek,categorydeal, categorytotal, categoryproportion, categoryproportionformat, categorydepth, categorybreadthweek, categorybreadthweekmerge, categorybreadth, categorycompstructure, df, df_bybrand,df_discount)


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
  ggplot(df_firststage, aes(x = observation, y = meanelasticity, color = brand, shape = format)) +
    geom_point(size = 4) +
    theme_minimal() +
    labs(x = "Observation Number", y = "Mean Elasticity", color = "Brand", shape = "Format")
  
  

#generate output for category z
  write.csv(df_firststage,'../gen/df_firststage.csv')

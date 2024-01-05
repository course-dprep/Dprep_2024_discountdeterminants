#Simulated Scanner Data
set.seed(1234567)

library(data.table)

brands = c("A","B","C","D","E")
formats = c("hypermarket","supermarket","convenience")
discounts_0 <- c(rep(0, times = 15), 1:15) 

n = 6000
# Create initial data.table
dt_scanner <- data.table(
  cust = sample(1:300, n, replace = TRUE),
  week_nr = sample(1:153, n, replace = TRUE),
  brand = sample(brands, n, replace = TRUE),
  format = sample(formats, n, replace = TRUE),
  category = "z"
)

# Generate regprice with a uniform distribution
dt_scanner[, regprice := runif(n, min = 15, max = 45)]

# Generate discount based on a distribution with mode at 0
discounts_0 <- c(rep(0, times = 15), 1:15) 
dt_scanner[, discount := sample(discounts_0, size = n, replace = TRUE)]

# Impose correlation:
# 1. Positive correlation between discount and quantity
# 2. Negative correlation between regprice and quantity
dt_scanner[, quantity := as.integer(1 + 2 * (discount > 0) - regprice / 45)]

# Ensure quantity is within a reasonable range
dt_scanner[quantity < 1, quantity := 1]
dt_scanner[quantity > 5, quantity := 5]

dt_holiday <- data.table(
  week_nr = 1:153,
  holiday = sample(c(0, 1), size = 153, replace = TRUE, prob = c(0.8, 0.2)) #20% of week is holiday
)

df <- merge(dt_scanner,dt_holiday, by=c("week_nr"))

write.csv(df,"df.csv", row.names = TRUE)
library(data.table) #for variable operationalization


df <- fread("../gen/df_firststage.csv")


#Assume brand C,E is private label
df[, PL := as.integer(brand %in% c('C', 'E'))]

#For consistency we multiplied all ratio to 100 so its easy to inpret to our coefficient

df[, `:=` (
  avgmarketshare = avgmarketshare * 100,
  avgbranddiscountdepth = avgbranddiscountdepth * 100,
  avgbranddiscountbreath = avgbranddiscountbreath * 100,
  avgcatediscdept = avgcatediscdept * 100,
  avgcatediscbreath = avgcatediscbreath * 100,
  averagedealprop = averagedealprop * 100,
  varmarketshare = varmarketshare * 100,
  format = as.factor(format),
  PL = as.factor(PL)
)]

#As we used only one category for this walkthrough, we can't include category factor as there is no variance or it perfectly collinear with store format
fixedSecondStageLS<- lm(meanelasticity ~ format+ avgbranddiscountdepth +avgbranddiscountbreath+ log(frequency) + PL, data = df, weights=(1/sdelasticity))

summary(fixedSecondStageLS)

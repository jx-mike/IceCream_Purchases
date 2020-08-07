#### Purchases of Ice Cream
ice = read.csv("ice_cream.csv")

## explore
names(ice)



## Full MODEL
## create a new variable for price per unit
priceper1 = (ice$price_paid_deal + ice$price_paid_non_deal) / ice$quantity
y <- log(1 + priceper1)
## collect some variables of interest
## create a data.frame
x <- ice[ ,c("flavor_descr", "size1_descr", "household_income", "household_size")]
## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr, "VAN")
## coupon usage
x$usecoup = factor(ice$coupon_value > 0)
x$couponper1 <- ice$coupon_value / ice$quantity
## organize some demographics
x$region <- factor(ice$region, levels=1:4, labels=c("East","Central","South","West"))
x$married <- factor(ice$marital_status==1)
x$race <- factor(ice$race, levels=1:4, labels=c("white", "black", "asian", "other"))
x$hispanic_origin <- ice$hispanic_origin==1
x$microwave <- ice$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- ice$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- ice$type_of_residence == 1
x$internet <- ice$household_internet_connection == 1
x$tvcable <- ice$tv_items > 1

## combine x and y
## cbind is "column bind".  It takes two dataframes and makes one.
xy <- cbind(x,y)

## EDA
househld_income_factor <- as.factor(xy$household_income)
house_inc_factor_df <- data.frame(househld_income_factor, priceper1)

# plot the data with a boxplot
boxplot(priceper1~househld_income_factor, data=house_inc_factor_df, main = "Price per unit vs Household Income",
        xlab = "Household income (Thousands of dollars)", ylab = "Price per Unit  (Dollars/unit)")
abline(h = mean(priceper1, data = house_inc_factor_df), col = "blue")

# graph price per unit vs household income
# turn household income into a factor and create a dataframe
househld_income_factor <- as.factor(xy$household_income)
house_inc_factor_df <- data.frame(househld_income_factor, priceper1)

# plot the data with a boxplot
boxplot(priceper1~househld_income_factor, data=house_inc_factor_df, main = "Price per unit vs Household Income",
        xlab = "Household income (Thousands of dollars)", ylab = "Price per Unit  (Dollars/unit)")
abline(h = mean(priceper1, data = house_inc_factor_df), col = "blue")

# plot the Price per unit vs coupon value per unit sold
tmp <- subset(xy, couponper1>0)
scatter.smooth(tmp$couponper1, exp(tmp$y-1), xlab = "Coupon Per Unit Sold", ylab = "Price Per Unit", main = "Price Per Unit vs Coupon Value Per Unit Sold")
cor(tmp$couponper1, tmp$y)

## Recommended MODEL
#library(tidyverse)
#ice$size1_descr = as.numeric(gsub("MLOZ", "", ice$size1_descr))
####### DEPENDENT VARIABLE
## create a new variable for price per unit oz
#priceper1oz = (ice$price_paid_deal + ice$price_paid_non_deal) / ice$quantity /ice$size1_descr
#y <- log(1 + priceper1oz)
#x <- ice[ ,c("size1_descr", "household_income", "household_size")]
#x$flavor_descr <- relevel(x$flavor_descr, "VAN")
#x$microwave <- ice$kitchen_appliances %in% c(1,4,5,7)
#x$dishwasher <- ice$kitchen_appliances %in% c(2,4,6,7)
#x$sfh <- ice$type_of_residence == 1
#x$internet <- ice$household_internet_connection == 1
#x$tvcable <- ice$tv_items > 1
#xy <- cbind(x,y)


## fit the regression
fit <- glm(y~., data=xy) 

## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column
pvals <- summary(fit)$coef[-1,4] 
summary(fit)

## source the fdr_cut function
source("fdr.R")

hist(pvals)

fdr(pvals, 0.05)



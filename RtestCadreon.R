###set this to the working directory of the saved files.
setwd("~/Documents/cadreon/")

install.packages("readxl")
install.packages("ggplot2")
install.packages("tidyr")

library(readxl)
library(ggplot2)
library(tidyr)

#Import the sales data into R:
salesData = read_excel("toy_sales_data.xlsx", sheet=1)
attach(salesData)

#Change to tidy-data:
tidySales = gather(salesData,  "Type", "Amount", 2:4)

#Create a plot of sales, TV and digital investment
ggplot(data= tidySales, aes(x=month, y=Amount/10^6, colour=Type))+
  geom_point()+
  geom_line()+
  ggtitle("Sales, TV investment and Digital investment vs. Time")+
  xlab("Time")+
  ylab("Amount ($ million) ")

####There does not appear to be any time trend in sales, except for a 
####positive linear growth. The behaviour of sales does appear positively
### correlated with both TV investment and Digital investment. 

cor(salesData[,2:4])
##                  sales  tv_spend digital_spend
##sales         1.0000000 0.4406862     0.6647654
##tv_spend      0.4406862 1.0000000     0.0720594
##digital_spend 0.6647654 0.0720594     1.0000000

####Sales is positively correlated with both types of investment. 
#### TV spending and digital spending do have some positive correlation, however is fairly small. Unlikely to cause a multicolinearity problem.  

#as.numeric(month)
##The month variable in R converts to an integer and represents time in seconds (the spacing
## between each month is different due to the different number of days in various months).
##The trend variable is an indicator for each month. We are only really interested in the monthly
##trend and so the "trend" variable is included and "month" omitted. 

#fitting a linear regression model to the data: use all data points
salesRegression = lm(sales ~ tv_spend + digital_spend + trend + xmas)
plot(salesRegression) #assumptions of linear regression appear to check out (random, normally dist errors)
summary(salesRegression)

#The adjusted R squared (adjusted for the number of variables in the model) is 0.8984.
#Therefore, approximately 90% of the variation in the sales data is explained by the model. 


#Assume a level of significance of 5%:
#TV spend regressor has an associated p-value of 2.58e-06 (<0.05) and therefore 
#there is statisitcally significant in the model (coefficient =/= 0). 

#Digital spend regressor has an associated p-value of 1.07e-05 and therefore 
#there is statisitcally significant in the model (coefficient =/= 0). 

#Trend has an associated p-value of 1.44e-06 and therefore there is statisitcally
#significant in the model (coefficient =/= 0). There is some growth over time. 

#xmas has an associated p-value of 0.00711 and therefore there is statisitcally
#significant in the model (coefficient =/= 0). X-mas effects sales positively.


#From this regression, for every dollar in TV_spend, sales increase by $2.026

#To find % change, fit a log-linear regression and interpret the coefficient of TV_spend
logSalesRegression = lm(log(sales) ~ tv_spend + digital_spend + trend + xmas)
summary(logSalesRegression)

#From this log-linear regression, for every dollar in TV_spend, sales increase by 0.00001957%. 

# ROI = (gains - investment cost)/investment cost  ($2.026 for every $1 in investment)
ROI = (2.026-1)/1*100   #102.6%

##plug in the values to the regression
plannedData = read_excel("toy_sales_data.xlsx", sheet=2)
plannedData$trend = c(25,26,27)  #continue the trend variable for the following months
plannedData$xmas = c(0,0,0)      #none of the prediction dates are xmas
predict.lm(salesRegression, plannedData)   #predicted sales

## The purpose of the model is to observe the return to investment of TV and digital ads 
## and to predict future sales. Therefore, any additional data that could explain sales
## would be beneficial to the model. This could include: any other important events other than xmas
## that could increase the sales of toys, prices/sales of competing toy brands or subsitution items 
## (such as video games), the number of stores selling the toy brand in each month.

## It is important to capture the impact of effects other than the advertising campaign to ensure the impact of the ads are not over estimated.

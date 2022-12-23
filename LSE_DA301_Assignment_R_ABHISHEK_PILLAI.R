## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

########################################################################
# SCENARIO

# Turtle Games is a game manufacturer and retailer who manufacture and 
# sell their own products, along with sourcing and selling products 
# manufactured by other companies. Their product range includes books, 
# board games, video games and toys. They have a global customer base and 
# have a business objective of improving overall  sales performance by
# using customer trends.

# In particular, Turtle Games wants to understand:

#    - what is the impact on sales per product 
#      (Week 4)
#    - the reliability of the data 
#      (e.g. normal distribution, Skewness, 
#       Kurtosis) (Week 5)
#    - if there is any possible relationship(s) 
#      in sales between North America,
#      Europe, and global sales (Week 6).

#########################################################################

# Week 4 Assignment: EDA Using R

# The sales department of Turtle games prefers R to Python. Therefore, 
# exploration and preparation of the data set for analysis will be 
# completed by using basic statistics and plots. 

#########################################################################

# 1. Load and explore the data.

# Check the working directory.
getwd()

# Install and import Tidyverse.
library(tidyverse)

# Import the 'turtle_sales.csv' data set to the 
# global environment.
data_TS <- read.csv("turtle_sales.csv", header = TRUE, sep = ",")
                    
# Print the data frame.
view(data_TS)
as_tibble(data_TS)

# Remove unnecessary columns (Ranking, Year, Genre and Publisher).
data_TS_2 <- subset(data_TS, select = -c(Ranking,Year, Genre, Publisher))

# View the new dataframe (data_TS_2)
view(data_TS_2)


# View the descriptive statistics of data_TS_2a.
stats_data_TS_2 = summary(data_TS_2)
view(stats_data_TS_2)

# Check for NA values in the data set
sum(is.na(data_TS_2))

# Check the spread of the 3 Sales columns to 
# determine whether there are any outlier values.

# NA_Sales Outlier Check.
NA_Out_Box = qplot(x = NA_Sales, data=data_TS_2, geom='boxplot')
NA_Out_Box

# Remove Outliers in NA_Sales.
Q1_NA <- quantile(data_TS_2$NA_Sales, .25)
Q3_NA <- quantile(data_TS_2$NA_Sales, .75)
IQR_NA <- IQR(data_TS_2$NA_Sales)

data_TS_2a <- subset(data_TS_2, data_TS_2$NA_Sales > 
                    (Q1_NA - 1.5*IQR_NA) & data_TS_2$NA_Sales < 
                    (Q3_NA + 1.5*IQR_NA))


# EU_Sales Outlier Check.
EU_Out_Box = qplot(x = EU_Sales, data=data_TS_2a, geom='boxplot')
EU_Out_Box

# Remove Outliers in EU_Sales.
Q1_EU <- quantile(data_TS_2a$EU_Sales, .25)
Q3_EU <- quantile(data_TS_2a$EU_Sales, .75)
IQR_EU <- IQR(data_TS_2a$EU_Sales)

data_TS_2b <- subset(data_TS_2a, data_TS_2a$EU_Sales > 
                    (Q1_EU - 1.5*IQR_EU) & data_TS_2a$EU_Sales < 
                    (Q3_EU + 1.5*IQR_EU))

# Global_Sales Outlier Check.
Global_Out_Box = qplot(x = Global_Sales, data=data_TS_2b, geom='boxplot')
Global_Out_Box

# Remove Outliers in Global_Sales.
Q1_Global <- quantile(data_TS_2b$Global_Sales, .25)
Q3_Global <- quantile(data_TS_2b$Global_Sales, .75)
IQR_Global <- IQR(data_TS_2b$Global_Sales)

data_TS_2c <- subset(data_TS_2b, data_TS_2b$Global_Sales >
                    (Q1_Global - 1.5*IQR_Global) 
                    & data_TS_2b$Global_Sales < 
                    (Q3_Global + 1.5*IQR_Global))

#########################################################################

# 2. Review plots to determine insights into the data set.

# 2a) Create scatterplot to show sale variation in North America.
#     x = Product, y = NA_Sales, Data Source = data_TS_2d.
NA_Plot = qplot(Product, NA_Sales, data=data_TS_2c, 
                geom = c("point", "smooth"))
NA_Plot

#     Create scatterplot to show sale variation in Europe.
#     x = Product, y = EU_Sales, Data Source = data_TS_2d   
EU_Plot = qplot(Product, EU_Sales, data=data_TS_2c, 
                geom = c("point", "smooth"))
EU_Plot

#     Create scatterplot to show sale variation globally.
#     x = product, y = Global_Sales, Data Source = data_TS_2d.
Global_Plot = qplot(Product, Global_Sales, data=data_TS_2c,
                    geom = c("point", "smooth"))
Global_Plot

# 2b) Create a histogram to view the sales variation in North America. 
#     x = NA_Sales, Data Source = data_TS_2d.
NA_Hist = qplot(NA_Sales, fill = Platform, data=data_TS_2c, 
                geom='histogram', binwidth = 1, col = I('black'))
NA_Hist

#     Create a histogram to view the sales variation in Europe. 
#     x = EU_Sales, Data Source = data_TS_2d.
EU_Hist = qplot(EU_Sales, fill = Platform, data=data_TS_2c,
                geom='histogram',binwidth = 1, col = I('black'))
EU_Hist

#     Create a histogram to view the sales variation globally. 
#     x = Global_Sales, Data Source = data_TS_2c.
Global_Hist = qplot(Global_Sales,fill = Platform, data=data_TS_2c,
                    geom='histogram', binwidth = 1, col = I('black'))
Global_Hist

# 2c) Create a boxplot to view the sales variation for North America.
#     x = NA_Sales, y = Platform, Data source = data_TS_2d
NA_Box = qplot(Product,NA_Sales, data=data_TS_2c, colour=I('blue'), 
               geom='boxplot')
NA_Box

#     Create a boxplot to view the sales variation for Europe
#     x = EU_Sales, y = Platform, Data Source = data_TS_2d
EU_Box = qplot(Product, EU_Sales, data=data_TS_2c, colour=I('red'),
               geom='boxplot')
EU_Box

#     Create a boxplot to view the sales variation globally.
#     x = Global_Sales, y = Platform, Data source = data_TS_2d
Global_Box = qplot(Product, Global_Sales, colour=I('purple'), 
                   data=data_TS_2c, geom='boxplot')
Global_Box

###############################################################################

# 3. OBSERVATIONS AND INSIGHTS

# I. Data has been cleaned to remove all outliers.

# II. All 3 Sales vs. Product ID scatterplots show a trend that as
#     product ID increases, the sales decrease.

# III. The most frequent NA_Sales value lies within the 0 - 0.5 NA_Sales 
#      bin, with the general trend being that the higher NA_Sale values 
#      tend to have less frequent ocurrences, with only exception being 
#      between the 0.5 - 1.5 & 1.5 - 2.5 NA_Sales bins, where a slight 
#      increase is observed.Significant decreases are observed between 
#      the 0 - 0.5 & 0.5 - 1.5 NA_Sales bins as well as the 2.5 - 3.5 & 
#      3.5 - 4.5 bins. Max NA_Sales bin observed = 6.5 - 7.5.

# IV. The most frequent EU_Sales value lies between 0 - 0.50 EU_Sales 
#     bin,with the trend being that the higher EU_Sale Values tend to 
#     have less frequent ocurrences, with a significant decrease observed 
#     between the 1.5 - 2.5 & 2.5 - 3.5 bins. Max EU_Sales bin observed
#     = 3.5 - 4.5.

# IV. The most frequent Global_Sales value lies between the 0 - 0.50 
#     Global_Sales bin, with a periodic decreasing trend observed between 
#     the sales value and the count, with there being a decreasing trend 
#     between global sales values of 0 - 3.5, 3.5 - 7.5 & 7.5 - 13.5, but 
#     a significant increase in the count can be seen between the 
#     2.5 - 3.5 & 3.5 - 4.5 Global_Sales bins in adition to observing a 
#     slight increase between the 6.5 - 7.5 & 7.5 - 8.5 Global_Sale bins. 
#     However, significant decreases in  the count occurs between the 
#     0.5 - 1.5 & 1.5 - 2.5 bins as well as between the 5.5 - 6.5 & 
#     6.5 - 7.5 bins. Max Global_Sales bin observed = 12.5 - 13.5.


#########################################################################
#########################################################################

# Week 5 assignment: Cleaning and manipulating data using R

# R will be used to explore, prepare and explain the normality of the 
# data set based on plots, Skewness Kurtosis, and a Shapiro-Wilk test. 

#########################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
view(data_TS_2c)
dim(data_TS_2c)

# Create and view a subset dataframe containing only
# sales data.
sales_df = data_TS_2c[,-1:-2]
view(sales_df)

# Check output: Determine the min, max and mean values of the sales data.
min_sales = apply(sales_df, 2, min)
max_sales = apply(sales_df, 2, max)
mean_sales = apply(sales_df, 2, mean)

min_sales
max_sales
mean_sales

# View the descriptive statistics.
stats_sales = summary(data_TS_2c)
view(stats_sales)

#########################################################################

# 2. Determine the impact on sales per product_id.

# 2a) Use the group_by function to group the data by Product and 
#     determine the sum per Product.
library(dplyr)

Product_Sales = data_TS_2c %>% group_by(Product) %>%
                summarise(NA_Sales_Sum = sum(NA_Sales),
                          EU_Sales_Sum = sum(EU_Sales),
                          Global_Sales_Sum = sum(Global_Sales),
                         .groups = 'drop')

# View the Product_Sales data frame.
view(Product_Sales) 

# Explore the Product_Sales data frame.
dim(Product_Sales)
Product_Sales_Stats = summary(Product_Sales)
view(Product_Sales_Stats)

# 2b) Create Plots to review and determine insights

# Import ggplot2
library(ggplot2)
#    i) Create Scatterplots using ggplot2.

#       NA_Sales.
NA_Plot_2 = ggplot (data = Product_Sales, 
                    mapping=aes(x = Product, y = NA_Sales_Sum)) + 
            geom_point(color = 'red', alpha = 0.5,  size = 1.5) +
            geom_smooth(method = 'lm',se=FALSE, size=1) +
            labs(x = "Product ID", y = "North America Sales Sum",
                 title = "NA Sales Sum Scatterplot")
NA_Plot_2
        
#       EU_Sales.
EU_Plot_2 = ggplot (data = Product_Sales, 
                    mapping=aes(x = Product, y = EU_Sales_Sum)) + 
            geom_point(color = 'red', alpha = 0.5, size = 1.5) +
            geom_smooth(method = 'lm',se=FALSE, size=1) +
            labs(x = "Product ID", y = "Europe Sales Total",
                 title = "Europe Sales Sum Scatterplot")
EU_Plot_2

#       Global_Sales.
Global_Plot_2 = ggplot (data = Product_Sales, 
                        mapping=aes(x = Product,y = Global_Sales_Sum)) + 
                geom_point(color = 'red', alpha = 0.5, size = 1.5) +
                geom_smooth(method = 'lm',se=FALSE, size=1) +
                labs(x = "Product ID", y = "Global Sales Total",
                     title = "Global Sales Sum Scatter")
Global_Plot_2

#    ii) Create Histograms using ggplot2.

#        NA_Sales.
NA_Hist_2 <- ggplot(Product_Sales, aes(x=NA_Sales_Sum)) + 
             geom_histogram(binwidth = 1, color="black", fill="green") + 
             labs(x = "North American Sales Sum",
                  y = "Count of Sales",
                  title = "NA Sales Sum Histogram")
NA_Hist_2

#        EU_Sales.
EU_Hist_2 <- ggplot(Product_Sales, aes(x=EU_Sales_Sum)) + 
             geom_histogram(binwidth = 1, color="black", fill="cyan") + 
             labs(x = "European Sales Sum",
                  y = "Count of Sales",
                  title = "EU Sales Sum Histogram")
EU_Hist_2

#        Global_Sales.
Global_Hist_2 <- ggplot(Product_Sales, aes(x=Global_Sales_Sum)) + 
                 geom_histogram(binwidth = 1, color="black", 
                                fill="magenta") +
                 labs(x = "Global Total Sales",
                      y = "Count of Sales",
                   title = "Global Total Sales Histogram")
Global_Hist_2

#    iii) Create Boxplots using ggplot2.
         
#        NA_Sales.
NA_Box_2 <- ggplot(Product_Sales, aes(x=Product, y=NA_Sales_Sum)) + 
            geom_boxplot() +
            geom_boxplot(outlier.colour="red", outlier.shape=8,
                         outlier.size=4) +
            labs(x= 'Product', y = "North America Sales Sum", 
                 title = "NA Sales Box")
NA_Box_2

#        EU_Sales.
EU_Box_2 <- ggplot(Product_Sales, aes(x = Product, y=EU_Sales_Sum)) + 
            geom_boxplot() +
            geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)+
            labs(x = "Product ID", y = 'EU Sales Sum',
                 title = "Europe Total Sales Box")
            
EU_Box_2

#        Global_Sales.
Global_Box_2 <- ggplot(Product_Sales, aes(x=Product, y = Global_Sales_Sum)) + 
                geom_boxplot() + 
                geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4) +
                labs(x = "Product ID", y = "Global Sales Sum",
                     title = "Global Sale Sum Box")
Global_Box_2

########################################################################

# 3. Determine the normality of the data set.

# 3a) Create Q-Q Plots For the sales data.

#     Store the necessary data in the variable my_data
Product_Sales_Data <- Product_Sales

#     Q - Q Plot for NA_Sales
 qqnorm(Product_Sales_Data$NA_Sales_Sum)
 qqline(Product_Sales_Data$NA_Sales_Sum, col = 'red')

#     Q - Q Plot for EU_Sales
qqnorm(Product_Sales_Data$EU_Sales_Sum)
qqline(Product_Sales_Data$EU_Sales_Sum, col = 'red')

#     Q - Q Plot for Global_Sales
qqnorm(Product_Sales_Data$Global_Sales_Sum)
qqline(Product_Sales_Data$Global_Sales_Sum, col = 'red')

# 3b) Perform Shapiro-Wilk test

#     Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test on NA_Sales
NA_SWT = shapiro.test((Product_Sales_Data$NA_Sales_Sum))
NA_SWT

# Perform Shapiro-Wilk test on EU_Sales.
EU_SWT = shapiro.test((Product_Sales_Data$EU_Sales_Sum))
EU_SWT

# Perform Shapiro-Wilk test on Global_Sales.
Global_SWT = shapiro.test((Product_Sales_Data$Global_Sales_Sum))
Global_SWT

## 3c) Determine Skewness and Kurtosis.

#      Skewness and Kurtosis of NA_Sales.
NA_Skew = skewness(Product_Sales_Data$NA_Sales_Sum)
NA_Skew

NA_Kurtosis = kurtosis(Product_Sales_Data$NA_Sales_Sum)
NA_Kurtosis

#      Skewness and Kurtosis of EU_Sales.
EU_Skew = skewness(Product_Sales_Data$EU_Sales_Sum)
EU_Skew

EU_Kurtosis = kurtosis(Product_Sales_Data$EU_Sales_Sum)
EU_Kurtosis

#      Skewness and Kurtosis of Global_Sales.
Global_Skew = skewness(Product_Sales_Data$Global_Sales_Sum)
Global_Skew

Global_Kurtosis = kurtosis(Product_Sales_Data$Global_Sales_Sum)
Global_Kurtosis

# 3d) Determine correlation

#     Check correlation between the NA and EU sales columns
NA_EU_Cor = cor(Product_Sales_Data$NA_Sales_Sum, 
                Product_Sales_Data$EU_Sales_Sum)
NA_EU_Cor

#     Check correlation between the NA and Global sales columns
NA_Global_Cor = cor(Product_Sales_Data$NA_Sales_Sum, 
                    Product_Sales_Data$Global_Sales_Sum)
NA_Global_Cor

#     Check correlation between the EU and Global sales columns
EU_Global_Cor = cor(Product_Sales_Data$EU_Sales_Sum, 
                    Product_Sales_Data$Global_Sales_Sum)
EU_Global_Cor

#########################################################################

# 4. Plot the data
#    Create plots to gain insights into data.

#    Scatterplots comparing product ID and NA, EU & Gloabal total sales.
library(ggpubr)
Sales_Plots = ggarrange(NA_Plot_2, EU_Plot_2, Global_Plot_2)
Sales_Plots

#    Histograms comparing the frequency of NA, EU and global total Sales.
Sales_Hists = ggarrange(NA_Hist_2, EU_Hist_2, Global_Hist_2)
Sales_Hists

#    Boxplots comparing the descriptive stats of NA, EU and global total.
#    sales
Sales_Boxes = ggarrange(NA_Box_2, EU_Box_2, Global_Box_2)
Sales_Boxes

# KDP to compare the density of NA, EU and Global total sales

# NA Sales
NA_KDP = ggplot(Product_Sales_Data, aes(x = NA_Sales_Sum)) +
         geom_density(color='blue') + 
         labs(title = "North American Total Sales Density")
NA_KDP

# EU Sales
EU_KDP = ggplot(Product_Sales_Data, aes(x = EU_Sales_Sum)) +
         geom_density(color='magenta') + 
         labs(title = "European Total Sales Density")
EU_KDP

# Global Sales
Global_KDP = ggplot(Product_Sales_Data, aes(x = Global_Sales_Sum)) +
             geom_density(color='brown') + 
             labs(title = "Global Total Sales Density")
Global_KDP
#########################################################################

# 5. OBSERVATIONS AND INSIGHTS

# I. All 3 total sales columns show a decreasing trend as Product ID 
#    increases
#
# II. The 'NA Sales Sum Histogram' follows a bell shape curve depicting a
#     right skew, with the peak of the North American Total Sales being
#     within the 2.5 - 3.5 NA Sales Sum bin.This shape is also observed
#     for the EU Total Sales histogram, with the EU Sales Sum Histogram
#     having its peak within the 1.5 - 2.5 EU Sales Sum bin.
#
#  III. The shape of the 'Global Sales Sum Histogram' also follows a 
#       similar shape to the other 2 histograms for the majority of the 
#       sales sum Values obtained, but there are unexpected increases 
#       between the 7.5 - 8.5 & 8.5 - 9.5 + 11.5 - 12.5 & 12.5 - 13.5 
#       Global Sales Sum bins. Note that the modal global total sales
#       lies within the 4.5 - 5.5 Global Sales Sum bin.
#       Max bin observed for NA Sales Sum = 11.5 - 12.5.
#       Max bin observed for EU Sales Sum = 9.5 - 10.5.
#       Max bin observed for EU Sales Sum = 21.5 - 22.5.
#
# IV. NA Total Sales Box: Lower Quartile = 2.375
#                         Upper Quartile = 4.625
#                                   Mean = 3.125
#                                    Min = 0.000
#                                    Max = 7.625
#
#  V. EU Total Sales Box: Lower Quartile = 1.250
#                         Upper Quartile = 3.250
#                                   Mean = 2.125
#                                    Min = 0.000
#                                    Max = 5.375
#
# VI. Global Total Sales Box: Lower Quartile = 5.000
#                             Upper Quartile = 9.750
#                                       Mean = 7.000
#                                        Min = 0.000
#                                        Max = 15.75
#  
# VII. All 3 Q-Q Plots show that the sales sum data possess a degree 
#      of right skew. Additionally, the Shapiro - Wilko tests for all 3 
#      sales sum data result in p values less than 0.05 so all 3 sales 
#      sum data can be concluded to have a distribution which is 
#      significantly different to that of a normal distribution.
#
# VIII. The skew values for each total sales sum data is positive, 
#       indicating that each sales data used has right skew.Additionally, 
#       the kurtosis values for all 3 total sales data is greater than 3, 
#       suggesting the data is leptokurtic.
#
# IX. There is moderate positive correlation between the European and
#     North American Sales Sum Data column, with there being strong
#     positive corelation displayed between the Global Sales Sum & 
#     European Sales Sum + Global Sales Sum & North American Sales Sum.

#########################################################################
#########################################################################

# Week 6 assignment: Making recommendations to the business using R

# The sales department wants to better understand if there is any 
# relationship between North America, Europe, and global sales. 
# Therefore, an investigation into any possible relationship(s) in the 
# sales data is to be undertaken by creating a simple and multiple linear
# regression model. Based the models and the previous analysis,
# recommendations will then be provided to Turtle Games based on:
#   - Is there confidence in the models based on goodness of fit and
#     accuracy of predictions?
#   - What suggestions and recommendations are there for the business?
#   - If needed, how would the model(s) be improved?
#########################################################################
#########################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
view(Product_Sales)

# Determine a summary of the data frame.
Product_Sales_Stats = summary(Product_Sales)
view(Product_Sales_Stats)

#########################################################################

# 2. Create a simple linear regression model

# 2a) Determine the correlation between columns
Sales_Cor = cor(Product_Sales)
Sales_Cor

# 2b) Create a plot (simple linear regression)

#     I. NA_Sales Sum Vs. Global Sales Sum Linear Regression Analysis

#        Basic Visualisation.
plot(Product_Sales$NA_Sales_Sum, Product_Sales$Global_Sales_Sum)

#        Create a model with only one x variable.
NA_Global_Model <- lm(Global_Sales_Sum~NA_Sales_Sum, data=Product_Sales)
NA_Global_Model

#        View the full regression table.
NA_Global_RT = summary(NA_Global_Model)
NA_Global_RT

#        View residuals on a plot.
plot(NA_Global_Model$residuals)

#        Plot the relationship with base R graphics.
plot(Product_Sales$NA_Sales_Sum, Product_Sales$Global_Sales_Sum)
coefficients(NA_Global_Model)

#        Add line-of-best-fit.
abline(coefficients(NA_Global_Model), col=c("red"))


#    II. EU_Sales Vs. Global_Sales Sum Linear Regression Analysis

#        Basic Visualisation.
plot(Product_Sales$EU_Sales_Sum, Product_Sales$Global_Sales_Sum)

#        Create a model with only one x variable.
EU_Global_Model <- lm(Global_Sales_Sum~EU_Sales_Sum, data=Product_Sales)
EU_Global_Model

#        View the full regression table.
EU_Global_RT = summary(EU_Global_Model)
EU_Global_RT

#        View residuals on a plot.
plot(EU_Global_Model$residuals)

#        Plot the relationship with base R graphics.
plot(Product_Sales$EU_Sales_Sum, Product_Sales$Global_Sales_Sum)
coefficients(EU_Global_Model)

#        Add line-of-best-fit.
abline(coefficients(EU_Global_Model), col=c("red"))

#   III. NA_Sales Sum Vs. EU_Sales Sum Basic Visualisation.

#        Basic Visualisation.
plot(Product_Sales$NA_Sales_Sum, Product_Sales$EU_Sales_Sum)

#        Create a model with only one x variable.
EU_NA_Model <- lm(EU_Sales_Sum~NA_Sales_Sum, data=Product_Sales)
EU_NA_Model

#        View the full regression table.
EU_NA_RT = summary(EU_NA_Model)
EU_NA_RT

#        View residuals on a plot.
plot(EU_NA_Model$residuals)

#        Plot the relationship with base R graphics.
plot(Product_Sales$NA_Sales_Sum, Product_Sales$EU_Sales_Sum)
coefficients(EU_NA_Model)

#        Add line-of-best-fit.
abline(coefficients(EU_NA_Model), col=c("red"))

#########################################################################

# 3. Create a multiple linear regression model

#    Select only numeric columns from the original data frame.
Product_Sales_2 = subset(Product_Sales, select = -c(Product))
str(Product_Sales_2)
view(Product_Sales_2)

#    Multiple linear regression model.
Sales_Model = lm(Global_Sales_Sum~NA_Sales_Sum + EU_Sales_Sum, 
                 data=Product_Sales_2)
Sales_Model
#########################################################################

# 4. Predictions based on given values

# Create a new object and specify the predict function.
Predict_Sales = predict(Sales_Model, newdata = Product_Sales_2,
                        interval='confidence')

# View the object.
view(Predict_Sales)

#########################################################################

# 5. OBSERVATIONS AND INSIGHTS

# I. NA_Sales_Sum = 3.66, EU_Sales_Sum = 1.54: Predicted Global_Sales_Sum
#    = 6.79, Actual Global_Sales_Sum = 7.40. This actual value exceeds
#    that of the predicted upper limit which is 7.00
#
# II. NA_Sales_Sum = 2.73, EU_Sales_Sum = 0.65: 
#     Predicted Global_Sales_Sum = 4.85, Actual Global_Sales_Sum = 4.32 
#     This actual value is lower than that of the predicted lower limit,
#     which is 4.60
#
# III. NA_Sales_Sum = 4.42, EU_Sales_Sum = 0.97: 
#      Predicted Global_Sales_Sum = 6.87, Actual Global_Sales_Sum = 6.12 
#      This actual value is lower than that of the predicted lower limit,
#      which is 6.59
#
# IV. NA_Sales_Sum = 11.09, EU_Sales_Sum = 6.66: 
#     Predicted Global_Sales_Sum = 19.95, Actual Global_Sales_Sum = 20.58 
#     This actual value lies within the confidence interval stated
#
# V. NA_Sales_Sum = 2.63, EU_Sales_Sum = 10.17: 
#    Predicted Global_Sales_Sum = 15.76, Actual Global_Sales_Sum = 15.59 
#    This actual value lies within the confidence interval stated.

#########################################################################
#########################################################################





library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(stats)
library(knitr)
library(ggpubr)
library(broom)
library(data.table)
library(moments)
library(GGally)
library(PerformanceAnalytics)

#Read NCSS sales list in CSV format
AllSales <- read.csv("Area72NCSSData.csv")

#Filter out RemBefore
AllSales_RemBefore <- AllSales %>%
  filter(AllSales$RemBefore == "0")

#Filter out RemDuring
GoodSales <- AllSales_RemBefore %>%
  filter(AllSales_RemBefore$RemDuring == "0")

#create table with only necessary columns - independend variable and chosen dependent variables
necessary_data <- GoodSales %>%
  select(LnTrendedPrice, BaseLandC, ConditionC, GradeC, TotalRcnC, KingCountyYN,
         Plat320493YN, Plat736630YN, Plats29440X, Plats76952X,
         Sub8YN, Nghb11_12_YN, AgeC_Ren_Cubed)

#Descriptive Statistics
#Create standard error function
std <- function(x) sd(x)/sqrt(length(x))

#Display descriptive statistics of model
descriptive_stats <- data.table("Variable" = c(colnames(necessary_data)))
descriptive_stats$Count <- count(necessary_data)
descriptive_stats$Mean <- apply(necessary_data, 2, mean)
descriptive_stats$Standard.Deviation <- apply(necessary_data, 2, std)
descriptive_stats$Minimum <- apply(necessary_data, 2, min)
descriptive_stats$Maximum <- apply(necessary_data, 2, max)

print(descriptive_stats, digit = 5)

#Create variables for computing the multiple regression processes
independent_var <-necessary_data[,1]
dependent1 <- necessary_data[,2]
dependent2 <- necessary_data[,3]
dependent3 <- necessary_data[,4]
dependent4 <- necessary_data[,5]
dependent5 <- necessary_data[,6]
dependent6 <- necessary_data[,7]
dependent7 <- necessary_data[,8]
dependent8 <- necessary_data[,9]
dependent9 <- necessary_data[,10]
dependent10 <- necessary_data[,11]
dependent11 <- necessary_data[,12]
dependent12 <- necessary_data[,13]

#Apply multiple regression
model <- lm(independent_var ~ dependent1 + dependent2 + dependent3 +
              dependent4 + dependent5 + dependent6 + dependent7 + dependent8 +
              dependent9 + dependent10 + dependent11 + dependent12)

summary(model)

#Show Estimated equation
model_equation <- function(model, ...) {
  format_args <- list(...)
  
  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)}

model_equation(model) #Returns estimated equation in the usual form

#Correlation Matrix
correlation_matrix <- ggcorr(necessary_data, label = TRUE, layout.exp = 1, nbreaks = 5, palette = "RdBu")

print(correlation_matrix)

#Regression Coefficients T-Tests
coefficient <- as.data.frame(summary(model)$coefficient) #saving part of the summary table as a data frame

coefficient$Reject <- "Yes" #Adding a 'Reject H0 at 1%?' column
coefficient$Reject[coefficient$'Pr(>|t|)' > 0.05] <- "No"

coefficient$Power <- "1.0000"
#### FIX!!! power.t.test(n = nrow(GoodSales), sd = 1, sig.level = 0.05, type = "one.sample")

coefficient %>% rename("Regression Coefficient" = "Estimate", #renaming some columns to what appraisers are used to seeing
                       "T-Statistic to Test H0" = "t value",
                       "Prob_Level" = "Pr(>|t|)",
                       "Power of Test at 5%" = "Power")

print(coefficient) #Print the table

#Regression coefficients confidence intervals
conf_limits <- as.data.frame(confint(model, level = 0.95)) %>%
  rename("Lower 95% Conf. Limit" = "2.5 %",
         "Upper 95% Conf. Limit" = "97.5 %")

conf_limits$Regression.Coefficient <- coefficient[1]
conf_limits$Standard.Error <- coefficient[2]

full_conf_limits <- conf_limits[, c(3, 4, 1, 2)] #reorder to match NCSS format

print(full_conf_limits)

#Analysis of variance detail ANOVA
variance_detail <- anova(model)

#variance_detail$Power <- T-TEST!!!

variance_detail %>% rename("Sum of Squares" = "Sum Sq",
    "Prob Level" = "Pr(>F)",
    "F-Ratio" = "F value",
    "Power (5%)" = "Power")

print(variance_detail)

#Normality tests
#add in predicted values to the necessary_data table
necessary_data$Predicted_Val <- predict(model, necessary_data)

  #Shapiro Wilk, Anderson Darling, D'Agostino Skewness, D'Agostino Kurtosis, D'Agostino Omnibus
shapiro.test(necessary_data$Predicted_Val)
ad.test(necessary_data$Predicted_Val)
agostino.test(necessary_data$Predicted_Val)

#R^2

#Variable omission


########Plots########
#Plot: histogram of residuals of LnTrendedPrice
hist_residual <- qplot(model$residuals,
                       geom = "histogram",
                       bins = 20,
                       col = I("black"),
                       fill = I("yellow")) +
  labs(title = "Histogram of Residuals",
       x = "Residual",
       y = "Frequency")

print(hist_residual)

#Plot: probability plot
prob_plot <- ggplot(data = GoodSales, aes(sample = model$residuals)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = "Normal Probability Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Residuals")

print(prob_plot)

#Plot: y vs x - fitted plot grid LNTrendedPrice vs coefficients, one plot per x variable
p1 <- ggplot(GoodSales, aes(BaseLandC, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p2 <- ggplot(GoodSales, aes(ConditionC, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p3 <- ggplot(GoodSales, aes(GradeC, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p4 <- ggplot(GoodSales, aes(TotalRcnC, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p5 <- ggplot(GoodSales, aes(KingCountyYN, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p6 <- ggplot(GoodSales, aes(Plat320493YN, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p7 <- ggplot(GoodSales, aes(Plat736630YN, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p8 <- ggplot(GoodSales, aes(Plats29440X, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p9 <- ggplot(GoodSales, aes(Plats76952X, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p10 <- ggplot(GoodSales, aes(Sub8YN, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p11 <- ggplot(GoodSales, aes(Nghb11_12_YN, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

p12 <- ggplot(GoodSales, aes(AgeC_Ren_Cubed, LnTrendedPrice)) +
  geom_point(color = "blue", alpha=0.4)

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, nrow=4)

#Plot: residual vs yhat - just one plot of residualLNTrendedPrice vs predictedLNTrendedPrice
resid_Yhat <- ggplot(model, aes(.fitted, .resid)) +
  geom_point() +
  stat_smooth(method="loess") +
  geom_hline(yintercept = 0, col="red") +
  ggtitle("Residuals vs Y-Hat") +
  xlab("Predicted Values of LNTrendedPrice") +
  ylab("Residuals of LNTrendedPrice")

print(resid_Yhat)

#Plot: residual of LNTrendedPrice vs fitted coefficients values, one plot per x variable
ggplot(model) +
  geom_point(aes(x=.fitted, y=.resid)) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted", y = "Residuals")

#Plot: partial residual vs x


#Plot: residual vs sale price
Residl <- resid(model)

plot(GoodSales$Sprice, Residl ,
     main = "Residuals Vs Sale Price" ,
     xlab = "Sale Price" ,
     ylab = "Residuals")
abline(h = 0, col = "red")



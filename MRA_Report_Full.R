library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(stats)
library(knitr)
library(ggpubr)

#Read NCSS sales list in CSV format
AllSales <- read.csv("Area72NCSSData.csv")

#Filter out RemBefore
AllSales_RemBefore <- AllSales %>%
  filter(AllSales$RemBefore == "0")

#Filter out RemDuring
GoodSales <- AllSales_RemBefore %>%
  filter(AllSales_RemBefore$RemDuring == "0")

#Apply multiple regression
model <- lm(LnTrendedPrice ~ BaseLandC + ConditionC + GradeC + TotalRcnC + KingCountyYN + Plat320493YN + Plat736630YN + Plats29440X + Plats76952X + Sub8YN + Nghb11_12_YN + AgeC_Ren_Cubed, data=GoodSales)

summary(model)

#Regression Coefficients T-Tests
coefficient <- as.data.frame(summary(model)$coefficient) #saving part of the summary table as a data frame

coefficient$Reject <- "Yes" #Adding a 'Reject H0 at 1%?' column
coefficient$Reject[coefficient$'Pr(>|t|)' > 0.05] <- "No"

coefficient$Power <- "1.0000"
#### FIX!!! power.t.test(n = nrow(GoodSales), sd = 1, sig.level = 0.05, type = "one.sample")

coefficient %>% rename("Regression Coefficient" = Estimate, #renaming some columns to what appraisers are used to seeing
                       "T-Statistic to Test H0" = "t value",
                       "Prob_Level" = "Pr(>|t|)",
                       "Power of Test at 5%" = "Power")

print(coefficient) #Print the table



#Descriptive Statistics
  #Variable - Count - Mean - Standard Deviation - Minimum - Maximum
descriptive_stats <- data.frame(coefficients(model),
                                "Count" = nrow(GoodSales))

descriptive_stats

#Correlation Matrix
print("Correlation Matrix")
correlation_matrix <- summary(model, correlation = TRUE)$correlation

print(correlation_matrix)

#Regression coefficients confidence intervals

#Estimated equation
print("Estimated Equation")
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
  return(model_eqn)
}
model_equation(model) #Returns estimated equation in the usual form

#Analysis of variance detail ANOVA
variance_detail <- anova(model) %>%
  rename("Sum of Squares" = "Sum Sq",
    "Prob Level" = "Pr(>F)")

variance_detail

#Normality tests
  #Shapiro Wilk, Anderson Darling, D'Agostino Skewness, D'Agostino Kurtosis, D'Agostino Omnibus
shapiro.test(model)

#R^2

#Variable omission

#Plot: histogram of residuals of LnTrendedPrice
Residl <- resid(model)

hist_residual <- qplot(model$residuals,
                       geom = "histogram",
                       bins = 10) +
  labs(title = "Histogram of Residuals",
       x = "Residual",
       y = "Frequency")

print(hist_residual)

#Plot: probability plot
prob_plot <- ggplot(data = GoodSales, aes(sample = model$residuals)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = "Normal Probability Plot of Residuals")

print(prob_plot)

#Plot: y vs x


#Plot: residual vs yhat


#Plot: residual vs x


#Plot: partial residual vs x


#Plot: residual vs sale price
Residl <- resid(model)

plot(GoodSales$Sprice, Residl ,
     main = "Residuals Vs Sale Price" ,
     xlab = "Sale Price" ,
     ylab = "Residuals")
abline(h = 0, col = "red")



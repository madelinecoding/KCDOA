#Load libraries
library(magrittr)
library(knitr)
library(kableExtra)
library(jsonlite)
library(jtools)
library(tidyverse)
library(broom)
library(data.table)
library(gridExtra)
library(ggthemes)
library(ggpubr)
library(GGally)
library(plotly)
library(grid)
library(olsrr)
library(fBasics)
library(car)
library(lmSubsets)
library(rmarkdown)

# Test Code Section ---------------------------------
# Any code added here will be removed by the server execution.
# Don't change anything here

executingFolder <- "C:/Data/RScript/EMVRegression/Upload"
parametersFileName <- "parameters.json"   
datasetFileName <- "testdataset.csv"
datasetSeparatorChar <- ","
setwd(executingFolder)
paramJson <- fromJSON(parametersFileName) 
dataset <- read.csv(file = datasetFileName, sep = datasetSeparatorChar)
jsonResultFileName <- "results.json"

dataset$SaleType <- as.factor(dataset$SaleType)
dataset$Cond <- as.numeric(dataset$Cond)
dataset$LnTrendedPrice <- as.numeric(log(dataset$TrendedPrice))
dataset$BaseLandC <- as.numeric(log(dataset$BaseLand))
dataset$TotalRcnC <- as.numeric(log(dataset$TotalRCN))
dataset$AgeC_Ren <- as.numeric(ifelse(dataset$YrRen==0, log(dataset$Age+1), log(dataset$Age+6)))
dataset$GoodYN <- as.numeric(ifelse(dataset$Cond == 4, 1, 0))
dataset$VGoodYN <- as.numeric(ifelse(dataset$Cond == 5, 1, 0))

# Server Code Section ---------------------------------
# This section will be executed by the server.

#I had to filter out all non-numerical rows from the Dataset for the lm(RegressionEquation to work)
dataset <- dplyr::filter(dataset, (dataset$LnTrendedPrice>0 & dataset$BaseLandC>0 & dataset$TotalRcnC>0 & dataset$AgeC_Ren>0))

# Filter out "bad" sales
dataset <- dplyr::filter(dataset, (dataset$RemBefore == 0 & dataset$RemDuring == 0))   

# Gather variables for regression
paramJson <- data.frame(paramJson, StringsAsFactors = FALSE)

IndependentVariables <- paramJson %>%
  dplyr::select(dplyr::starts_with("Independent"))

DependentVariable <-  paramJson %>%
  dplyr::select(dplyr::starts_with("Dependent"))

# Prepare formula for regression
RegressionEquation <- as.formula(
  paste(DependentVariable[1, ],
        paste(IndependentVariables[1, ], collapse = " + "),
        sep = " ~ "
  )
)

IndepVariableCount <- length(IndependentVariables)

# Calculate linear regression---------------------------------------------------
Model <- lm(RegressionEquation, data = dataset)

# Estimated equation with coefficients------------------------------------------
DepenVarAndIntercept <- paste(DependentVariable, "=",
                              Model$coefficients[1], "+")
CoeffAndIndepenVars <- paste(Model$coefficients[-1],
                             names(Model$coefficients[-1]),
                             sep = " * ",
                             collapse = " + ")
CoeffAndIndepenVars <- gsub("\\+ -", "- ", CoeffAndIndepenVars)
EstimatedEquation <- paste(DepenVarAndIntercept, CoeffAndIndepenVars)

dataset$AVRatio <- as.numeric(dataset$AVRatio)

# Identify influential outliers
ModelResult <- dataset %>%
  mutate(
    Predicted = fitted.values(Model),
    Residual = resid(Model),
    RStudent = rstudent(Model),     
    Hat = hatvalues(Model),
    CovRat = covratio(Model),
    Dffits = dffits(Model),
    CooksD = cooks.distance(Model),
    P = ncol(IndependentVariables) + 1,
    N = n(),
    HatLimit = 2 * (P / N),
    CovRatUprLim = 1 + (3 * (P / N)),
    CovRatLwrLim = 1 - (3 * (P / N)),
    DffitsLimit = 2 * (sqrt(P / N)),
    CooksDLimit = 1,
    HatOutlier = abs(as.numeric(Hat)) > HatLimit,
    CovRatOutlier = (CovRat > CovRatUprLim) + (CovRat < CovRatLwrLim),
    DffitsOutlier = abs(as.numeric(Dffits)) > DffitsLimit,
    CooksDOutlier = (CooksD > 1) * 1,
    RStudOutlier = abs(as.numeric(RStudent)) >= 2,
    SASOutlier = (CovRatOutlier > 0) * (DffitsOutlier > 0) * (RStudOutlier > 0),
    SASObs = (CovRatOutlier > 0) * (DffitsOutlier > 0) * (RStudOutlier > 0) * (HatOutlier > 0),
    InfluentialOutlier = SASOutlier + SASObs
  )
                  
    #Combine independent and dependent variables into one table
    ModelVariables <- data.frame(Model$model)
                  
    #Descriptive Statistics-----------------------------------------------------
    #Create standard error function
    std <- function(x) sd(x)/sqrt(length(x))
                  
    #Create table showing Descriptive Statistics of model
    DescriptiveStatistics <- data.table("Variable" = c(colnames(ModelVariables)))
    DescriptiveStatistics$Count <- count(ModelVariables)
    DescriptiveStatistics$Mean <- apply(ModelVariables, 2, mean)
    DescriptiveStatistics$StandardDeviation <- apply(ModelVariables, 2, sd)
    DescriptiveStatistics$StandardError <- apply(ModelVariables, 2, std)
    DescriptiveStatistics$Min <- apply(ModelVariables, 2, min)
    DescriptiveStatistics$Max <- apply(ModelVariables, 2, max)
                  
    DescriptiveStatistics <- DescriptiveStatistics %>% rename("Std. Dev." = "StandardDeviation",
                                                                            "Std. Error" = "StandardError")
                  
    DescriptiveStatistics <- kable(DescriptiveStatistics, digits = 5, format = 'html') %>%
      column_spec(1:7, width = "3cm") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")
                  
    #Run Summary Report---------------------------------------------------------
    RunSummaryReport <- data.table("R Squared" = summary(Model)$r.squared)
    RunSummaryReport$AdjRSq <- data.table(summary(Model)$adj.r.squared)
    RunSummaryReport$ResidStdError <- data.table(summary(Model)$sigma)
    RunSummaryReport$DepVar <- data.table(as.character(Model$terms[[2]]))
    RunSummaryReport$IndVarCount <- data.table(as.integer(Model$rank)-1)
                  
    RunSummaryReport <- RunSummaryReport[,c(4,5,1,2,3)] %>%
      rename("Adjusted R-Squared" = "AdjRSq",
             "Residual Standard Error" = "ResidStdError",
             "Dependent Variable" = "DepVar",
             "Number of Independent Variables" = "IndVarCount",
             #"Number of Sales Removed" ="FilteredRows"
             )
                  
    RunSummaryReport <- t(RunSummaryReport)
                  
    RunSummaryReport <- kable(RunSummaryReport, digits = 8, format = 'html') %>%
      column_spec(1, width = "5cm", bold=T) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")
                  
    #Correlation Matrix---------------------------------------------------------
    CorrelationMatrix <- GGally::ggcorr(ModelVariables, label = TRUE, layout.exp = 1, nbreaks = 5, palette = "RdBu", label_round = 2, label_size = 3, size = 0)
    CorrNames <- data.frame(x=seq(ModelVariables), y=seq(ModelVariables), lbs = names(ModelVariables))
    CorrelationMatrix <- CorrelationMatrix + geom_text(data=CorrNames, aes(x, y, label = names(ModelVariables), hjust = .3), size = 2.5)
                  
    #Create Regression Coefficients T-Tests and Confidence Intervals------------
    ConfidenceLimits <- as.data.frame(confint(Model, level = 0.95)) %>%
      rename("Lower 95% Confidence Limit" = "2.5 %",
             "Upper 95% Confidence Limit" = "97.5 %")
    ConfidenceLimits$Variables <-rownames(ConfidenceLimits)
                  
    Coefficient <- data.table(summary(Model)$coefficient)
    Coefficient$Variables <- rownames(ConfidenceLimits)
                  
    Coefficient$Reject <- "Yes" #Adding a 'Reject H0 at 1%?' column
    Coefficient$Reject[Coefficient$'Pr(>|t|)' > 0.05] <- "No"
                  
    Coefficient <- Coefficient[, c(5, 1, 2, 3, 4, 6)] %>%
      rename("Regression Coefficient" = "Estimate",
             "T-Statistic to Test (H0)" = "t value",
             "Prob Level" = "Pr(>|t|)",
             "Reject H0 at 5%?" = "Reject")
      
    Coefficient <- dplyr::right_join(Coefficient, ConfidenceLimits, by = c("Variables" = "Variables"))
                  
    Coefficient <- kable(Coefficient, digits = 8, format = 'html') %>%
      column_spec(1:6, width = "4cm") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")
                  
    #Analysis of variance detail ANOVA------------------------------------------
    ANOVATable <- as.data.table(anova(Model)) %>%
      rename("Sum of Squares" = "Sum Sq",
             "Prob Level" = "Pr(>F)")
                  
    ANOVATable$Variables <- c(colnames(ModelVariables)[-1], "Error")
                  
    ANOVATable <- ANOVATable[,c(6,1, 2, 3, 4, 5)]
                  
    ANOVATable <- kable(ANOVATable, digits = 5, format = 'html') %>%
      column_spec(1, width = "4cm") %>%
        column_spec(3:6, width = "3cm") %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")
                  
    #R-Sq Report----------------------------------------------------------------
    #Run forward stepwise regression function
    ForwardStep <- ols_step_forward_p(Model)
                  
    RSqReport <- data.table("Variable" = ForwardStep$predictors)
    RSqReport$TotalRSq <- ForwardStep$rsquare
    RSqReport$MallowCp <- ForwardStep$mallows_cp
    RSqReport$RMSE <- ForwardStep$rmse
                  
    colnames(RSqReport) <- c("Independent Variable (I.V.)", "Stepwise R-Squared", "Stepwise Mallow's Cp", "Stepwise RMSE")
                  
    RSqReport <- kable(RSqReport, digits = 5, format = 'html') %>%
      column_spec(1:4, width = "4cm") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")
                  
    #Variable Omission Report---------------------------------------------------
    #Function for variable omission report - R2
    #If-Then statement to account for fewer than 2 variables in model
    if (IndepVariableCount > 2) {
    VarOmissionR2 <- function(Var) {
      Var <- rlang::quo_name(rlang::enquo(Var)) #needed to put argument into quotation marks
      LessorRegressor <- lmSubsets(Model, exclude = Var, intercept = FALSE)
      LessorRegressor <- summary(LessorRegressor)
      StatTable <- as.data.frame(LessorRegressor$stats) #pulls stats table from summary
      StatTable <- tail(StatTable, 1) #returns last row of table
      return(StatTable$R2)
    }
                  
      #Function for variable omission report - Mallow's Cp
      VarOmissionMCp <- function(Var) {
        Var <- rlang::quo_name(rlang::enquo(Var)) #needed to put argument into quotation marks
        LessorRegressor <- lmSubsets(Model, exclude = Var, intercept = FALSE)
        LessorRegressor <- summary(LessorRegressor)
        StatTable <- as.data.frame(LessorRegressor$stats) #pulls stats table from summary
        StatTable <- tail(StatTable, 1) #returns last row of table
        return(StatTable$Cp)
      }
                  
    #Create Variable Omission Report
    VarOmissionVars <- data.table("Variable" = c(colnames(ModelVariables[-1])))
    VarOmissionROmit <- data.table("R_Omit" = apply(VarOmissionVars, 1, VarOmissionR2))
    VarOmissionMallowCp <- data.table("MallowCp" = apply(VarOmissionVars, 1, VarOmissionMCp))
    VarOmissionFull <- bind_cols(VarOmissionVars, VarOmissionROmit, VarOmissionMallowCp)
    names(VarOmissionFull) <- c("Variable", "R-Sq if I.V. Omitted", "Mallow's Cp if I.V. Omitted")
                  
    VarOmissionFull <- kable(VarOmissionFull, digits = 5, format = 'html') %>%
      column_spec(1:3, width = "4cm") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")
    }
    
    #Multicollinearity Table----------------------------------------------------
    MulticollinearityTable <- as.data.frame(ols_vif_tol(Model)) %>%
      rename("Variance Inflation Factor (VIF)" = "VIF")
    MulticollinearityTable$Variables <- colnames(ModelVariables[-1])
                  
    MulticollinearityTable <- kable(MulticollinearityTable, digits = 8, format = 'html') %>%
      column_spec(1:3, width = "4cm") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")
                  
    #Plot: histogram of residuals of LnTrendedPrice-----------------------------
    HistResidual <- qplot(resid(Model),
                          geom = "histogram",
                          bins = 20,
                          col = I("black"),
                          fill = I("yellow")) +
                    labs(title = "Histogram of Residuals",
                         x = "Residual",
                         y = "Frequency")
    HistResidual <- ggplotly(HistResidual)
                  
    #Plot: probability plot-----------------------------------------------------
    options(scipen=10000)
    ProbPlot <- ggplot(data = dataset, aes(sample = resid(Model))) +
      stat_qq() +
      stat_qq_line(color = "red") +
      scale_y_continuous(name="Residuals") +
      labs(title = "Probability Plot of Residuals",
           x = "Theoretical Quantiles")
    QQPlotly <- ggplotly(ProbPlot)
    table <- Model[order(resid(Model))]
    table$text <- paste0("Enumber:", " ",dataset$Enum)
    QQPlotly$x$data[[1]]$text <- table$text
    QQPlotly %>% layout(title = list(text = "Probability Plot of Residuals"))
                     
    #Probability Tables---------------------------------------------------------
    ShapiroTable <- as.data.table(shapiroTest(resid(Model))@test)
    AndersonTable <- as.data.table(adTest(resid(Model))@test)
    DAgostinoTable <- as.data.table(dagoTest(resid(Model))@test)
    DAgostinoTable$method <- c("D'Agostino Chi-Squared Test", "D'Agostino Skewness Test", "D'AGostino Kurtosis Test")
                      
    NormalityOutput <- rbind(ShapiroTable, AndersonTable, DAgostinoTable)[,-4] %>%
      rename("Test Name" = "method",
             "Prob Level" = "p.value",
             "Test Statistic" = "statistic")
    NormalityOutput <- NormalityOutput[, c(3,1,2)]
                  
    NormalityOutput <- kable(NormalityOutput, digits = 5, format = 'html') %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")
                  
    # Residual of Y vs Predicted Y----------------------------------------------
    YvPredY<-ggplot(Model, aes(.fitted, .resid))+geom_point()
    YvPredY<-YvPredY+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
    YvPredY<-YvPredY+xlab("Fitted values")+ylab("Residuals")
    YvPredY<-YvPredY+ggtitle("Residual vs Fitted Plot")+theme_bw()
    YvPredY <- ggplotly(YvPredY)
                  
    # Residual of Y vs Sale Price-----------------------------------------------
    ResYvSP<-ggplot(Model, aes(dataset$Sprice, .resid))+geom_point()
    ResYvSP<-ResYvSP+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
    ResYvSP<-ResYvSP+xlab("Sale Price")+ylab("Residuals")
    ResYvSP<-ResYvSP+ggtitle("Residual vs Sale Price")+theme_bw()
    ResYvSP<-ggplotly(ResYvSP)
                  
    # Fitted Y vs Fitted X------------------------------------------------------
    ModelVariables_Long <- reshape2::melt(ModelVariables, id.vars = "LnTrendedPrice")
    FittedPlots <- ggplot(ModelVariables_Long, aes(x=value, y=LnTrendedPrice)) +
      geom_point(shape=1) +
      geom_smooth(method=lm, color="blue", se=FALSE) +
      geom_smooth(method=loess, color="red", linetype="dashed", se=FALSE) +
      facet_wrap(~variable, scales="free", ncol=2)
    FittedPlots <- ggplotly(FittedPlots)
                 
    # Component + Residual Plots------------------------------------------------
    ModelVariables_Resid <- ModelVariables
    ModelVariables_Resid$Residual <- Model$residuals
    ModelVariables_Long_Resid <- reshape2::melt(ModelVariables_Resid, id.vars = "Residual")
    CompResidPlots <- ggplot(ModelVariables_Long_Resid, aes(x=value, y=Residual)) +
      geom_point(shape=1) +
      geom_smooth(method=lm, color="blue", se=FALSE) +
      geom_smooth(method=loess, color="red", linetype="dashed", se=FALSE) +
      facet_wrap(~variable, scales="free", ncol=2)
    CompResidPlots <- ggplotly(CompResidPlots)
    
    # Generate the report for linear regression - first time to get the HTML format
    render("EMVMRA.Rmd", output_format = "html_document")
    # Second time to get the Word format
    #render("EMVMRA_word.Rmd", output_format = "word_document")
    
    # create list for intercept - this will get combined with the coefficients in the Results list
    ResultsAndInterceptList <- list(name = "regression_results",
                                    "Intercept" = as.numeric(Model$coefficients[1])
    )
    
    # IndependentVariables to atomic vector
    IndependentVariables <- unname(unlist(IndependentVariables[1,])) 
    
    IVCoeffList <- list()
    
    for(i in seq_along(IndependentVariables)) {
      IndependentVariables <- i  
      IVCoeffList[[paste0("Coefficient", i)]] <- Model$coefficients[[1+i]]
    }
    
    # prepare the results for JSON
    results <- list(
      "results" = list(c(ResultsAndInterceptList,
                         IVCoeffList
      ),
      list(name = "regression_goodness_of_fit",
           "R-Squared" = as.numeric(summary(Model)$r.squared),
           "AdjustedR-Squared" = as.numeric(summary(Model)$adj.r.squared)
      )
      ),
      "EstimatedEquation" = list(
        list("EstimatedEquation" = EstimatedEquation
        )
      ),
      "fileResults" = list(
      #  list("Type" = "Report",
      #       "Title" = "Regression Report",
      #       "FileName" = "EMVMRA_word.docx",
      #       Description = "EMV MRA Word Report"
      #  ),
        list("Type" = "Report",
             "Title" = "Regression Report",
             "FileName" = "EMVMRA.html",
             Description = "EMV MRA HTML Report"
        )
      )
    )
                      
    # Export the JSON file to results.json
    writeLines(toJSON(results, pretty=TRUE, auto_unbox=TRUE, digits=16), jsonResultFileName)
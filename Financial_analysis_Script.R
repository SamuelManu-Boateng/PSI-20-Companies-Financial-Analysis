## LOADING REQUIRED LIBRARIES##

library(readxl)
library(tidyverse)
library(lmtest)
library(sandwich)
library(dplyr)
library(corrplot)
library(psych)
library(plm)

# 1) Import dataset
 data <- read_excel("PSI 20 - Companies for Essay.xlsx")
 data2 <- data |> select (-`Net Income`,- `Total Assets`,
                          - `Total Liabilities`,-`Total Equity`, - Revenue,
                          - Company,- Year)
 View(data)
 
# 2) Data Cleaning

 data <- data |> select (- `Financial statement found` )

 data <- data |> 
   rename(
     ROA = Return_on_Assets,
     ROE = Return_on_Equity,
     FSIZ = Firm_Size,
     LEV = Leverage,
     BSIZ = Board_Size,
     BIND = Board_Independence,
     GDIV = Board_Diversity
     
   )
 
 # 3) PANEL STRUCTURE SETUP (for Hausman)
 pdata <- pdata.frame(data, index = c("Company", "Year"))
 
 # 4) Fixed and Random Effects Models + Hausman Tests
 
 # ROA Models
 fe_roa <- plm(ROA ~ BSIZ + BIND + GDIV + FSIZ + LEV, data = pdata, model = "within")
 re_roa <- plm(ROA ~ BSIZ + BIND + GDIV + FSIZ + LEV, data = pdata, model = "random")
 
 hausman_roa <- phtest(fe_roa, re_roa)
 print("Hausman Test for ROA:")
 print(hausman_roa)
 
 # ROE Models
 fe_roe <- plm(ROE ~ BSIZ + BIND + GDIV + FSIZ + LEV, data = pdata, model = "within")
 re_roe <- plm(ROE ~ BSIZ + BIND + GDIV + FSIZ + LEV, data = pdata, model = "random")
 
 hausman_roe <- phtest(fe_roe, re_roe)
 print("Hausman Test for ROE:")
 print(hausman_roe)
 
 
 # 5) Summary Statistics
 print("Summary Statistics:")
 Descriptive_stats <- summary(data2)
 describe(data2)

 
 # 6) Correlation Matrix
 numeric_vars <- data |>
   select(ROA, ROE, FSIZ, LEV, BSIZ, BIND, GDIV)
 
 cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
 print("Correlation Matrix:")
 print(cor_matrix)
 corrplot(cor_matrix, method = "number")
 
 # 7) OLS Models (robust)
 
 # ROA OLS
 model_roa <- lm(ROA ~ BSIZ + BIND + GDIV + FSIZ + LEV, data = data)
 print("OLS Model for ROA:")
 summary(model_roa)
 coeftest(model_roa, vcov = vcovHC(model_roa, type = "HC1"))
 
 # ROE OLS
 model_roe <- lm(ROE ~ BSIZ + BIND + GDIV + FSIZ + LEV, data = data)
 print("OLS Model for ROE:")
 summary(model_roe)
 coeftest(model_roe, vcov = vcovHC(model_roe, type = "HC1"))
 
 
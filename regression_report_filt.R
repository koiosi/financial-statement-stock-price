# Regression with just the filtered companies so 27 companies -------
# Regression R #
library(tidyverse)
library(fixest)
library(modelsummary)
library(lubridate)
#########REGRESSION PART ----------------------------

# I changed this code from data_ew.csv to data_ew_2.csv
df_repew <- readr::read_csv("C:/R_folder/new/1_data/data_report_ew.csv")


print(unique(df_repew$company))

# df_repew <- df_repew %>%
#   select(-PE, -VA, -VO)


df_repew <- df_repew %>%
  group_by(company, year(date)) %>%
  mutate(
    ew = tidyr::replace_na(ew, 0),  # Replace missing values in 'ew' with 0
    t = row_number() - 1, # Create a time index (starting from 0)
    date_published2 = ifelse(report_published == 1, t, NA), #ttt = ifelse(publish_dummy == 1, date - date_published, 0)
  ) %>% 
  tidyr::fill(date_published2, .direction = "updown") %>% 
  dplyr::mutate(
    ttt = t - date_published2,
    ttt = ifelse(ttt %in% -10:10, ttt, 0)) %>% 
  dplyr::group_by(company) %>% 
  dplyr::mutate(
    dP = c(NA, diff(P)), # Change in P (lagged difference)
    dRI = c(NA, diff(RI)), # Change in RI
    dMV = c(NA, diff(MV)), # Change in MV
    dPE= c(NA, diff(PE)), # Change in PE
    dVA = c(NA, diff(VA)), # Change in VA
    dVO = c(NA, diff(VO)), # Change in VO
    gr_P = ((P / lag(P))-1)*100, # Growth rate of P
    gr_RI = ((RI / lag(RI))-1)*100, # Growth rate of RI
    gr_MV = ((MV / lag(MV))-1)*100, # Growth rate of MV
    gr_PE = ((PE / lag(PE))-1)*100, # Growth rate of PE
    gr_VA = ((VA / lag(VA))-1)*100, # Growth rate of VA
    gr_VO = ((VO / lag(VO))-1)*100, # Growth rate of VO
    covers_sust = ifelse(n_sust > 0, 1, 0), # Sustainability reports
    covers_ets = ifelse(n_ets > 0, 1, 0), # ETS reports
    covers_emis = ifelse(n_emis > 0, 1, 0), # Emissions reports
    covers_ai = ifelse(n_ai > 0, 1, 0) # AI reports
  )
glimpse(df_repew)


write.csv(df_repew, "C:/R_folder/new/1_data/analyse_report.csv")

# Custom variable names for the summary table
custom_variable_names <- c(
  "P" = "Price",
  "MV" = "Market Value",
  "RI" = "Return of Investment",
  "dP" = "Change in P",
  "l(dP, 1:14)" = "Lagged Change in P (1-10)",
  "dMV" = "Change in MV",
  "dRI" = "Change in RI",
  "dPE" = "Change in PE",
  "dVA" = "Change in VA",
  "dVO" = "Change in VO",
  "gr_P" = "Growth Rate of P",
  "gr_MV" = "Growth Rate of MV",
  "gr_RI" = "Growth Rate of RI",
  "gr_PE" = "Growth Rate of PE",
  "gr_VA" = "Growth Rate of VA",
  "gr_VO" = "Growth Rate of VO",
  "as.factor(wday(date))" = "Day of Week Fixed Effects",
  "ttt" = "Time to Treatment (ttt)",
  "n_sust" = "Frequency of sustainability",
  "n_ets" = "Frequency of ets",
  "n_emis" = "Frequency of emissions",
  "n_ai"= "Frequency of ai",
  "covers_sust" = "Covers Sustainability",
  "covers_ets" = "Covers ETS",
  "covers_emis" = "Covers Emissions",
  "covers_ai" = "Covers AI"
)

# First Check how to create the model, then create the model
# Regression models ---------------------------------
# 1
model_1 <- fixest::feols(
  fml = P ~ i(ttt, ref = 0) + MV +
    as.factor(lubridate::wday(date))
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_1a <- fixest::feols(
  fml = P ~ i(ttt, ref = 0) + RI +
    as.factor(lubridate::wday(date))
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_1b <- fixest::feols(
  fml = P ~ i(ttt, ref = 0) + MV + covers_sust + covers_ets + covers_emis + covers_ai +
    as.factor(lubridate::wday(date))
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_1c <- fixest::feols(
  fml = P ~ i(ttt, ref = 0) + RI + covers_sust + covers_ets + covers_emis + covers_ai +
    as.factor(lubridate::wday(date))
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

modelsummary(list(model_1, model_1a, model_1b, model_1c),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
             )

# 2 ****

model_2 <- fixest::feols(
  fml = dP ~ i(ttt, ref=0) + dMV +
    as.factor(lubridate::wday(date))  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_2a <-  fixest::feols(
  fml = dP ~  i(ttt, ref = 0) + l(dP, 1:10) + dMV + 
    as.factor(lubridate::wday(date))   
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_2b <- fixest::feols(
  fml = dP ~  (i(ttt,ref = 0)* dMV) + 
    as.factor(lubridate::wday(date))  
     | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_2c <- fixest::feols(
  fml = dP ~  (i(ttt,ref = 0)* dMV) + l(dP, 1:10) + 
    as.factor(lubridate::wday(date))  
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_2d <- fixest::feols(
  fml = dP ~  i(ttt,ref = 0) + dMV + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust + covers_ets + covers_emis + covers_ai
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_2e <- fixest::feols(
  fml = dP ~  (i(ttt,ref = 0)* dMV) + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust + covers_ets + covers_emis + covers_ai
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

modelsummary(list(model_2,model_2a, model_2b, model_2c, model_2d, model_2e),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)


iplot(list(model_2, model_2d), sep = 0.5, ref.line = 0,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)')
legend("bottomleft", col = c(1, 2), pch = c(20, 17),
       legend = c("Model 1", "Model 2")) # Two way fixed effects model

esttex(model_2)
?etable
######### 3# 3model_2 ----------------
model_3 <- fixest::feols(
  fml = gr_P ~ i(ttt, ref = 0) + gr_MV + 
    as.factor(lubridate::wday(date))  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_3a <-  fixest::feols(
  fml = gr_P ~  i(ttt, ref = -1) + l(dP, 1:10) + gr_MV + 
    as.factor(lubridate::wday(date))   
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_3b <- fixest::feols(
  fml = gr_P ~  (i(ttt,ref = -1)* gr_MV) + 
    as.factor(lubridate::wday(date))  
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_3c <- fixest::feols(
  fml = gr_P ~  (i(ttt,ref = -1)* gr_MV) + l(dP, 1:10) + 
    as.factor(lubridate::wday(date))  
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_3d <- fixest::feols(
  fml = gr_P ~  i(ttt,ref = -1) + gr_MV + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust + covers_ets + covers_emis + covers_ai
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_3e <- fixest::feols(
  fml = gr_P ~  (i(ttt,ref = -1)* gr_MV) + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust + covers_ets + covers_emis + covers_ai
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

modelsummary(list(model_3,model_3a, model_3b, model_3c, model_3d, model_3e),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo")

iplot(list(model_3d, model_3e), sep = 0.5, ref.line = 0,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') 

modelsummary(list(model_2a, model_2b, model_2c, model_3a, model_3b, model_3c),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo")



###### CALISAN KOD, fe Yes gosteriyor --------------------
modelsummary(
  list(model_2a, model_2b, model_2c, model_3a, model_3b, model_3c),
  stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  coef_map = custom_variable_names,  # Custom variable labels
  title = "Regression Results",  # Add a title
  gof_map = list(
    list(raw = "nobs", clean = "Number of Observations", fmt = function(x) format(x, big.mark = ",")),
    list(raw = "r2.within", clean = "R2 Within", fmt = function(x) sprintf("%.3f", x)),
    list(raw = "r2.within.adjusted", clean = "Adjusted R2 Within", fmt = function(x) sprintf("%.3f", x)),
    list(raw = "vcov.type", clean = "Standard Errors", fmt = "Company")
  ),
  add_rows = data.frame(
    term = c("Company Fixed Effects", "Month Year Fixed Effects"),
    model_2a = c("YES", "YES"),
    model_2b = c("YES", "YES"),
    model_2c = c("YES", "YES"),
    model_3a = c("YES", "YES"),
    model_3b = c("YES", "YES"),
    model_3c = c("YES", "YES")
  )
)


###### "eski" 3 -> yeni 4------------------------------

model_4 <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10)  + gr_MV + 
    i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

table(df_repew$ttt)         # For categorical variables like event time
table(df_repew$company)     # Company distribution
table(df_repew$report_published)
table(df_repew$publish_dummy)



model_4c <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10)  + gr_MV + 
    i(ttt ,ref = 0) +
  + covers_sust + covers_ets + covers_emis + covers_ai +
  as.factor(lubridate::wday(date)) | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

# 3b Danneman (guess)
model_4b <- fixest::feols(
  fml = dP ~ l(dP, 1:14) + dMV + dRI + 
    as.factor(lubridate::wday(date)) +  
    i(ttt, ref = 0) | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)


# 4 ************

model_4a <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dRI * i(ttt, ref = 0) + dMV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) 
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)


model_4d <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dRI * i(ttt, ref = 0) + dMV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) + covers_sust + covers_ets + covers_ai 
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

modelsummary(list(model_4, model_4c, model_4b, model_4a, model_4d),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)

iplot(list(model_4, model_4c, model_4b, model_4a, model_4d),
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

#5 **********

model_5 <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10) + gr_RI * i(ttt, ref = 0) + gr_MV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) 
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)


model_6 <- fixest::feols(
  fml = dP ~ l(dP, 1:10)+ dPE + dVA + dVO +
    as.factor(lubridate::wday(date)) +  
    i(ttt, ref = 0)
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_7 <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10) + gr_PE + gr_VA + gr_VO +
    as.factor(lubridate::wday(date)) +  
    i(ttt, ref = 0)
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

modelsummary(list(
  "(abs. values)" = model_1,
  "difference values" = model_2,
  "growth values" = model_3,
  "difference values with treatment" = model_4
  # "growth values with treament" = model_5,
  # "difference values frequence added" = model_6,
  # "growth values freq added" = model_7
),
stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
coef_map = custom_variable_names, # Map variable names to custom labels
title = "Regression Results", # Add a title to the table
gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo",
)


iplot(model_4,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(model_4b,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(model_3,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(model_3b,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model


iplot(list(model_3b, model_4),
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model





# GOOD MODEL TABLE HERE --------------------
modelsummary(list(model_4b, model_4b),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)
modelsummary(list(model_3b, model_4b),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo",
             output = "latex"
)


modelsummary(model_4b,
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)

modelsummary(list(model_1, model_2, model_3),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)


# Y??l?? filtreledim
# tam anlam veremedi??im bi sonu?? ald??m, as??l modelimde
# ??irketin her raporu sonras?? bir onceki y??l??n rapor etkisi s??f??rlan??yor mu ?

df_filt <- df_repew %>%
  filter(date >= "2010-01-01", date <= "2023-12-31")

model_z <- fixest::feols(
  fml = gr_P ~  (i(ttt,ref = -1)* gr_MV) + 
    as.factor(lubridate::wday(date))  
  | company + month(date)^year(date), 
  data = df_filt, panel.id=~company+date)

model_zz <- fixest::feols(
  fml = gr_P ~  (i(ttt,ref = -1)* gr_MV) + l(dP, 1:10) + 
    as.factor(lubridate::wday(date))  
  | company + month(date)^year(date), 
  data = df_filt, panel.id=~company+date)

model_zzz <- fixest::feols(
  fml = gr_P ~  i(ttt,ref = -1) + gr_RI + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust + covers_ets + covers_emis + covers_ai
  | company + month(date)^year(date), 
  data = df_filt, panel.id=~company+date)

model_zzzz <- fixest::feols(
  fml = gr_P ~  i(ttt,ref = -1) + gr_RI + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust + covers_ets + covers_emis + covers_ai
  | company + month(date)^year(date), 
  data = df_filt, panel.id=~company+date)

modelsummary(list(model_z, model_zz, model_zzz, model_zzzz),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)

model_MVd <- fixest::feols(
  fml = dP ~  i(ttt,ref = 0) + dMV + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_RId <- fixest::feols(
  fml = dP ~  i(ttt,ref = 0) + dRI + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)


model_together_dall <- fixest::feols(
  fml = dP ~  i(ttt,ref = 0) + dMV + dRI + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_MVgr <- fixest::feols(
  fml = gr_P ~  i(ttt,ref = 0) + gr_MV + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_RIgr <- fixest::feols(
  fml = gr_P ~  i(ttt,ref = 0) + gr_RI + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)


model_together_all <- fixest::feols(
  fml = gr_P ~  i(ttt,ref = 0) + gr_RI +  gr_MV + l(dP, 1:14) + 
    as.factor(lubridate::wday(date)) + covers_sust
  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)



modelsummary(list("model real change over time + MV" = model_MVd,
                  "model real change over time + RI" = model_RId,
                  "model real change over time all" = model_together_dall,
                  "model percentage change over time + MV" = model_MVgr,
                  "model percentage change over time + RI" = model_RIgr,
                  "model percentage change over time all" = model_together_all),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)             



modelsummary(
  list(
    "MV only (dP)"     = model_MVd,
    "RI only (dP)"     = model_RId,
    "MV + RI (dP)"     = model_together_dall,
    "MV only (gr_P)"   = model_MVgr,
    "RI only (gr_P)"   = model_RIgr,
    "MV + RI (gr_P)"   = model_together_all
  ),
  stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  coef_map = custom_variable_names,
  title = "Regression Results",
  gof_map = list(
    list(raw = "nobs", clean = "Number of Observations", fmt = function(x) format(x, big.mark = ",")),
    list(raw = "r2.within", clean = "R2 Within", fmt = function(x) sprintf("%.3f", x)),
    list(raw = "r2.within.adjusted", clean = "Adjusted R2 Within", fmt = function(x) sprintf("%.3f", x)),
    list(raw = "vcov.type", clean = "Standard Errors", fmt = "Company")
  ),
  add_rows = data.frame(
    term = c("Company Fixed Effects", "Month-Year Fixed Effects"),
    `MV only (dP)`     = c("Yes", "Yes"),
    `RI only (dP)`     = c("Yes", "Yes"),
    `MV + RI (dP)`     = c("Yes", "Yes"),
    `MV only (gr_P)`   = c("Yes", "Yes"),
    `RI only (gr_P)`   = c("Yes", "Yes"),
    `MV + RI (gr_P)`   = c("Yes", "Yes")
  ),
  output = "latex"
)


etable(
  model_MVd,model_RId,model_together_dall,model_MVgr,model_RIgr,model_together_all)

model_word_sust <- fixest::feols(
  fml = gr_P ~  i(ttt,ref = 0) + gr_RI + covers_sust +
    as.factor(lubridate::wday(date))  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_word_ets <- fixest::feols(
  fml = gr_P ~  i(ttt,ref = 0) + gr_RI + covers_ets +
    as.factor(lubridate::wday(date))  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_word_emis <- fixest::feols(
  fml = gr_P ~  i(ttt,ref = 0) + gr_RI + covers_emis +
    as.factor(lubridate::wday(date))  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_word_ai <- fixest::feols(
  fml = gr_P ~  (gr_RI * i(ttt,ref = 0)) + (gr_MV * i(ttt,ref = 0)) + covers_ai +
    as.factor(lubridate::wday(date))  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

model_word_all <- fixest::feols(
  fml = gr_P ~  i(ttt,ref = 0) + gr_RI + covers_sust + covers_ets + covers_emis + covers_ai +
    as.factor(lubridate::wday(date))  | company + month(date)^year(date), 
  data = df_repew, panel.id=~company+date)

modelsummary(list(model_word_sust, model_word_ets, model_word_emis, model_word_ai, model_word_all),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)

etable(
  model_word_sust, model_word_ets, model_word_emis, model_word_ai, model_word_all)

iplot(model_word_ai,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(model_together_d,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model
iplot(list(model_z, model_zz, model_zzz, model_zzzz),
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

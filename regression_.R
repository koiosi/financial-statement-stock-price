# Regression R #
library(tidyverse)
library(fixest)
library(modelsummary)
library(lubridate)
#########REGRESSION PART ----------------------------

# I changed this code from data_ew.csv to data_ew_2.csv
df_ew <- readr::read_csv("C:/R_folder/new/1_data/merg_df.csv")


# df_ew <- df_ew %>% 
#   select(-PE, -VA, -VO)

print(unique(df_ew$company))

colSums(is.na(df_ew)) # Check for missing values


df_ew <- df_ew %>%
  group_by(company, year(date)) %>%
  mutate(
    ew = tidyr::replace_na(ew, 0),  # Replace missing values in 'ew' with 0
    t = row_number() - 1, # Create a time index (starting from 0)
    date_published2 = ifelse(report_published == 1, t, NA), #ttt = ifelse(publish_dummy == 1, date - date_published, 0)
  ) %>% 
  tidyr::fill(date_published2, .direction = "updown") %>% 
  dplyr::mutate(
    ttt = t - date_published2,
    ttt = ifelse(ttt %in% -20:20, ttt, 0)) %>% 
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
    covers_sust = ifelse(n_sust > 50, 1, 0), # Sustainability reports
    covers_ets = ifelse(n_ets > 0, 1, 0), # ETS reports
    covers_emis = ifelse(n_emis > 0, 1, 0), # Emissions reports
    covers_ai = ifelse(n_ai > 0, 1, 0) # AI reports
  )
glimpse(df_ew)



write.csv(df_ew, "C:/R_folder/new/1_data/analyse_with.csv")

# For modelsummary
gm <- "R2$|R2 Adj.|AIC|BIC|RMSE"

# Custom variable names for the summary table
custom_variable_names <- c(
  "P" = "Price",
  "MV" = "Market Value",
  "RI" = "Return of Investment",
  "dP" = "Change in P",
  "l(dP, 1:10)" = "Lagged Change in P (1-10)",
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
  "i(ttt, ref = 0)" = "Time to Treatment (ttt)",
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
  fml = P ~ MV * i(ttt, ref = 0) + RI *  i(ttt, ref = 0) + 
    as.factor(lubridate::wday(date))  
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

# 2 ****

model_2 <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dMV + dRI + 
    as.factor(lubridate::wday(date)) +  
    i(ttt, ref = 0) | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

# 3

model_3 <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10) + gr_RI  + gr_MV + 
    i(ttt, ref = 0) + as.factor(lubridate::wday(date)) | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

# 3b Danneman (guess)
model_3b <- fixest::feols(
  fml = dP ~ l(dP, 1:14) + dMV + dRI + 
    as.factor(lubridate::wday(date)) +  
    i(ttt, ref = -1) | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)


# 4 ************

model_4 <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dRI * i(ttt, ref = -1) + dMV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) 
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)


model_4b <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dRI * i(ttt, ref = 0) + dMV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) + covers_sust + covers_ets + covers_emis + covers_ai 
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)
# bu modele coverlar?? tek tek sokmaya cal??st??m olmad?? ----------------

model_4b_a <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dRI * i(ttt, ref = 0) + dMV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) + covers_sust 
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

model_4b_b <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dRI * i(ttt, ref = 0) + dMV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) + covers_ets 
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

model_4b_c <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dRI * i(ttt, ref = 0) + dMV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) + covers_emis 
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

model_4b_d <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dRI * i(ttt, ref = 0) + dMV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) + covers_ai 
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

modelsummary(list(model_4b, model_4b_a, model_4b_b, model_4b_c, model_4b_d),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)
# ------------------

model_5 <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10) + gr_RI * i(ttt, ref = 0) + gr_MV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) 
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)
iplot(model_5,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

model_6 <- fixest::feols(
  fml = dP ~ l(dP, 1:10)+ dPE + dVA + dVO +
    as.factor(lubridate::wday(date)) +  
    i(ttt, ref = 0)
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

model_7 <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10) + gr_PE + gr_VA + gr_VO +
    as.factor(lubridate::wday(date)) +  
    i(ttt, ref = 0)
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)



iplot(model_4,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model


iplot(model_3b,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model


iplot(list(model_3b, model_4, model_4b),
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model





# GOOD MODEL TABLE HERE --------------------
modelsummary(list(model_3b, model_4, model_4b),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)


modelsummary(model_4b,
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)

modelsummary(list(model_6, model_5, model_7),
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
output = "markdown"
)






###### YOUR WORK HERE ----------------------------

model_together <- fixest::feols(
  fml = gr_P ~ (dRI + dMV + 
                   as.factor(lubridate::wday(date)) + covers_sust )
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

df_ew$weekday <- as.factor(lubridate::wday(df_ew$date))
df_ew$year_month <- format(df_ew$date, "%Y-%m")
table(df_ew$covers_sust, useNA = "always")


model_gpt <- feols(
  gr_P ~ gr_RI + gr_MV + covers_sust + i(ttt, ref = 0)
  + as.factor(lubridate::wday(date))
  | company + month(date)^year(date),
  data = df_ew, panel.id = ~company + date)

modelsummary(model_gpt)
fixest::fitstat(model_gpt)
summary(model_gpt)

iplot(model_gpt,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

model_interact <- feols(
  gr_P ~ gr_RI + gr_MV + weekday + i(ttt, covers_sust, ref = 0)
  | company + year_month,
  data = df_ew
)
iplot(model_interact, main = "Effect of Sustainability Coverage Over Time", xlab = "Day to treatment")




model_together_d <- fixest::feols(
  fml = dP ~  (gr_RI + 
                 as.factor(lubridate::wday(date)) + covers_sust )
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

modelsummary(list(model_together, model_together_d),
             stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             coef_map = custom_variable_names, # Map variable names to custom labels
             title = "Regression Results", # Add a title to the table
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2$|R2 Adj.|R2 Pseudo"
)             

iplot(model_together,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(model_together_d,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

model_together_2 <- fixest::feols(
  fml = gr_P ~  (i(ttt,ref = 0) + gr_MV + l(dP, 1:20) + 
                   as.factor(lubridate::wday(date)) + covers_ai)
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

modelsummary(
  list(model_together, model_together_d, model_together_2),
  stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
  coef_map = custom_variable_names, # Map variable names to custom labels
  title = "Regression Results", # Add a title to the table
  gof_map = list(
    list(raw = "nobs", clean = "Number of Observations", fmt = function(x) format(x, big.mark = ",")),
    list(raw = "r2.within", clean = "R2 Within", fmt = function(x) sprintf("%.3f", x)),
    list(raw = "r2.within.adjusted", clean = "Adjusted R2 Within", fmt = function(x) sprintf("%.3f", x)),
    list(raw = "vcov.type", clean = "Standard Errors", fmt = "Company")
             ),
  add_rows = data.frame(
    term = c("Company Fixed Effects", "Month Year Fixed Effects"),
    model_together = c("YES", "YES"),
    model_together_d = c("YES", "YES"),
    model_together_2 = c("YES", "YES")))             



model_together_3 <- fixest::feols(
  fml = dP ~  (gr_RI + gr_MV + l(gr_P, 1:5) + i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) + covers_ai) | 
  company + month(date)^year(date), data = df_ew, panel.id=~company+date)
modelsummary(
  model_together_3,
  stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
  coef_map = custom_variable_names # Map variable names to custom labels
)

model_together_4 <- fixest::feols(
  fml = dP ~  (gr_RI + gr_MV + i(ttt, covers_ets, ref = 0) + l(dP, 1:5) +
    as.factor(lubridate::wday(date))) | 
  company + month(date)^year(date), data = df_ew, panel.id=~company+date)
modelsummary(
  model_together_4,
  stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1)
  # coef_map = custom_variable_names # Map variable names to custom labels
)

iplot(list(model_together_3, model_together_4),
       xlab = 'Day to treatment',
       main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model


# PAPER MODEL HERE CHECK MORE W??TH GPT AND TEST IT 
# WRITE ABOUT TESTS TO THE PAPER ---------------------------


model_15 <- fixest::feols(
  fml = P ~  (gr_RI +  i(ttt, ref = 0) + l(gr_P, 1:10) + covers_sust +
    as.factor(lubridate::wday(date))) | 
  company + month(date)^year(date), data = df_ew, panel.id=~company+date)
iplot(model_15,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model
modelsummary(
  model_15,
  stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
  coef_map = custom_variable_names # Map variable names to custom labels
)
model_5 <- fixest::feols(
  fml = gr_P ~  (gr_RI + i(ttt, ref = 0) + l(gr_P, 1:10) +
                   as.factor(lubridate::wday(date))) | 
    company + month(date)^year(date), data = df_ew_5, panel.id=~company+date)
iplot(model_5,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model
model_10 <- fixest::feols(
  fml = gr_P ~  (gr_RI + i(ttt, ref = 0) + l(gr_P, 1:10) +
                   as.factor(lubridate::wday(date))) | 
    company + month(date)^year(date), data = df_ew_10, panel.id=~company+date)
iplot(model_10,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

model_20 <- fixest::feols(
  fml = gr_P ~  (gr_RI + i(ttt, ref = 0) + l(gr_P, 1:10) +
                   as.factor(lubridate::wday(date))) | 
    company + month(date)^year(date), data = df_ew_20, panel.id=~company+date)
iplot(model_20,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(list(model_5, model_10, model_15, model_20),
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model


pdf("event_study_2x2.pdf", width = 10, height = 8)
par(mfrow = c(2, 2))
iplot(model_5, main = "Model event window(5days)", xlab = "Day to treatment")
iplot(model_10, main = "Model event window(10days)", xlab = "Day to treatment")
iplot(model_15, main = "Model event window(15days)", xlab = "Day to treatment")
iplot(model_20, main = "Model event window(20days)", xlab = "Day to treatment")
par(mfrow = c(1, 1))
dev.off()


model_paper_sust <- fixest::feols(
  fml = gr_P ~  (gr_RI + i(ttt, ref = 0) + l(gr_P, 1:10) + covers_sust +
                   as.factor(lubridate::wday(date))) | 
    company + month(date)^year(date), data = df_ew, panel.id=~company+date)
iplot(model_paper_sust,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model
modelsummary(
  model_paper_sust,
  stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
  coef_map = custom_variable_names # Map variable names to custom labels
)
library(performance)
check_collinearity(model_paper)
etable(model_paper, output = "html")

model_paper_emiss <- fixest::feols(
  fml = gr_P ~  (gr_RI + i(ttt, ref = 0) + l(gr_P, 1:10) + covers_emis +
                   as.factor(lubridate::wday(date))) | 
    company + month(date)^year(date), data = df_ew, panel.id=~company+date)
iplot(model_paper_emiss,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model
modelsummary(
  model_paper_emiss,
  stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
  coef_map = custom_variable_names # Map variable names to custom labels
)




model_paper_2 <- fixest::feols(
  fml = dP ~  (dRI +dMV + i(ttt, ref = 0) + l(dP, 1:7) +
    as.factor(lubridate::wday(date))) | 
  company + month(date)^year(date), data = df_ew, panel.id=~company+date)
iplot(model_paper_2,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

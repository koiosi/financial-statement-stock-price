# Necessary packages

library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(readr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)

# Load the data

# I changed this code from data_ew.csv to data_ew_2.csv
df_ew <- readr::read_csv("new/data_ew_2.csv")


df_ew <- df_ew %>% 
  select(-PE, -VA, -VO)

# df_ew %>% unique() %>% glimpse()
# tail(df_ew)

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
    ttt = ifelse(ttt %in% -5:5, ttt, 0)) %>% 
  dplyr::group_by(company) %>% 
  dplyr::mutate(
    dP = c(NA, diff(P)), # Change in P (lagged difference)
    dRI = c(NA, diff(RI)), # Change in RI
    dMV = c(NA, diff(MV)), # Change in MV
    gr_P = ((P / lag(P))-1)*100, # Growth rate of P
    gr_RI = ((RI / lag(RI))-1)*100, # Growth rate of RI
    gr_MV = ((MV / lag(MV))-1)*100 # Growth rate of MV
  )
glimpse(df_ew)

# Custom variable names for the summary table
custom_variable_names <- c(
  "P" = "Price",
  "MV" = "Market Value",
  "RI" = "Return of Investment",
  "dP" = "Change in P",
  "l(dP, 1:10)" = "Lagged Change in P (1-10)",
  "dMV" = "Change in MV",
  "dRI" = "Change in RI",
  "gr_P" = "Growth Rate of P",
  "gr_MV" = "Growth Rate of MV",
  "gr_RI" = "Growth Rate of RI",
  "as.factor(wday(date))" = "Day of Week Fixed Effects",
  "i(ttt, ref = 0)" = "Time to Treatment (ttt)",
  "n_sust" = "Frequency of sustainability",
  "n_ets" = "Frequency of ets",
  "n_emis" = "Frequency of emissions",
  "n_ai"= "Frequency of ai"
)


# First Check how to create the model, then create the model
# Regression models ---------------------------------
# 1
model_1 <- fixest::feols(
  fml = P ~ MV * i(ttt, ref = 0) + RI *  i(ttt, ref = 0) + 
    as.factor(lubridate::wday(date))  
    | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)
modelsummary(model_1, stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2 Within|R2 Pseudo") # Exclude AIC, BIC, and other stats

iplot(model_1, 
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model


# 5 ***************

model_5 <- fixest::feols(
  fml = gr_P ~ l(dP, 1:10)  + gr_MV *i(ttt, ref = 0) + gr_RI * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

modelsummary(model_5, stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2 Within|R2 Pseudo")

iplot(model_5,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

# 6


model_6 <- fixest::feols(
  fml = gr_P ~ l(dP, 1:10) + gr_RI + gr_RI * n_sust + gr_RI * n_ai + gr_RI * n_emis + gr_RI * n_ets +  
    i(ttt, ref = 0) | company + month(date)^year(date), 
  data = df_ew, panel.id = ~company + date)
modelsummary(model_6, stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2 Within|R2 Pseudo") # Exclude AIC, BIC, and other stats

iplot(model_6,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model


# 7 ****

model_7 <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10) +gr_MV* i(ttt, ref=0) + gr_RI* i(ttt,ref =0)  + n_sust  + n_ai + gr_MV * n_emis + gr_MV * n_ets + 
    i(ttt, ref = 0) | company + month(date)^year(date), 
  data = df_ew, panel.id = ~company + date)
modelsummary(model_7, stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2 Within|R2 Pseudo") # Exclude AIC, BIC, and other stats

iplot(model_7,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model


modelsummary(list(
  "(abs. values)" = model_1,
  "difference values" = model_2,
  "growth values" = model_3,
  "difference values with treatment" = model_4,
  "growth values with treament" = model_5,
  "difference values frequence added" = model_6,
  "growth values freq added" = model_7), stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
  coef_map = custom_variable_names, # Map variable names to custom labels
  title = "Regression Results", # Add a title to the table
  gof_omit = "AIC|BIC|Log.Lik|RMSE|R2 Within|R2 Pseudo"
  )

modelsummary(list(
  "model_6" = model_6,
  "model_7" = model_7), stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
)

# modelleri s??rala merdiven gibi yap




# since we have many coefficients, we can use the iplot() function to plot the coefficients of the model


iplot(model_1, 
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(model_2, 
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(model_3,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(model_4,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(model_5,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

iplot(model_6,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model


iplot(model_7,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model




# Displaying a summary table of both models with custom variable names and model titles
modelsummary(list(
  "model_1" = model_1,
  "model_2" = model_2,
  "model_3" = model_3,
  "model_4" = model_4), stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1))



modelsummary(
  list(
    "Model 1: Lagged Effects" = model_1,
    "Model 2: Lagged Effects" = model_2,
    "Model 3: Growth Rates" = model_3,
    "Model 4: Growth Rates" = model_4
  ),
  coef_map = custom_variable_names, # Map variable names to custom labels
  stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1), # Add significance stars
  title = "Regression Results", # Add a title to the table
  gof_omit = "AIC|BIC|Log.Lik|RMSE|R2 Within|R2 Pseudo" # Exclude AIC, BIC, and other stats
)


# 
# 
# # DENEME --------------
# 
# dd <- 
#   ggplot(df_ew, aes(x = gr_P, y = gr_RI)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(
#     title = "Scatter plot of Growth Rate of P and Growth Rate of RI",
#     x = "Growth Rate of P",
#     y = "Growth Rate of RI"
#   )
# print(dd)
# 
# dd_2 <- 
#   ggplot(df_ew, aes(x = gr_P, y = gr_MV)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(
#     title = "Scatter plot of Growth Rate of P and Growth Rate of MV",
#     x = "Growth Rate of P",
#     y = "Growth Rate of MV"
#   )
# print(dd_2)


library(ggplot2)
library(ggpubr)  # For stat_cor()

dd_3 <- ggplot(df_ew, aes(x = gr_P, y = gr_MV)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +  # Adjust color, transparency & size
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +  # Regression line
  stat_cor(aes(label = paste(..rr.label.., sep = "")), label.x = min(df_ew$gr_P), label.y = max(df_ew$gr_MV), size = 5) +  # Show R??
  labs(
    title = "Growth Rate of P and Growth Rate of MV",
    subtitle = "Linear regression with correlation coefficient",
    x = "Growth Rate of P (%)",
    y = "Growth Rate of MV (%)",
    caption = "Data source: Refinitv Eikon"
  ) +
  theme_minimal(base_size = 14) +  # Use a clean theme
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed")
  )

print(dd_3)

dd_4 <- ggplot(df_ew, aes(x = gr_P, y = gr_RI)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +  # Adjust point color, transparency & size
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +  # Regression line
  stat_cor(aes(label = paste(..rr.label.., sep = "")), 
           label.x = min(df_ew$gr_P), label.y = max(df_ew$gr_RI), size = 5) +  # Show R?? value
  labs(
    title = "Growth Rate of P and Growth Rate of RI",
    subtitle = "Linear regression with correlation coefficient",
    x = "Growth Rate of P (%)",
    y = "Growth Rate of RI (%)",
    caption = "Data source: Refinitiv Eikon"
  ) +
  theme_minimal(base_size = 14) +  # Use a clean theme
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed")
  )

print(dd_4)


# time to treatment

# add more colist()# add more company 
# company reports
# test different ways -------------------
# test -------------------
# at worst just present like this results 

model_9 = fixest::feols(
  fml = gr_P ~ l(dP, 1:10) + gr_RI +  n_sust + n_ai +  n_emis + n_ets +  
    i(ttt, ref = 0) | company + month(date)^year(date), 
  data = df_ew, panel.id = ~company + date)
modelsummary(model_9, stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2 Within|R2 Pseudo") # Exclude AIC, BIC, and other stats
iplot(model_9,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

model_10 = fixest::feols(
  fml = gr_P ~ l(dP, 1:10) + gr_MV +  n_sust + n_ai +  n_emis + n_ets +  
    i(ttt, ref = 0) | company + month(date)^year(date), 
  data = df_ew, panel.id = ~company + date)
iplot(model_10,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

modelsummary(list(
  "change variable" = model_4,
  "model_10" = model_10), stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1))


model_11 = fixest::feols(
  fml = gr_P ~ l(dP, 1:10) + dMV +  dRI +  n_sust + n_ai +  n_emis + n_ets +  
    i(ttt, ref = 0) +as.factor(lubridate::wday(date)) | company + month(date)^year(date), 
  data = df_ew, panel.id = ~company + date)
modelsummary(model_11, stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             gof_omit = "AIC|BIC|Log.Lik|RMSE| R2|R2 Within|R2 Pseudo") # Exclude AIC, BIC, and other stats
iplot(model_11,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model  


model_12 <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10) + gr_RI  + gr_MV + as.factor(lubridate::wday(date)) +
    i(ttt, ref = 0) | company + month(date)^year(date), 
  data = df_ew, panel.id = ~company + date)
modelsummary(model_12, stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             gof_omit = "AIC|BIC|Log.Lik|RMSE|R2 Within|R2 Pseudo") # Exclude AIC, BIC, and other stats

model_13 <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10) + gr_RI * i(ttt, ref=0) +gr_MV * i(ttt, ref = 0) +as.factor(lubridate::wday(date))
     | company + month(date)^year(date), 
  data = df_ew, panel.id = ~company + date)

# 2 ****

model_2 <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dMV + dRI + 
    as.factor(lubridate::wday(date)) +  
    i(ttt, ref = 0) | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

iplot(model_2, 
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

# 3

model_3 <- fixest::feols(
  fml = gr_P ~  l(dP, 1:10) + gr_RI  + gr_MV + 
    i(ttt, ref = 0) + as.factor(lubridate::wday(date)) | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

iplot(model_3,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model

# 4 ************

model_4 <- fixest::feols(
  fml = dP ~ l(dP, 1:10) + dRI * i(ttt, ref = 0) + dMV * i(ttt, ref = 0) +
    as.factor(lubridate::wday(date)) 
  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

iplot(model_4,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates(TWFE)') # Two way fixed effects model
model_d = fixest::feols(
  fml = dP ~ dMV + dRI | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)
model_g = fixest::feols(
  fml = gr_P ~   gr_RI + gr_MV  | company + month(date)^year(date), 
  data = df_ew, panel.id=~company+date)

modelsummary(
  list(
    "Model 1: Lagged Effects" = model_2,
    "Model 2: Lagged Effects" = model_4,
    "Model 3: Lagged Effects" = model_11,
    "Model 5: Rate of Return" = model_13,
    "Model 4: Rate of Return" = model_7
    ),
  coef_map = custom_variable_names, # Map variable names to custom labels
  stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1), # Add significance stars
  title = "Regression Results", # Add a title to the table
  gof_omit = "DF|AIC|BIC" # Exclude AIC, BIC, and other stats
  # output = "exports/table_actual.png" 
)



modelsummary(
  list(
    "Model 1: Lagged Effects" = model_2,
    "Model 2: Lagged Effects" = model_4,
    "Model 3: Lagged Effects" = model_11,
    "Model 4: Rate of Return" = model_12,
    "Model 5: Rate of Return" = model_7
  ),
  stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1), # Add significance stars
  title = "Regression Results", # Add a title to the table # Exclude just AIC, BIC, R2
  gof_omit = 'DF|Deviance|R2|AIC|BIC' 
)

iplot(model_2,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates Difference in variables (TWFE)') # Two way fixed effects model

iplot(model_12,
      xlab = 'Day to treatment',
      main = 'Event study: Reports Publishing dates Growth on variables (TWFE)') # Two way fixed effects model


# BE CAREFUL ABOUT R within 
# this what you need not R2 

# Check the creating regression first and articles
# is it better to work with whole data and NAs
# is it better filter down to just the companies with the reports
# if you can work with Rmarkdown and R script together
# control variables/ fixed effects YES YES YES instead of ( X  X  X )
# check the results and the tables
# if its too hard decrease the number of word you are looking for from reports 

custom_gof <- tibble::tribble(
  ~raw, ~clean, ~fmt,
  "r2", "R2", function(x) NULL,  # Remove standard R??
  "adj.r2", "Adjusted R??", function(x) NULL,  # Remove Adjusted R??
  "r2.within", "R2 Within", function(x) sprintf("%.3f", x),  # Keep R?? Within
  "fixed effects", "Fixed Effects", function(x) "YES",
  "control variables", "Control Variables", function(x) "YES"
)


modelsummary(
  list("Model 1" = model_1, "Model 2" = model_5),
  gof_map = custom_gof
)

modelsummary(model_1, stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             custom_gof)

             # gof_omit = "AIC|BIC|Log.Lik|RMSE|R2|R2 Pseudo|R2 Adj.",
             # gof = "R2 Within") # Exclude AIC, BIC, and other stats
# working on R2 within and proper model creation 
# there were some problem about number of words variables

modelsummary(model_5, stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1),
             statistic_override = list("Fixed Effects" = "YES", "Control Variables" = "YES"),
             gof_omit = "^(?!.*R2.within).*"  # Keeps only R?? Within
) # Exclude AIC, BIC, and other stats
             
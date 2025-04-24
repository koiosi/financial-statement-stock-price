# DAX Data And Annual Report dates data Combining -------------------

library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(readr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)


# Read in the DAX data
dax_data <- read_csv("Data_2/scratch_3.csv")
print(unique(dax_data$Company))

# Read in the Annual Report dates data

annual_data <- read_csv("Data_2/df_report.csv")

class(annual_data$File)

class(annual_data$Creationdate)




# as.date

# I tried to convert the date format of report dates posit to date
# I am trying to convert the date format of stock price date to posit

# Maybe you can ask Mr. Dannemann -------------- posit or date
# dax_data$Date <- as.POSIXct(dax_data$Date,  format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin")

class(annual_data$Creationdate)
class(dax_data$Date)

# yedek kodlar
annual_data$Creationdate <- as.Date(annual_data$Creationdate, format = "%d/%m/%Y")
# annual_data$LastModifiedDate <- as.Date(annual_data$LastModifiedDate, format = "%d/%m/%Y")


print(length(unique(dax_data$Company)))


# re-shape the annual_data
# Separate File into Company and Year
# or delete the part starting with "_"

annual_data <- annual_data %>%
  mutate(File = str_remove(File, "_.*"))

print(length(unique(annual_data$File)))
print((unique(annual_data$File)))
# here is the code for dividing into 2 pieces of column from "_"


# Combine the two datasets as annual report dates dummy variables

# Merge datasets
dax_data <- dax_data %>%
  mutate(
    publish_dummy = ifelse(
      paste(Company, Date) %in% paste(annual_data$File, annual_data$Creationdate),
      1,
      0
    )
  )



# Count matches and non-matches
?table
table(dax_data$publish_dummy)

print(length(unique(dax_data$Company)))

# all the companies at least once matched
print(unique(dax_data$Company[dax_data$publish_dummy == 1]))
print(length(unique(dax_data$Company[dax_data$publish_dummy == 1])))


print(unique(annual_data$File))
print(length(unique(annual_data$File)))




# comparison
match <- c(print(unique(dax_data$Company[dax_data$publish_dummy == 1])))

all_reports <- c( print(unique(annual_data$File)))

# check the difference
print(setdiff(all_reports, match))
print(unique(dax_data$Company))
# is this 5 in my whole data ?
unmatch <- print(setdiff(all_reports, match))

all_comp <- c(print(unique(dax_data$Company)))

table(dax_data$publish_dummy)




# these companies not in my report pool I can add more report

### now we can process this data in the next step

# Save the data as ready for regression with dummy, filtered company

write_csv(dax_data, "Data_2/data_dummy.csv")


# this part from document 4 --------------------------


# in this part we should prepare the data for regression

# load the data


df <- readr::read_csv("Data_2/data_dummy.csv")

df <- df %>% 
  dplyr::mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
  ) %>%
  dplyr::rename(company = Company, date=Date) %>%
  dplyr::arrange(
    company, date
  )

# maybe save

# I copied here what Dannemann did down there with actual data
df_ew <- df %>% 
  mutate (
    report_published = publish_dummy == 1,
    date_published = ifelse(report_published == 1, as.Date(date), as.Date("2010-01-01", format = "%Y-%m-%d")) %>% 
      as.Date()
  )
head(annual_data)
df_ew <- df_ew %>%
  mutate(
    n_sust = ifelse(report_published == 1, annual_data$sustainability, 0),
    n_ets = ifelse(report_published == 1, annual_data$ETS, 0),
    n_emis = ifelse(report_published == 1, annual_data$emission, 0),
    n_ai = ifelse(report_published == 1, annual_data$AI, 0)
  )


# load the freqeuncy data



?ifelse

summary(df_ew)

#| Either use:
#| - lubridate package (filter if date is in sequence)
#| - zoo package (apply rolling filter function)
#| - https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html

#  create a t variable t-7 to t+7






# check the data
table(df_ew$publish_dummy)
head(df_ew)
summary(df_ew)
str(df_ew)

# save the data

write_csv(df_ew, "Data_2/data_ew.csv")


# In case it is better result ---------------------
# filter out the companies that are not in the report

# df_ew2 <- df_ew %>%
#   filter(company %in% match)
# 

# write_csv(df_ew2, "Data_2/data_ew_with_report.csv")


df_ew <- df_ew %>%
  filter(company != "SIE" & company != "FRE")


df_ew <- df_ew %>%
  group_by(company) %>%
  mutate(
    ew = zoo::rollmax(report_published, k = 21, fill = NA, align = "center"),
  )

write_csv(df_ew, "new/1_data/merg_df.csv")



# I will use this data in the next step

df_ew <- df_ew %>%
  mutate(
    mean_sust = zoo::rollmean(n_sust, k = 21, fill = NA, align = "center"),
    mean_ets = zoo::rollmean(n_ets, k = 21, fill = NA, align = "center"),
    mean_emis = zoo::rollmean(n_emis, k = 21, fill = NA, align = "center"),
    mean_ai = zoo::rollmean(n_ai, k = 21, fill = NA, align = "center")
  )


?zoo::rollmean


# NOT sure about n_sust, n_ets, n_emis, n_ai
# in the data ADS' 2010 n_sust, n_ets, n_emis, n_ai are matching with DB1s data
# rolling code maybe mistaking


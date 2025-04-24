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

annual_data <- read_csv("Data_2/rep_dates.csv")

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
annual_data$LastModifiedDate <- as.Date(annual_data$LastModifiedDate, format = "%d/%m/%Y")


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

# # filter out SIE and FRE
dax_data <- dax_data %>%
  filter(Company != "SIE" & Company != "FRE")

annual_data <- annual_data %>%
  filter(File != "SIE" & File != "FRE")

#####

# I think there are 386 matches in the data,
# Maybe I should delete the companies without 15years report data(2010-2023)

######


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

uniq_rep <- unique(annual_data) %>%
  group_by(File) %>%
  summarise(n = n())

# some weird situation in SIE and FRE
##### Matched companies and how many times they are matched

report <- dax_data %>%
  filter(publish_dummy == 1) %>%
  group_by(Company) %>%  # Group by company
  summarise(match_count = n()) %>%  # Count occurrences per company
  ungroup()

report

# these companies not in my report pool I can add more report

### now we can process this data in the next step



# Save the data as ready for regression with dummy, filtered company

write_csv(dax_data, "Data_2/data_dummy_2.csv")
# write_csv(dax_data, "new/data_dummy_2.csv")

# this part from document 4 --------------------------


# in this part we should prepare the data for regression

# load the data


df <- readr::read_csv("Data_2/data_dummy_2.csv")

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




?ifelse

summary(df_ew)

#| Either use:
#| - lubridate package (filter if date is in sequence)
#| - zoo package (apply rolling filter function)
#| - https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html

#  create a t variable t-7 to t+7



df_ew <- df_ew %>%
  group_by(company) %>%
  mutate(
    ew = zoo::rollmax(report_published, k = 15, fill = NA, align = "center"),
  )



# check the data
table(df_ew$publish_dummy)
head(df_ew)
summary(df_ew)
str(df_ew)

# save the data

write_csv(df_ew, "Data_2/data_ew_2.csv")
# write_csv(df_ew, "new/data_ew_2.csv")

# In case it is better result ---------------------
# filter out the companies that are not in the report

# df_ew2 <- df_ew %>%
#   filter(company %in% match)
# 
# write_csv(df_ew2, "Data_2/data_ew_with_report.csv")


# Applied econonmics ------------------

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(readr)
library(ggplot2)

# write a working directory path

# setwd("C:/R_folder")

# getwd()

?read_xlsx

data_raw <- read_excel(path = "C:/R_folder/Data/data.xlsx", skip = 1) %>% 
  dplyr::select(-starts_with('...'))

# there was a problem with the column names, 
# so I removed the one starting with "..."

str(data_raw)

head(data_raw)
# Data processing ------------------------------------------------------------


# remove the long headers instead use the code row as header and clean the "D:" part
data_clean <- data_raw %>% rename_with(~str_remove_all(., "D:"))
head(data_clean)

# Clean column names: replace parentheses with a separator like "_"

colnames(data_clean) <- gsub("\\(|\\)", "_", colnames(data_clean))  # Replace "(" and ")" with "_"
colnames(data_clean) <- gsub("_$", "", colnames(data_clean))        # Remove double underscores if they occur Removes trailing "_"
head(data_clean)
colSums(is.na(data_clean))
str(data_clean)


# there are 2 columns with NA values [970, 1117]

# make all the column names as.character
data_clean <- data_clean %>%
  mutate(across(everything(), as.character))

colSums(is.na(data_clean))

#> this data is too long try to shape it with pivot longer
#> its always repetition first company name and then the notation of the data 
#> these are the notations 
#>
#>

# data_long <- data_clean %>%
#   pivot_longer(cols = -Code,  # Adjust column names accordingly
#                names_to =  c("Company", "DataCode"),
#                names_sep = "_",
#                values_to = "value") %>%
#   mutate(value = as.numeric(value))  # Convert to numeric after pivoting


data_long <- data_clean %>%
  pivot_longer(
    cols = -Code,
    names_to = c("Company", "DataCode"),
    names_sep = "_",
    values_to = "Value"
  )

str(data_long)
colSums(is.na(data_long))

# where are these NA values



# save the long data

write_csv(data_long, "C:/R_folder/Data_2/scratch_long.csv")
# write_csv(data_long, "C:/R_folder/new/scratch_long.csv")

head(data_long)

glimpse(data_long)
str(data_long)
summary(data_long)

# check the missing values
colSums(is.na(data_long))

com_na <- data_long %>%
  filter(is.na(Value)) %>%
  group_by(Company) %>%
  summarise(n = n())

print(sum(com_na$n))

sapply(data_long, class)


# Count unique companies and data codes
length(unique(data_long$Company))  # Number of unique companies
length(unique(data_long$DataCode))  # Number of unique data codes

# company names
print(unique(data_long$Company))

# There is also NA as Datacode!!!

print(unique(data_long$DataCode))



# which companies has NA values

data_long %>%
  filter(is.na(Value)) %>%
  group_by(Company) %>%
  summarise(n = n())

# check the missing values
colSums(is.na(data_long))

length(unique(data_long$Company))

# how many values per company

comp <- c(
  "Z29", "UN0", "SHL", "P911",
  "KBX", "HLAG", "ENR", "DTG",
  "DHER", "8TRA", "123F", "1COV",
  "TLX", "VNA")

# filter out this companies "comp"

data_long <- data_long %>%
  filter(!Company %in% comp)

print(unique(data_long$Company))
colSums(is.na(data_long))



# save the cleaned data, without NAs 

# write.csv(data_long, "C:/R_folder/Data/cleaned_long_data_NA.csv", row.names = FALSE)

write.csv(data_long, "C:/R_folder/Data_2/scratch_long_2.csv", row.names = FALSE)
# write_csv(data_long, "C:/R_folder/new/scratch_long_2.csv")

colSums(is.na(data_long))

# SECOND DOC START HERE ------------------------------------------

getwd()

# This is the second script for the DAX and P filter visualization
# here I tried to do the following:
# filtered the data for P, MV, RI
# filtered the data for not exactly for DAX 40 companies
#  # but for the companies that are in the DAX 40 list 33 of them + 9 more





data <- read.csv("./Data_2/scratch_long_2.csv")

head(data)

print(unique(data$Company))
print(unique(data$DataCode))

colnames(data)


df <- data %>% 
  tidyr::pivot_wider(names_from = DataCode, values_from = Value)

# corr matrix
# Assuming your data frame is named `df` and contains the specified variables
correlation_matrix <- cor(df[, c(
  "P", "MV", "DY", "PE", "PI",
  "PH", "PL", "PO", "VA", "VO",
  "UP", "VWAP", "PA", "PB", "UPA",
  "UPB", "UPH", "UPL", "UPO", "UVO", "RI")], use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)
corrplot::corrplot(correlation_matrix, method = "shade", type = "upper", tl.col = "black", tl.srt = 45)

# remove rows according to their value
# filter data by ("P", "MV", "RI", "DY", "PE", "VA", "VO")

df <- df %>% 
  select( "Code", "Company", "P", "MV", "RI", "DY", "PE", "VA", "VO")

correlation_matrix_2 <- cor(df[, c(
  "P", "MV", "RI", "DY", "PE", "VA", "VO")], use = "complete.obs")

corrplot::corrplot(correlation_matrix_2, method = "shade", type = "upper", tl.col = "black", tl.srt = 45)


# after seeing P,  MV, RI coming down under each other we make pivot wider

# df <- data_filtered %>% 
#   tidyr::pivot_wider(names_from = DataCode, values_from = Value)

# check the NAs per column

colSums(is.na(df))
# MAYBE P ~ RI + DY ------------------------

class(df$Code)


companies <- c(print(unique(df$Company)))

# Complete List of DAX 40 Companies and Their Tickers (without .DE)-----
dax40 <- c(
  "ADS",   # Adidas
  "AIR",   # Airbus
  "ALV",   # Allianz
  "BAS",   # BASF
  "BAYN",  # Bayer
  "BEI",   # Beiersdorf
  "BMW",   # BMW
  "BNR",   # Brenntag
  "CON",   # Continental
  "1COV",  # Covestro
  "DBK",   # Deutsche Bank
  "DHL",   # Deutsche Post (DHL)
  "DTE",   # Deutsche Telekom
  "EOAN",  # E.ON
  "FME",   # Fresenius Medical Care
  "FRE",   # Fresenius
  "HEI",   # Heidelberg Materials
  "HEN3",  # Henkel
  "IFX",   # Infineon Technologies
  "LIN",   # Linde (Now on NYSE, but was part of DAX)
  "MBG",   # Mercedes-Benz (Daimler)
  "MRK",   # Merck KGaA
  "MTX",   # MTU Aero Engines
  "MUV2",  # Munich Re (M??nchener R??ckversicherung)
  "PAH3",  # Porsche Automobil Holding
  "PUM",   # Puma
  "QIA",   # Qiagen
  "RHM",   # Rheinmetall
  "RWE",   # RWE
  "SAP",   # SAP
  "SRT3",  # Sartorius
  "SIE",   # Siemens
  "SY1",   # Symrise
  "VNA",   # Vonovia
  "VOW3",  # Volkswagen (Preferred Shares)
  "ZAL",   # Zalando
  "ENR",   # Siemens Energy
  "HFG",   # HelloFresh
  "SHL"    # Siemens Healthineers
)

# Print the tickers
print(dax40)
# could be unnecessary above /----- 

# what I have from dax in my dataset
dif_dax_data <- unlist(setdiff(companies, dax40))

# my_company_list
dax_40_companies = c(
  "SAP", "SIE", "DTE", "ALV", "MBG", "BMW", "MUV2", "ADS", "BAS", "IFX",
  "VOW", "DB1", "DBK", "DHL", "BEI", "EOAN", "RHM", "RWE", "BAYN", "FRE",
  "HEI", "MRK", "CON", "MTX", "PAH3", "SY1", "FME", "PUM")

print(unique(df$Company))
print(length(unique(df$Company)))

?length

summary(df)
str(df)

# check NAs

colSums(is.na(df))

# Visualization just from the data --------------------------------------------

# visualize the Siemens stock price over time

colSums(is.na(df))
# Code Company       P      MV      RI 
# 0       0     123    1718     123 

# rename code to date and change to date class

df <- df %>% 
  rename(Date = Code)

# change the class of Date to date

df$Date <- as.Date(df$Date, format = "%Y-%m-%d")

#  HERE
?Date

class(df$Date)

df %>% filter(Company == "SIE") %>%
  ggplot(aes(x = (Date), y = P)) + 
  geom_line() + theme_minimal() + 
  labs(title = "Siemens Stock Price", x = "Date", y = "Stock Price")

# Visualize the SAP stock price over time
df %>% filter(Company == "SAP") %>%
  ggplot(aes(x = (Date), y = P)) +
  geom_line() +
  labs(title = "SAP Price Over Time", x = "Date", y = "Price")

# Visualize the stock price of all 46 companies in my data over time

df %>%
  ggplot(aes(x = (Date), y = P, color = Company)) +
  geom_line() +
  labs(title = "Stock Prices of DAX 40 Companies", x = "Date", y = "Stock Price") +
  theme_minimal()

# save the data

write.csv(df, "./Data_2/scratch_3.csv", row.names = FALSE)
# write_csv(data_long, "C:/R_folder/new/scratch_3.csv")


# statistical analysis of variables --------------------------------------------------------

# calculate the mean stock price of each company

df %>%
  group_by(Company) %>%
  summarize(mean_price = mean(P))

# calculate the median stock price of each company

df %>%
  group_by(Company) %>%
  summarize(median_price = median(P))

# calculate the standard deviation of stock price of each company

df %>%
  group_by(Company) %>%
  summarize(sd_price = sd(P))

# calculate the minimum stock price of each company

df %>%
  group_by(Company) %>%
  summarize(min_price = min(P))

print(unique(df$Company))

# check the correlation between stock prices and market value

cov(df$P, df$MV, use = "complete.obs")
cor(df$P, df$MV, use = "complete.obs")

# check the correlation between stock prices and return on investment

cov(df$P, df$RI, use = "complete.obs")
cor(df$P, df$RI, use = "complete.obs")

# check the correlation between market value and return on investment

cov(df$MV, df$RI, use = "complete.obs")
cor(df$MV, df$RI, use = "complete.obs")



# plot heatmap of correlation matrix

cor_matrix <- cor(df[, c("P", "MV", "RI")], use = "complete.obs")

cor_matrix <- cor(df[, c("P", "MV", "RI")], use = "complete.obs")

cor_matrix

heatmap_cor <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "Company") %>%
  pivot_longer(cols = -Company, names_to = "Variable", values_to = "Correlation") %>%
  ggplot(aes(x = Company, y = Variable, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(
    title = "Corr Matrix of Stock Prices, Market Value, and Return on Investment",
    x = "Company",
    y = "Variable"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(heatmap_cor)

# intro literature data results
colnames(df)
ADS_1 <- df %>%
  filter(Company == "ADS") %>%  # Filter for the company "ADS"
  ggplot(aes(x = P, y = DY)) +  # Map P to x-axis and MV to y-axis
  geom_point(alpha = 0.6, color = "blue") +  # Use geom_point() for a scatterplot
  theme_minimal() +  # Use a minimal theme
  labs(
    title = "ADS: Stock Price vs Market Value",  # Corrected title
    x = "Stock Price (P)",  # Corrected x-axis label
    y = "Dividend Yield (DY)"  # Corrected y-axis label
  )

print(ADS_1)


ADS_2 <- df %>%
  filter(Company == "ADS") %>%
  ggplot(aes(x = P, y = MV, color = DY)) +  # Color points by DY
  geom_point(alpha = 0.6) +
  labs(
    title = "ADS: Stock Price vs Market Value",
    x = "Stock Price (P)",
    y = "Market Value (MV)",
    color = "Dividend Yield"  # Add a legend title
  ) +
  theme_minimal()
print(ADS_2)


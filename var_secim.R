# R Code 

# Load required libraries
library(tidyverse)
library(car)

# Read the CSV file
df <- read.csv("C:/R_folder/new/1_data/scratch_3.csv")

# Step 1: Drop rows with NA values in key columns
df_clean <- df %>%
  drop_na(P, RI, MV, VO)

# Step 2: Check correlation with the dependent variable (P)
cor(df_clean %>% select(P, RI, MV, VO, DY, PE, VA), use = "complete.obs")

# Step 3: Fit initial linear regression model
model1 <- lm(P ~ RI + MV + VO + DY + PE + VA, data = df_clean)
model2 <- lm(P ~ 
               # RI + MV, 
             VO + DY + PE,
             data = df_clean)
model3 <- lm(MV ~ P + VO + DY + PE,
             data = df_clean)
summary(model1)

# Step 4: Check for multicollinearity (Variance Inflation Factor)
vif(model1)
vif(model2)
vif(model3)
# Step 5 (optional): Diagnostic plots
par(mfrow = c(2, 2))
plot(model1)

modelsummary::modelsummary(list(model1, model2, model3), 
                           stars = c("***"= 0.01, "**" = 0.05, "*" = 0.1))



### CORR map 

# Load libraries
library(tidyverse)
library(Hmisc)       # For rcorr()
library(xtable)      # For LaTeX export


# Select numeric variables
numeric_df <- df %>%
  select(P, MV, RI, DY, PE, VA, VO) %>%
  drop_na()

# Compute correlations and p-values
cor_results <- rcorr(as.matrix(numeric_df), type = "pearson")
cor_matrix <- cor_results$r
p_matrix <- cor_results$P

# Function to add stars based on p-values
format_apa <- function(r, p) {
  stars <- ifelse(p < 0.01, "***", 
                  ifelse(p < 0.05, "**", 
                         ifelse(p < 0.10, "*", "")))
  paste0(sprintf("%.4f", r), stars)
}

# Apply formatting
formatted_matrix <- matrix("", nrow = ncol(cor_matrix), ncol = ncol(cor_matrix))
rownames(formatted_matrix) <- colnames(formatted_matrix) <- colnames(cor_matrix)

for (i in 1:ncol(cor_matrix)) {
  for (j in 1:ncol(cor_matrix)) {
    if (i == j) {
      formatted_matrix[i, j] <- "1.0000***"
    } else if (i > j) {
      formatted_matrix[i, j] <- format_apa(cor_matrix[i, j], p_matrix[i, j])
    } else {
      formatted_matrix[i, j] <- ""
    }
  }
}

# Convert to data frame for xtable
formatted_df <- as.data.frame(formatted_matrix)
formatted_df <- tibble::rownames_to_column(formatted_df, "Variable")

# Create LaTeX table
latex_table <- xtable(formatted_df, caption = "Pearson correlations between key variables", label = "tab:correlation", align = "llccccccc")

# Print LaTeX code
print(latex_table, include.rownames = FALSE, sanitize.text.function = identity)


library(tidyverse)
data_2013 <- read.csv('selected_2013.csv')
data_2015 <- read.csv('selected_2015.csv')
#summary_output <- dfSummary(df7, title.freq = "Summary of data", style = "grid",plain.ascii = FALSE) 
#view(summary_output, file = "summary_mice.html")
df <- rbind(data_2013, data_2015)
df <- df %>% select(-X)

# CHANGE NA TO "NO"
df2 <- df %>%
  mutate(
    DIQ175U.x = replace_na(DIQ175U.x, "No"),
    DIQ175V.x = replace_na(DIQ175V.x, "No")
  )

# REMOVE VARIABLES WITH HIGH SHARE OF NA'S
df3 <- df2 %>% select(-c("ALQ110", "SMQ040", "year"))
# MAKE "REFUSED" ANSWER NA
df4 <- df3 %>%
  mutate(across(where(is.character), ~ na_if(.x, "Refused")))
# MAKE CHARACTER VARIABLE FACTOR
df5 <- df4 %>%
  mutate(across(where(is.character), as.factor))

# list of NA var 
library(dplyr)
library(mice)
library(forcats) # for fct_lump_n if needed

# 1️⃣ Keep only columns with NA
na_vars <- df6 %>%
  select(where(~ any(is.na(.))))

# 2️⃣ Build method vector
meth <- make.method(na_vars)

# Numeric → pmm
meth[sapply(na_vars, is.numeric)] <- "pmm"

# Binary factors → logreg
meth[sapply(na_vars, function(x) is.factor(x) && nlevels(x) == 2)] <- "logreg"

# Small multiclass factors → polyreg
meth[sapply(na_vars, function(x) is.factor(x) && nlevels(x) > 2 & nlevels(x) <= 20)] <- "polyreg"

# Large factors (> 20 levels) → pmm (avoids nnet weight explosion)
meth[sapply(na_vars, function(x) is.factor(x) && nlevels(x) > 20)] <- "pmm"

# 3️⃣ Run MICE
imp <- mice(na_vars, m = 5, method = meth, seed = 123)

# 4️⃣ Replace imputed values into original df
df6_imputed <- df6
df6_imputed[names(na_vars)] <- complete(imp)

# 5️⃣ Optional: print summary of imputation methods used
imputed_summary <- data.frame(
  Variable = names(meth),
  Method = meth
)
print(imputed_summary)
eig_df <- data.frame(
  Dim = 1:nrow(famd_result$eig),
  Variance = famd_result$eig[, 2]
)

ggplot(eig_df, aes(x = Dim, y = Variance)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(Variance, 2)), vjust = -0.5) +
  labs(x = "Dimension", y = "% of Variance Explained",
       title = "Scree Plot (FAMD)") +
  theme_minimal()

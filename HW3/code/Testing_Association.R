# Load necessary libraries
library(tidyverse)

setwd("../HW3/")
#load data
cleaned_data <- read.table("clean_data/PlateletHW_cleaned.tsv", header = TRUE, sep = "\t")
#summary statistics for ADP
summary(cleaned_data$ADP)

#QQ plot for ADP and log(ADP)
ggplot(cleaned_data, aes(sample = ADP)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  ggtitle("QQ Plot for ADP")

cleaned_data$log_ADP <- log(cleaned_data$ADP)

ggplot(cleaned_data, aes(sample = log_ADP)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("QQ Plot for log(ADP)")

#QQ plot for the association between the log(ADP) values and genotypes
ggplot(cleaned_data, aes(x = rs4244285, y = log_ADP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "Association between log(ADP) and rs4244285 genotype",
       x = "rs4244285 Genotype (0, 1, 2)",
       y = "log(ADP)") +
  theme_minimal()

ggplot(cleaned_data, aes(x = rs4986893, y = log_ADP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Association between log(ADP) and rs4986893 genotype",
       x = "rs4986893 Genotype (0, 1, 2)",
       y = "log(ADP)") +
  theme_minimal()

ggplot(cleaned_data, aes(x = rs662, y = log_ADP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "purple") +
  labs(title = "Association between log(ADP) and rs662 genotype",
       x = "rs662 Genotype (0, 1, 2)",
       y = "log(ADP)") +
  theme_minimal()

#Single Linear Regression for each SNP
model_rs4244285 <- lm(log_ADP ~ rs4244285, data = cleaned_data)
summary(model_rs4244285)

model_rs4986893 <- lm(log_ADP ~ rs4986893, data = cleaned_data)
summary(model_rs4986893)

model_rs662 <- lm(log_ADP ~ rs662, data = cleaned_data)
summary(model_rs662)

# Multiple linear regression including covariates
model_SNPs <- lm(log_ADP ~ rs4244285 + rs4986893 + rs662, data = cleaned_data)
summary(model_SNPs)

model_all_variables <- lm(log_ADP ~ rs4244285 + rs4986893 + rs662 + Resistance + AGE + SEX, data = cleaned_data)
summary(model_all_variables)

#Logistic regression to examine the association between Resistance and three SNPs
model_resistance <- glm(Resistance ~ rs4244285 + rs4986893 + rs662, family = binomial, data = cleaned_data)
summary(model_resistance)



#!/usr/bin/env Rscript
# Multilevel Mediation Analysis: Flow Mediation of Game Effects on Pain Unpleasantness
# 
# This script performs multilevel mediation analysis to test whether Flow mediates 
# the effects of game conditions (Game MP vs. Game LP, 2-back, and LR) on pain 
# unpleasantness. The analysis uses a multilevel modeling approach with bootstrapping 
# to estimate indirect effects and their confidence intervals.
#
# Dependencies: readxl, nlme, tidyr, boot

library(readxl)
library(nlme)
library(tidyr)
library(boot)

options(scipen = 999)

# Data Loading and Preparation

data <- read_excel("Game2.xlsx")
data <- as.data.frame(data)

data$TrialType <- factor(data$TrialType, 
                        levels = c("GP", "2back", "GNP", "LR"))

contrasts(data$TrialType) <- contr.treatment(4)
dummyVars <- model.matrix(~ data$TrialType)[, -1]
dummyVars <- -dummyVars
colnames(dummyVars) <- c("GP_vs_2back", "GP_vs_GNP", "GP_vs_LR")

# Data Restructuring for Multilevel Mediation

tmp <- data.frame(
  X1 = dummyVars[, "GP_vs_GNP"],
  X2 = dummyVars[, "GP_vs_2back"],
  X3 = dummyVars[, "GP_vs_LR"],
  Y = data$Unpleasantness,
  M = data$Flow,
  L2id = as.factor(data$SubjectID),
  Md = data$Flow
)

tmp <- tidyr::pivot_longer(tmp, cols = c(Y, M), names_to = "Outcome", values_to = "Z")

tmp$Sy <- ifelse(tmp$Outcome == "Y", 1, 0)
tmp$Sm <- ifelse(tmp$Outcome == "M", 1, 0)

tmp$SmX1 <- tmp$Sm * tmp$X1
tmp$SmX2 <- tmp$Sm * tmp$X2
tmp$SmX3 <- tmp$Sm * tmp$X3
tmp$SyX1 <- tmp$Sy * tmp$X1
tmp$SyX2 <- tmp$Sy * tmp$X2
tmp$SyX3 <- tmp$Sy * tmp$X3
tmp$SyM <- tmp$Sy * tmp$Md

# Model Fitting

fixed.formula <- "Z ~ 0 + Sm + Sy + SmX1 + SmX2 + SmX3 + SyX1 + SyX2 + SyX3 + SyM"
random.formula <- "~ 0 + Sm + Sy | L2id"
mod_med_tmp <- lme(
  fixed = as.formula(fixed.formula),
  random = as.formula(random.formula),
  weights = varIdent(form = ~ 1 | Sm),
  data = tmp,
  control = lmeControl(opt = "nlm")
)

# Bootstrap Analysis

boot_fun <- function(data, indices) {
  boot_data <- data[indices,]
  
  mod <- try(lme(
    fixed = as.formula(fixed.formula),
    random = as.formula(random.formula),
    weights = varIdent(form = ~ 1 | Sm),
    data = boot_data,
    control = lmeControl(opt = "nlm")
  ), silent = TRUE)
  
  if(inherits(mod, "try-error")) {
    return(rep(NA, 15))
  }
  
  coef <- fixef(mod)
  
  indirect1 <- coef["SmX1"] * coef["SyM"]
  indirect2 <- coef["SmX2"] * coef["SyM"]
  indirect3 <- coef["SmX3"] * coef["SyM"]
  
  diff12 <- indirect1 - indirect2
  diff13 <- indirect1 - indirect3
  diff23 <- indirect2 - indirect3
  
  return(c(coef, indirect1, indirect2, indirect3, diff12, diff13, diff23))
}

set.seed(123)
boot_result <- boot(
  data = tmp,
  statistic = boot_fun,
  R = 5000
)

# Results Compilation

coef_matrix <- summary(mod_med_tmp)$tTable
boot_ci <- t(apply(boot_result$t, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE))
results <- data.frame(
  Predictor = c(
    "Outcome: Pain Unpleasantness",
    "(Intercept)",
    "(Intercept)",
    "a₁ (GP vs GNP → Flow)",
    "a₂ (GP vs 2back → Flow)",
    "a₃ (GP vs LR → Flow)",
    "c'₁ (GP vs GNP → Pain Unpleasantness)",
    "c'₂ (GP vs 2back → Pain Unpleasantness)",
    "c'₃ (GP vs LR → Pain Unpleasantness)",
    "b (Flow → Pain Unpleasantness)",
    "Indirect effect (ab₁: GP vs GNP)",
    "Indirect effect (ab₂: GP vs 2back)",
    "Indirect effect (ab₃: GP vs LR)",
    "Difference of indirect effects (ab₁ - ab₂)",
    "Difference of indirect effects (ab₁ - ab₃)",
    "Difference of indirect effects (ab₂ - ab₃)"
  ),
  Estimate = c(
    NA,
    coef_matrix["Sm", "Value"],
    coef_matrix["Sy", "Value"],
    coef_matrix["SmX1", "Value"],
    coef_matrix["SmX2", "Value"],
    coef_matrix["SmX3", "Value"],
    coef_matrix["SyX1", "Value"],
    coef_matrix["SyX2", "Value"],
    coef_matrix["SyX3", "Value"],
    coef_matrix["SyM", "Value"],
    boot_result$t0[10],
    boot_result$t0[11],
    boot_result$t0[12],
    boot_result$t0[13],
    boot_result$t0[14],
    boot_result$t0[15]
  ),
  Lower = c(NA, boot_ci[,1]),
  Upper = c(NA, boot_ci[,2]),
  DF = c(NA, rep(1418, 9), rep(NA, 6)),
  t = c(NA, coef_matrix[,"t-value"], rep(NA, 6)),
  P = c(
    NA,
    coef_matrix[,"p-value"],
    sapply(10:15, function(i) 2 * min(mean(boot_result$t[,i] >= 0, na.rm = TRUE), 
                                     mean(boot_result$t[,i] <= 0, na.rm = TRUE)))
  )
)

results$Stars <- ifelse(results$P < 0.001, "***",
                       ifelse(results$P < 0.01, "**",
                             ifelse(results$P < 0.05, "*", "")))

# Print and Save Results

cat("\nTable: Estimates from multilevel mediation model\n")
cat("Comparing GP (reference) vs 2back, GNP, and LR conditions\n")
cat("Outcome variable: Pain Unpleasantness\n\n")

cat(sprintf("%-40s %10s %20s %6s %8s %10s\n",
            "Predictor", "Estimate", "95% CI", "DF", "t", "P"))
cat(sprintf("%40s %10s %10s %10s\n", "", "", "Lower", "Upper"))
cat(rep("-", 100), "\n")

for(i in 1:nrow(results)) {
  if(is.na(results$Estimate[i])) {
    cat(sprintf("%-40s\n", results$Predictor[i]))
  } else if(!is.na(results$DF[i])) {
    cat(sprintf("%-40s %10.2f %10.2f %10.2f %6d %8.2f %10.3f%s\n",
                results$Predictor[i],
                results$Estimate[i],
                results$Lower[i],
                results$Upper[i],
                results$DF[i],
                results$t[i],
                results$P[i],
                results$Stars[i]))
  } else {
    cat(sprintf("%-40s %10.2f %10.2f %10.2f %6s %8s %10.3f%s\n",
                results$Predictor[i],
                results$Estimate[i],
                results$Lower[i],
                results$Upper[i],
                "",
                "",
                results$P[i],
                results$Stars[i]))
  }
}

cat("\nSignificance codes: *** p<0.001, ** p<0.01, * p<0.05\n")

write.csv(results, "mediation_results_unpleasantness.csv", row.names = FALSE)
cat("\nResults saved to 'mediation_results_unpleasantness.csv'\n") 

#!/usr/bin/env Rscript
# Repeated Measures ANOVA Analysis: Game Experience Questionnaire (GEQ)
# 
# This script performs repeated measures ANOVA analyses to examine the effects of 
# different trial types on game experience 
# questionnaire components (Flow, Positive Affect, Negative Affect, Immersion, 
# Competence, Tension, Challenge).
#
# Dependencies: readxl, dplyr, tidyr, car, afex, emmeans, ggplot2

library(readxl)
library(dplyr)
library(tidyr)
library(car)
library(afex)
library(emmeans)
library(ggplot2)

options(scipen = 999)

data <- read_excel("Game.xlsx")
data <- as.data.frame(data)

data$TrialType <- factor(data$FinalTrialType...47, 
                        levels = c("GP", "GNP", "2back", "LR"),
                        labels = c("Game MP", "Game LP", "2-back", "LR"))

# Data Preparation for RM-ANOVA
geq_vars <- c("Flow", "PositiveAffect", "NegativeAffect", "Immersion", 
              "Competence", "Tension", "Challenge")

within_factors <- data.frame(
  condition = factor(1:4, labels = c("Game.MP", "Game.LP", "2.back", "LR"))
)

# Function to run GEQ analysis for each variable

run_geq_analysis <- function(var_name, data) {
  cat(paste("\n=== ", toupper(var_name), " ANALYSIS ===\n\n"))
  
  data_wide <- data %>%
    group_by(SubjectID, TrialType) %>%
    summarise(!!var_name := mean(get(var_name), na.rm = TRUE)) %>%
    pivot_wider(names_from = TrialType, values_from = !!var_name) %>%
    ungroup()
  
  names(data_wide) <- make.names(names(data_wide), unique = TRUE)
  names(data_wide) <- gsub("^X", "", names(data_wide))
  
  model <- lm(cbind(Game.MP, Game.LP, `2.back`, LR) ~ 1, data = data_wide)
  aov_model <- Anova(model, idata = within_factors, idesign = ~condition, type = "III")
  sphericity <- summary(aov_model, multivariate = FALSE)
  mauchly_p <- sphericity$sphericity.tests[1, "p-value"]
  gg_eps <- sphericity$pval.adjustments[1, "GG eps"]
  
  cat("Sphericity Test: Mauchly's W =", round(sphericity$sphericity.tests[1, "Test statistic"], 3), 
      ", p =", round(mauchly_p, 3), "\n")
  
  if (mauchly_p < 0.05) {
    cat("Sphericity VIOLATED → Using GG correction (ε =", round(gg_eps, 3), ")\n")
    f_value <- sphericity$univariate.tests[2, "F value"]
    df_num <- sphericity$univariate.tests[2, "num Df"] * gg_eps
    df_den <- sphericity$univariate.tests[2, "den Df"] * gg_eps
    p_value <- sphericity$pval.adjustments[1, "Pr(>F[GG])"]
    cat("F(", round(df_num, 1), ",", round(df_den, 1), ") =", round(f_value, 3), 
        ", p =", format(p_value, scientific = TRUE), " [GG corrected]\n")
  } else {
    cat("Sphericity NOT VIOLATED → No correction needed\n")
    f_value <- sphericity$univariate.tests[2, "F value"]
    df_num <- sphericity$univariate.tests[2, "num Df"]
    df_den <- sphericity$univariate.tests[2, "den Df"]
    p_value <- sphericity$univariate.tests[2, "Pr(>F)"]
    cat("F(", df_num, ",", df_den, ") =", round(f_value, 3), 
        ", p =", format(p_value, scientific = TRUE), " [uncorrected]\n")
  }
  
  ss_effect <- sphericity$univariate.tests[2, "Sum Sq"]
  ss_error <- sphericity$univariate.tests[2, "Error SS"]
  eta_sq <- ss_effect / (ss_effect + ss_error)
  cat("ηp² =", round(eta_sq, 3), "\n\n")
  
  cat("Post-hoc Tests (All Pairwise):\n")
  means <- colMeans(data_wide[,2:5], na.rm=TRUE)
  ms_error <- sphericity$univariate.tests[2, "Error SS"] / sphericity$univariate.tests[2, "den Df"]
  n_subjects <- nrow(data_wide)
  
  pairs <- data.frame(
    contrast = c("Game MP - Game LP", "Game MP - 2-back", "Game MP - LR",
                 "Game LP - 2-back", "Game LP - LR", "2-back - LR"),
    estimate = c(means[1] - means[2], means[1] - means[3], means[1] - means[4],
                 means[2] - means[3], means[2] - means[4], means[3] - means[4])
  )
  pairs$se <- sqrt(2 * ms_error / n_subjects)
  pairs$t.ratio <- pairs$estimate / pairs$se
  pairs$p.value <- 2 * pt(abs(pairs$t.ratio), df = sphericity$univariate.tests[2, "den Df"], lower.tail = FALSE)
  pairs$p.value.adj <- p.adjust(pairs$p.value, method = "holm")
  
  cat(" contrast           estimate   SE  df t.ratio p.value\n")
  for(i in 1:nrow(pairs)) {
    cat(sprintf("%-18s %8.2f %5.2f %2.0f %7.3f %7.3f\n",
                pairs$contrast[i], pairs$estimate[i], pairs$se[i],
                sphericity$univariate.tests[2, "den Df"],
                pairs$t.ratio[i], pairs$p.value.adj[i]))
  }
  cat("P value adjustment: holm method for 6 tests\n")
  
  cat("\nPlanned Contrasts (3 comparisons):\n")
  planned <- data.frame(
    contrast = c("TwoBack_vs_LR", "GameLP_vs_2back", "GameMP_vs_GameLP"),
    estimate = c(means[3] - means[4], means[2] - means[3], means[1] - means[2])
  )
  planned$se <- sqrt(2 * ms_error / n_subjects)
  planned$t.ratio <- planned$estimate / planned$se
  planned$p.value <- 2 * pt(abs(planned$t.ratio), df = sphericity$univariate.tests[2, "den Df"], lower.tail = FALSE)
  planned$p.value.adj <- p.adjust(planned$p.value, method = "holm")
  
  cat(" contrast         estimate   SE  df t.ratio p.value\n")
  for(i in 1:nrow(planned)) {
    cat(sprintf("%-16s %8.2f %5.2f %2.0f %7.3f %7.3f\n",
                planned$contrast[i], planned$estimate[i], planned$se[i],
                sphericity$univariate.tests[2, "den Df"],
                planned$t.ratio[i], planned$p.value.adj[i]))
  }
  cat("P value adjustment: holm method for 3 tests\n")
  
  return(list(
    var_name = var_name,
    means = means,
    sphericity = sphericity,
    mauchly_p = mauchly_p,
    gg_eps = gg_eps,
    f_value = f_value,
    df_num = df_num,
    df_den = df_den,
    p_value = p_value,
    eta_sq = eta_sq,
    pairs = pairs,
    planned = planned
  ))
}

# Run analysis for all GEQ variables

results <- list()
for(var in geq_vars) {
  results[[var]] <- run_geq_analysis(var, data)
}

# Create Line Plots for all GEQ variables

cat("\n=== CREATING LINE PLOTS ===\n")

create_geq_plot <- function(var_name, data) {
  plot_data <- data %>%
    group_by(TrialType) %>%
    summarise(
      mean = mean(get(var_name), na.rm = TRUE),
      se = sd(get(var_name), na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    ) %>%
    mutate(
      TrialType = factor(TrialType, levels = c("Game MP", "Game LP", "2-back", "LR")),
      lower = mean - se,
      upper = mean + se
    )
  
  y_max <- max(plot_data$upper, na.rm = TRUE) * 1.1
  y_breaks <- if(y_max <= 5) seq(0, 5, 1) else seq(0, ceiling(y_max), 2)
  
  p <- ggplot(plot_data, aes(x = TrialType, y = mean, group = 1)) +
    geom_line(linewidth = 1.2, color = "black") +
    geom_point(size = 5, color = "black") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, linewidth = 0.8) +
    labs(
      title = var_name,
      x = "Condition",
      y = paste(var_name, "Rating")
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 33, face = "bold"),
      axis.title = element_text(size = 28),
      axis.text = element_text(size = 24),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_y_continuous(limits = c(0, y_max), breaks = y_breaks)
  
  return(p)
}

for(var in geq_vars) {
  p <- create_geq_plot(var, data)
  filename <- paste0(tolower(var), "_plot.png")
  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  cat(paste(var, "plot saved as:", filename, "\n"))
}

# Save Results to File

sink("geq_complete_analysis_results.txt")
cat("GAME EXPERIENCE QUESTIONNAIRE COMPLETE ANALYSIS RESULTS\n")
cat("======================================================\n\n")

for(i in 1:length(geq_vars)) {
  var <- geq_vars[i]
  res <- results[[var]]
  
  cat(paste(i, ".", toupper(var), "EXPERIENCE\n"))
  cat(paste(rep("=", nchar(var) + 12), collapse = ""), "\n\n")
  
  cat("Sphericity Test:\n")
  cat("Mauchly's W =", round(res$sphericity$sphericity.tests[1, "Test statistic"], 3), 
      ", p =", round(res$mauchly_p, 3), "\n")
  if (res$mauchly_p < 0.05) {
    cat("Sphericity VIOLATED → Using GG correction (ε =", round(res$gg_eps, 3), ")\n\n")
  } else {
    cat("Sphericity NOT VIOLATED → No correction needed\n\n")
  }
  
  cat("ANOVA Results:\n")
  if (res$mauchly_p < 0.05) {
    cat("F(", round(res$df_num, 1), ",", round(res$df_den, 1), ") =", round(res$f_value, 3), 
        ", p =", format(res$p_value, scientific = TRUE), " [GG corrected] , ηp² =", round(res$eta_sq, 3), "\n\n")
  } else {
    cat("F(", res$df_num, ",", res$df_den, ") =", round(res$f_value, 3), 
        ", p =", format(res$p_value, scientific = TRUE), " [uncorrected] , ηp² =", round(res$eta_sq, 3), "\n\n")
  }
  
  cat("Post-hoc Tests (All Pairwise):\n")
  print(res$pairs[,c("contrast", "estimate", "se", "t.ratio", "p.value.adj")])
  
  cat("\nPlanned Contrasts (3 comparisons):\n")
  print(res$planned[,c("contrast", "estimate", "se", "t.ratio", "p.value.adj")])
  
  if(i < length(geq_vars)) cat("\n\n")
}

sink()

cat("\n=== Analysis Complete ===\n")
cat("Results saved to: geq_complete_analysis_results.txt\n")
cat("Individual plots saved as PNG files\n")

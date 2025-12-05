#!/usr/bin/env Rscript
# Repeated Measures ANOVA Analysis: Game Effects on Pain Intensity and Unpleasantness
# 
# This script performs repeated measures ANOVA analyses to examine the effects of 
# different trial types (game conditions vs. control conditions) on pain intensity 
# and unpleasantness ratings.
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

data <- read_excel("Game2.xlsx")
data <- as.data.frame(data)

data$PainIntensity100 <- data$PainIntensity200 - 100

data$TrialType <- factor(data$TrialType, 
                        levels = c("GP", "GNP", "2back", "LR"),
                        labels = c("Game MP", "Game LP", "2-back", "LR"))

# Data Preparation for RM-ANOVA
data_wide_pi <- data %>%
  group_by(SubjectID, TrialType) %>%
  summarise(PainIntensity100 = mean(PainIntensity100, na.rm = TRUE)) %>%
  pivot_wider(names_from = TrialType, values_from = PainIntensity100) %>%
  ungroup()

data_wide_up <- data %>%
  group_by(SubjectID, TrialType) %>%
  summarise(Unpleasantness = mean(Unpleasantness, na.rm = TRUE)) %>%
  pivot_wider(names_from = TrialType, values_from = Unpleasantness) %>%
  ungroup()

names(data_wide_pi) <- make.names(names(data_wide_pi), unique = TRUE)
names(data_wide_up) <- make.names(names(data_wide_up), unique = TRUE)

names(data_wide_pi) <- gsub("^X", "", names(data_wide_pi))
names(data_wide_up) <- gsub("^X", "", names(data_wide_up))

within_factors <- data.frame(
  condition = factor(1:4, labels = c("Game.MP", "Game.LP", "2.back", "LR"))
)

# Pain Intensity Analysis

cat("=== PAIN INTENSITY ANALYSIS ===\n\n")

model_pi <- lm(cbind(Game.MP, Game.LP, `2.back`, LR) ~ 1, data = data_wide_pi)
aov_pi <- Anova(model_pi, idata = within_factors, idesign = ~condition, type = "III")
sphericity_pi <- summary(aov_pi, multivariate = FALSE)
mauchly_p_pi <- sphericity_pi$sphericity.tests[1, "p-value"]
gg_eps_pi <- sphericity_pi$pval.adjustments[1, "GG eps"]

cat("Sphericity Test: Mauchly's W =", round(sphericity_pi$sphericity.tests[1, "Test statistic"], 3), 
    ", p =", round(mauchly_p_pi, 3), "\n")

if (mauchly_p_pi < 0.05) {
  cat("Sphericity VIOLATED → Using GG correction (ε =", round(gg_eps_pi, 3), ")\n")
  f_value_pi <- sphericity_pi$univariate.tests[2, "F value"]
  df_num_pi <- sphericity_pi$univariate.tests[2, "num Df"] * gg_eps_pi
  df_den_pi <- sphericity_pi$univariate.tests[2, "den Df"] * gg_eps_pi
  p_value_pi <- sphericity_pi$pval.adjustments[1, "Pr(>F[GG])"]
  cat("F(", round(df_num_pi, 1), ",", round(df_den_pi, 1), ") =", round(f_value_pi, 3), 
      ", p =", format(p_value_pi, scientific = TRUE), " [GG corrected]\n")
} else {
  cat("Sphericity NOT VIOLATED → No correction needed\n")
  f_value_pi <- sphericity_pi$univariate.tests[2, "F value"]
  df_num_pi <- sphericity_pi$univariate.tests[2, "num Df"]
  df_den_pi <- sphericity_pi$univariate.tests[2, "den Df"]
  p_value_pi <- sphericity_pi$univariate.tests[2, "Pr(>F)"]
  cat("F(", df_num_pi, ",", df_den_pi, ") =", round(f_value_pi, 3), 
      ", p =", format(p_value_pi, scientific = TRUE), " [uncorrected]\n")
}

ss_effect_pi <- sphericity_pi$univariate.tests[2, "Sum Sq"]
ss_error_pi <- sphericity_pi$univariate.tests[2, "Error SS"]
eta_sq_pi <- ss_effect_pi / (ss_effect_pi + ss_error_pi)
cat("ηp² =", round(eta_sq_pi, 3), "\n\n")

cat("Post-hoc Tests (All Pairwise):\n")
emm_pi <- emmeans(model_pi, ~ 1)
contrast_matrix_pi <- matrix(c(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1), nrow=4, byrow=TRUE)
rownames(contrast_matrix_pi) <- c("Game.MP", "Game.LP", "2.back", "LR")

means_pi <- colMeans(data_wide_pi[,2:5], na.rm=TRUE)
ms_error_pi <- sphericity_pi$univariate.tests[2, "Error SS"] / sphericity_pi$univariate.tests[2, "den Df"]
n_subjects <- nrow(data_wide_pi)

pairs_pi <- data.frame(
  contrast = c("Game MP - Game LP", "Game MP - 2-back", "Game MP - LR", 
               "Game LP - 2-back", "Game LP - LR", "2-back - LR"),
  estimate = c(means_pi[1] - means_pi[2], means_pi[1] - means_pi[3], means_pi[1] - means_pi[4],
               means_pi[2] - means_pi[3], means_pi[2] - means_pi[4], means_pi[3] - means_pi[4])
)
pairs_pi$se <- sqrt(2 * ms_error_pi / n_subjects)
pairs_pi$t.ratio <- pairs_pi$estimate / pairs_pi$se
pairs_pi$p.value <- 2 * pt(abs(pairs_pi$t.ratio), df = sphericity_pi$univariate.tests[2, "den Df"], lower.tail = FALSE)
pairs_pi$p.value.adj <- p.adjust(pairs_pi$p.value, method = "holm")

cat(" contrast           estimate   SE  df t.ratio p.value\n")
for(i in 1:nrow(pairs_pi)) {
  cat(sprintf("%-18s %8.2f %5.2f %2.0f %7.3f %7.3f\n", 
              pairs_pi$contrast[i], pairs_pi$estimate[i], pairs_pi$se[i], 
              sphericity_pi$univariate.tests[2, "den Df"], 
              pairs_pi$t.ratio[i], pairs_pi$p.value.adj[i]))
}
cat("P value adjustment: holm method for 6 tests\n")

cat("\nPlanned Contrasts (3 comparisons):\n")
planned_pi <- data.frame(
  contrast = c("TwoBack_vs_LR", "GameLP_vs_2back", "GameMP_vs_GameLP"),
  estimate = c(means_pi[3] - means_pi[4], means_pi[2] - means_pi[3], means_pi[1] - means_pi[2])
)
planned_pi$se <- sqrt(2 * ms_error_pi / n_subjects)
planned_pi$t.ratio <- planned_pi$estimate / planned_pi$se
planned_pi$p.value <- 2 * pt(abs(planned_pi$t.ratio), df = sphericity_pi$univariate.tests[2, "den Df"], lower.tail = FALSE)
planned_pi$p.value.adj <- p.adjust(planned_pi$p.value, method = "holm")

cat(" contrast         estimate   SE  df t.ratio p.value\n")
for(i in 1:nrow(planned_pi)) {
  cat(sprintf("%-16s %8.2f %5.2f %2.0f %7.3f %7.3f\n", 
              planned_pi$contrast[i], planned_pi$estimate[i], planned_pi$se[i], 
              sphericity_pi$univariate.tests[2, "den Df"], 
              planned_pi$t.ratio[i], planned_pi$p.value.adj[i]))
}
cat("P value adjustment: holm method for 3 tests\n")

# Pain Unpleasantness Analysis

cat("\n=== PAIN UNPLEASANTNESS ANALYSIS ===\n\n")

model_up <- lm(cbind(Game.MP, Game.LP, `2.back`, LR) ~ 1, data = data_wide_up)
aov_up <- Anova(model_up, idata = within_factors, idesign = ~condition, type = "III")
sphericity_up <- summary(aov_up, multivariate = FALSE)
mauchly_p_up <- sphericity_up$sphericity.tests[1, "p-value"]
gg_eps_up <- sphericity_up$pval.adjustments[1, "GG eps"]

cat("Sphericity Test: Mauchly's W =", round(sphericity_up$sphericity.tests[1, "Test statistic"], 3), 
    ", p =", round(mauchly_p_up, 3), "\n")

if (mauchly_p_up < 0.05) {
  cat("Sphericity VIOLATED → Using GG correction (ε =", round(gg_eps_up, 3), ")\n")
  f_value_up <- sphericity_up$univariate.tests[2, "F value"]
  df_num_up <- sphericity_up$univariate.tests[2, "num Df"] * gg_eps_up
  df_den_up <- sphericity_up$univariate.tests[2, "den Df"] * gg_eps_up
  p_value_up <- sphericity_up$pval.adjustments[1, "Pr(>F[GG])"]
  cat("F(", round(df_num_up, 1), ",", round(df_den_up, 1), ") =", round(f_value_up, 3), 
      ", p =", format(p_value_up, scientific = TRUE), " [GG corrected]\n")
} else {
  cat("Sphericity NOT VIOLATED → No correction needed\n")
  f_value_up <- sphericity_up$univariate.tests[2, "F value"]
  df_num_up <- sphericity_up$univariate.tests[2, "num Df"]
  df_den_up <- sphericity_up$univariate.tests[2, "den Df"]
  p_value_up <- sphericity_up$univariate.tests[2, "Pr(>F)"]
  cat("F(", df_num_up, ",", df_den_up, ") =", round(f_value_up, 3), 
      ", p =", format(p_value_up, scientific = TRUE), " [uncorrected]\n")
}

ss_effect_up <- sphericity_up$univariate.tests[2, "Sum Sq"]
ss_error_up <- sphericity_up$univariate.tests[2, "Error SS"]
eta_sq_up <- ss_effect_up / (ss_effect_up + ss_error_up)
cat("ηp² =", round(eta_sq_up, 3), "\n\n")

cat("Post-hoc Tests (All Pairwise):\n")
emm_up <- emmeans(model_up, ~ 1)

means_up <- colMeans(data_wide_up[,2:5], na.rm=TRUE)
ms_error_up <- sphericity_up$univariate.tests[2, "Error SS"] / sphericity_up$univariate.tests[2, "den Df"]
n_subjects <- nrow(data_wide_up)

pairs_up <- data.frame(
  contrast = c("Game MP - Game LP", "Game MP - 2-back", "Game MP - LR", 
               "Game LP - 2-back", "Game LP - LR", "2-back - LR"),
  estimate = c(means_up[1] - means_up[2], means_up[1] - means_up[3], means_up[1] - means_up[4],
               means_up[2] - means_up[3], means_up[2] - means_up[4], means_up[3] - means_up[4])
)
pairs_up$se <- sqrt(2 * ms_error_up / n_subjects)
pairs_up$t.ratio <- pairs_up$estimate / pairs_up$se
pairs_up$p.value <- 2 * pt(abs(pairs_up$t.ratio), df = sphericity_up$univariate.tests[2, "den Df"], lower.tail = FALSE)
pairs_up$p.value.adj <- p.adjust(pairs_up$p.value, method = "holm")

cat(" contrast           estimate   SE  df t.ratio p.value\n")
for(i in 1:nrow(pairs_up)) {
  cat(sprintf("%-18s %8.2f %5.2f %2.0f %7.3f %7.3f\n", 
              pairs_up$contrast[i], pairs_up$estimate[i], pairs_up$se[i], 
              sphericity_up$univariate.tests[2, "den Df"], 
              pairs_up$t.ratio[i], pairs_up$p.value.adj[i]))
}
cat("P value adjustment: holm method for 6 tests\n")

cat("\nPlanned Contrasts (3 comparisons):\n")
planned_up <- data.frame(
  contrast = c("TwoBack_vs_LR", "GameLP_vs_2back", "GameMP_vs_GameLP"),
  estimate = c(means_up[3] - means_up[4], means_up[2] - means_up[3], means_up[1] - means_up[2])
)
planned_up$se <- sqrt(2 * ms_error_up / n_subjects)
planned_up$t.ratio <- planned_up$estimate / planned_up$se
planned_up$p.value <- 2 * pt(abs(planned_up$t.ratio), df = sphericity_up$univariate.tests[2, "den Df"], lower.tail = FALSE)
planned_up$p.value.adj <- p.adjust(planned_up$p.value, method = "holm")

cat(" contrast         estimate   SE  df t.ratio p.value\n")
for(i in 1:nrow(planned_up)) {
  cat(sprintf("%-16s %8.2f %5.2f %2.0f %7.3f %7.3f\n", 
              planned_up$contrast[i], planned_up$estimate[i], planned_up$se[i], 
              sphericity_up$univariate.tests[2, "den Df"], 
              planned_up$t.ratio[i], planned_up$p.value.adj[i]))
}
cat("P value adjustment: holm method for 3 tests\n")

# Percentage Reductions

cat("\n=== PERCENTAGE REDUCTIONS ===\n\n")

pi_means <- data %>% group_by(TrialType) %>% summarise(mean = mean(PainIntensity100, na.rm = TRUE))
up_means <- data %>% group_by(TrialType) %>% summarise(mean = mean(Unpleasantness, na.rm = TRUE))

game_mp_pi <- pi_means$mean[pi_means$TrialType == "Game MP"]
twoback_pi <- pi_means$mean[pi_means$TrialType == "2-back"]
lr_pi <- pi_means$mean[pi_means$TrialType == "LR"]

game_mp_up <- up_means$mean[up_means$TrialType == "Game MP"]
twoback_up <- up_means$mean[up_means$TrialType == "2-back"]
lr_up <- up_means$mean[up_means$TrialType == "LR"]
pi_reduction_2back <- (twoback_pi - game_mp_pi) / twoback_pi * 100
pi_reduction_lr <- (lr_pi - game_mp_pi) / lr_pi * 100
up_reduction_2back <- (twoback_up - game_mp_up) / twoback_up * 100
up_reduction_lr <- (lr_up - game_mp_up) / lr_up * 100

cat("Pain Intensity:\n")
cat("Game MP vs 2-back:", round(pi_reduction_2back, 1), "% reduction (from", round(twoback_pi, 2), "to", round(game_mp_pi, 2), ")\n")
cat("Game MP vs LR:", round(pi_reduction_lr, 1), "% reduction (from", round(lr_pi, 2), "to", round(game_mp_pi, 2), ")\n\n")

cat("Pain Unpleasantness:\n")
cat("Game MP vs 2-back:", round(up_reduction_2back, 1), "% reduction (from", round(twoback_up, 2), "to", round(game_mp_up, 2), ")\n")
cat("Game MP vs LR:", round(up_reduction_lr, 1), "% reduction (from", round(lr_up, 2), "to", round(game_mp_up, 2), ")\n")

sink("rm_anova_results.txt")
cat("REPEATED MEASURES ANOVA RESULTS\n")
cat("===============================\n\n")

cat("1. PAIN INTENSITY ANALYSIS\n")
cat("==========================\n\n")

cat("Sphericity Test:\n")
cat("Mauchly's W =", round(sphericity_pi$sphericity.tests[1, "Test statistic"], 3), 
    ", p =", round(mauchly_p_pi, 3), "\n")
if (mauchly_p_pi < 0.05) {
  cat("Sphericity VIOLATED → Using Greenhouse-Geisser correction (ε =", round(gg_eps_pi, 3), ")\n")
} else {
  cat("Sphericity NOT VIOLATED → No correction needed\n")
}

cat("\nANOVA Results:\n")
cat("F(", ifelse(mauchly_p_pi < 0.05, paste(round(df_num_pi, 1), ",", round(df_den_pi, 1)), paste(df_num_pi, ",", df_den_pi)), 
    ") =", round(f_value_pi, 3), ", p =", format(p_value_pi, scientific = TRUE), 
    ifelse(mauchly_p_pi < 0.05, " [GG corrected]", " [uncorrected]"), 
    ", ηp² =", round(eta_sq_pi, 3), "\n\n")

cat("Post-hoc Tests (All Pairwise):\n")
print(pairs_pi[,c("contrast", "estimate", "se", "t.ratio", "p.value.adj")])

cat("\nPlanned Contrasts (3 comparisons):\n")
print(planned_pi[,c("contrast", "estimate", "se", "t.ratio", "p.value.adj")])

cat("\n\n2. PAIN UNPLEASANTNESS ANALYSIS\n")
cat("===============================\n\n")

cat("Sphericity Test:\n")
cat("Mauchly's W =", round(sphericity_up$sphericity.tests[1, "Test statistic"], 3), 
    ", p =", round(mauchly_p_up, 3), "\n")
if (mauchly_p_up < 0.05) {
  cat("Sphericity VIOLATED → Using Greenhouse-Geisser correction (ε =", round(gg_eps_up, 3), ")\n")
} else {
  cat("Sphericity NOT VIOLATED → No correction needed\n")
}

cat("\nANOVA Results:\n")
cat("F(", ifelse(mauchly_p_up < 0.05, paste(round(df_num_up, 1), ",", round(df_den_up, 1)), paste(df_num_up, ",", df_den_up)), 
    ") =", round(f_value_up, 3), ", p =", format(p_value_up, scientific = TRUE), 
    ifelse(mauchly_p_up < 0.05, " [GG corrected]", " [uncorrected]"), 
    ", ηp² =", round(eta_sq_up, 3), "\n\n")

cat("Post-hoc Tests (All Pairwise):\n")
print(pairs_up[,c("contrast", "estimate", "se", "t.ratio", "p.value.adj")])

cat("\nPlanned Contrasts (3 comparisons):\n")
print(planned_up[,c("contrast", "estimate", "se", "t.ratio", "p.value.adj")])

cat("\n\n3. PERCENTAGE REDUCTIONS\n")
cat("========================\n\n")
cat("Pain Intensity - Game MP vs 2-back:", round(pi_reduction_2back, 1), "% reduction (from", round(twoback_pi, 2), "to", round(game_mp_pi, 2), ")\n")
cat("Pain Intensity - Game MP vs LR:", round(pi_reduction_lr, 1), "% reduction (from", round(lr_pi, 2), "to", round(game_mp_pi, 2), ")\n")
cat("Pain Unpleasantness - Game MP vs 2-back:", round(up_reduction_2back, 1), "% reduction (from", round(twoback_up, 2), "to", round(game_mp_up, 2), ")\n")
cat("Pain Unpleasantness - Game MP vs LR:", round(up_reduction_lr, 1), "% reduction (from", round(lr_up, 2), "to", round(game_mp_up, 2), ")\n")

sink()

# Create Line Plots

cat("\n=== CREATING LINE PLOTS ===\n")

plot_data_pi <- data %>%
  group_by(TrialType) %>%
  summarise(
    mean = mean(PainIntensity100, na.rm = TRUE),
    se = sd(PainIntensity100, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  mutate(
    TrialType = factor(TrialType, levels = c("Game MP", "Game LP", "2-back", "LR")),
    lower = mean - se,
    upper = mean + se
  )

plot_data_up <- data %>%
  group_by(TrialType) %>%
  summarise(
    mean = mean(Unpleasantness, na.rm = TRUE),
    se = sd(Unpleasantness, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  mutate(
    TrialType = factor(TrialType, levels = c("Game MP", "Game LP", "2-back", "LR")),
    lower = mean - se,
    upper = mean + se
  )

# Pain Intensity Plot
p1 <- ggplot(plot_data_pi, aes(x = TrialType, y = mean, group = 1)) +
  geom_line(linewidth = 1.2, color = "black") +
  geom_point(size = 5, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, linewidth = 0.8) +
  labs(
    title = "Pain Intensity",
    x = "Condition",
    y = "Pain Intensity (0-100)"
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
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10))

p2 <- ggplot(plot_data_up, aes(x = TrialType, y = mean, group = 1)) +
  geom_line(linewidth = 1.2, color = "black") +
  geom_point(size = 5, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, linewidth = 0.8) +
  labs(
    title = "Pain Unpleasantness",
    x = "Condition",
    y = "Pain Unpleasantness (0-100)"
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
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10))

ggsave("pain_intensity_plot.png", p1, width = 8, height = 6, dpi = 300)
ggsave("pain_unpleasantness_plot.png", p2, width = 8, height = 6, dpi = 300)

cat("Line plots saved as:\n")
cat("- pain_intensity_plot.png\n")
cat("- pain_unpleasantness_plot.png\n")

cat("\n=== Analysis Complete ===\n")
cat("Results saved to: rm_anova_results.txt\n")
cat("Plots saved as PNG files\n")

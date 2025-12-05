#!/usr/bin/env Rscript
# Game Identity Analysis: Testing Game Identity vs. Identification Effect
# 
# This script tests whether the preference effect is due to the specific 
# identity of the games selected (i.e., certain games are inherently more effective) or 
# whether it reflects a general identification effect (i.e., identifying with any game 
# confers benefits regardless of which specific game it is).
#
# We compare pain intensity and unpleasantness ratings across different game identities 
# when they are most identified, using Kruskal-Wallis tests and post-hoc Dunn's tests.
# If game identity matters, we would expect significant differences between games. If only 
# the identification effect matters, we would expect no differences between games.
#
# Dependencies: readxl, tidyverse, rstatix, ggpubr, effectsize

library(readxl)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(effectsize)

data <- read_excel("Game2ident.xlsx")

participant_summary <- data %>%
  group_by(SubjectID) %>%
  summarise(
    n_trials = n(),
    n_mp = sum(Game == 1),
    n_lp = sum(Game == 0),
    identified_game = first(PrefferedGameName),
    least_identified_game = first(LeastPrefferedGameName)
  )

game_identity_data <- data %>%
  filter(Game %in% c(0, 1)) %>%  # Only analyze Game MP (1) and Game LP (0)
  mutate(
    GameName = case_when(
      Game == 1 ~ PrefferedGameName,
      Game == 0 ~ LeastPrefferedGameName
    ),
    IdentityStatus = case_when(
      Game == 1 ~ "Most Identified",
      Game == 0 ~ "Least Identified"
    ),
    PainIntensity100 = PainIntensity200 - 100
  )

participant_averages <- game_identity_data %>%
  group_by(SubjectID, IdentityStatus, GameName) %>%
  summarise(
    mean_pain = mean(PainIntensity100, na.rm = TRUE),
    mean_unp = mean(Unpleasantness, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

identified_games <- participant_averages %>%
  filter(IdentityStatus == "Most Identified")

sample_sizes <- identified_games %>%
  group_by(GameName) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )

kw_pain <- kruskal.test(mean_pain ~ GameName, data = identified_games)
kw_unp <- kruskal.test(mean_unp ~ GameName, data = identified_games)

k_pain <- length(unique(identified_games$GameName))
n_pain <- nrow(identified_games)
pain_es <- (kw_pain$statistic - k_pain + 1) / (n_pain - k_pain)

k_unp <- length(unique(identified_games$GameName))
n_unp <- nrow(identified_games)
unp_es <- (kw_unp$statistic - k_unp + 1) / (n_unp - k_unp)

pain_dunn <- dunn_test(mean_pain ~ GameName, data = identified_games, p.adjust.method = "holm")
unp_dunn <- dunn_test(mean_unp ~ GameName, data = identified_games, p.adjust.method = "holm")

cat("\n=== KRUSKAL-WALLIS TESTS ===\n\n")
cat("Pain Intensity:\n")
cat("H =", round(kw_pain$statistic, 3), ", df =", kw_pain$parameter, ", p =", format(kw_pain$p.value, scientific = TRUE), "\n")
cat("Epsilon-squared (effect size) =", round(pain_es, 3), "\n\n")

cat("Pain Unpleasantness:\n")
cat("H =", round(kw_unp$statistic, 3), ", df =", kw_unp$parameter, ", p =", format(kw_unp$p.value, scientific = TRUE), "\n")
cat("Epsilon-squared (effect size) =", round(unp_es, 3), "\n\n")

cat("=== POST-HOC TESTS (Dunn's test with Holm adjustment) ===\n\n")
cat("Pain Intensity:\n")
print(pain_dunn)
cat("\nPain Unpleasantness:\n")
print(unp_dunn)
cat("\n")

game_detailed_stats <- identified_games %>%
  group_by(GameName) %>%
  summarise(
    n_participants = n(),
    mean_pain = mean(mean_pain, na.rm = TRUE),
    sd_pain = sd(mean_pain, na.rm = TRUE),
    se_pain = if(n() > 1) sd_pain / sqrt(n()) else NA,
    median_pain = median(mean_pain, na.rm = TRUE),
    mean_unp = mean(mean_unp, na.rm = TRUE),
    sd_unp = sd(mean_unp, na.rm = TRUE),
    se_unp = if(n() > 1) sd_unp / sqrt(n()) else NA,
    median_unp = median(mean_unp, na.rm = TRUE)
  ) %>%
  arrange(mean_pain)

custom_theme <- theme_classic() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 20)),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20, margin = margin(b = 20)),
    plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(b = 20)),
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    axis.line = element_line(linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

pain_comparison_plot <- ggplot(participant_averages, 
       aes(x = GameName, y = mean_pain, fill = IdentityStatus)) +
  geom_boxplot(position = position_dodge(width = 0.85), alpha = 0.8, width = 0.7) +
  geom_point(aes(color = IdentityStatus),
             position = position_jitterdodge(dodge.width = 0.85, jitter.width = 0.2),
             alpha = 0.7, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7, linewidth = 1) +
  scale_fill_manual(values = c("Most Identified" = "#1b9e77", "Least Identified" = "#d95f02")) +
  scale_color_manual(values = c("Most Identified" = "#1b9e77", "Least Identified" = "#d95f02")) +
  custom_theme +
  labs(
    title = "Pain Intensity Comparison",
    subtitle = "Negative values indicate pain reduction relative to baseline",
    x = "Game",
    y = "Pain Intensity Change (0-100)",
    fill = "Identity Status",
    color = "Identity Status"
  )

unpleasantness_comparison_plot <- ggplot(participant_averages, 
       aes(x = GameName, y = mean_unp, fill = IdentityStatus)) +
  geom_boxplot(position = position_dodge(width = 0.85), alpha = 0.8, width = 0.7) +
  geom_point(aes(color = IdentityStatus),
             position = position_jitterdodge(dodge.width = 0.85, jitter.width = 0.2),
             alpha = 0.7, size = 3) +
  scale_fill_manual(values = c("Most Identified" = "#7570b3", "Least Identified" = "#e7298a")) +
  scale_color_manual(values = c("Most Identified" = "#7570b3", "Least Identified" = "#e7298a")) +
  custom_theme +
  labs(
    title = "Pain Unpleasantness Comparison",
    subtitle = "Higher values indicate greater unpleasantness",
    x = "Game",
    y = "Pain Unpleasantness Rating",
    fill = "Identity Status",
    color = "Identity Status"
  )

game_summary <- participant_averages %>%
  group_by(GameName, IdentityStatus) %>%
  summarise(
    n = n(),
    mean_pain = mean(mean_pain, na.rm = TRUE),
    sd_pain = sd(mean_pain, na.rm = TRUE),
    mean_unp = mean(mean_unp, na.rm = TRUE),
    sd_unp = sd(mean_unp, na.rm = TRUE),
    .groups = "drop"
  )

sink("identity_comparison_analysis.txt")
cat("Game Identity Comparison Analysis\n")
cat("=================================\n\n")

cat("KRUSKAL-WALLIS TESTS\n")
cat("====================\n\n")

cat("Pain Intensity:\n")
cat("H =", round(kw_pain$statistic, 3), ", df =", kw_pain$parameter, ", p =", format(kw_pain$p.value, scientific = TRUE), "\n")
cat("Epsilon-squared (effect size) =", round(pain_es, 3), "\n\n")

cat("Pain Unpleasantness:\n")
cat("H =", round(kw_unp$statistic, 3), ", df =", kw_unp$parameter, ", p =", format(kw_unp$p.value, scientific = TRUE), "\n")
cat("Epsilon-squared (effect size) =", round(unp_es, 3), "\n\n")

cat("POST-HOC TESTS (Dunn's test with Holm adjustment)\n")
cat("==================================================\n\n")

cat("Pain Intensity:\n")
print(pain_dunn)
cat("\n")

cat("Pain Unpleasantness:\n")
print(unp_dunn)
cat("\n")

cat("SUMMARY STATISTICS\n")
cat("==================\n\n")
print(game_summary)
sink()

pdf("identity_comparison_plots.pdf", width = 14, height = 10)
print(pain_comparison_plot)
print(unpleasantness_comparison_plot)
dev.off()

ggsave("pain_intensity_comparison.png", pain_comparison_plot, width = 14, height = 10, dpi = 300)
ggsave("unpleasantness_comparison.png", unpleasantness_comparison_plot, width = 14, height = 10, dpi = 300)

cat("\nAnalysis complete. Results have been saved to:\n")
cat("1. identity_comparison_analysis.txt (summary statistics)\n")
cat("2. identity_comparison_plots.pdf (combined plots)\n")
cat("3. pain_intensity_comparison.png (separate pain intensity plot)\n")
cat("4. unpleasantness_comparison.png (separate pain unpleasantness plot)\n")

popular_games_plot <- participant_averages %>%
  filter(GameName %in% c("subway surfer", "flow free", "2048")) %>%
  pivot_longer(
    cols = c(mean_pain, mean_unp),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = case_when(
      measure == "mean_pain" ~ "Pain Intensity",
      measure == "mean_unp" ~ "Pain Unpleasantness"
    )
  ) %>%
  ggplot(aes(x = GameName, y = value, fill = IdentityStatus)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2),
             alpha = 0.4, size = 2) +
  facet_wrap(~measure, scales = "free_y", ncol = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Detailed Analysis of Most Popular Games",
    subtitle = "Games with largest sample sizes",
    x = "Game",
    y = "Rating",
    fill = "Identity Status"
  )

pdf("popular_games_analysis.pdf", width = 10, height = 12)
print(popular_games_plot)
dev.off() 

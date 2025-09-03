# Repeated Measures ANOVA Analysis: Game Effects on Pain Intensity and Unpleasantness
# 
# This script performs repeated measures ANOVA analyses to examine the effects of 
# different trial types (game conditions vs. control conditions) on pain intensity 
# and unpleasantness ratings.
#
# Dependencies: readxl, afex, emmeans, ggplot2

# Load required packages
library(readxl)
library(afex)
library(emmeans)
library(ggplot2)

# Set options
options(scipen = 999)

# Read and prepare data
data <- read_excel("Game2.xlsx")
data <- as.data.frame(data)

# Display original pain intensity means (100-200 scale)
cat("\n=== Original Pain Intensity (100-200) Means by Condition ===\n")
orig_means <- aggregate(PainIntensity200 ~ TrialType, data = data, FUN = mean)
print(orig_means)

# Rescale pain intensity from 100-200 to 0-100 scale
data$PainIntensity100 <- data$PainIntensity200 - 100

# Display rescaled means
cat("\n=== Rescaled Pain Intensity (0-100) Means by Condition ===\n")
rescaled_means <- aggregate(PainIntensity100 ~ TrialType, data = data, FUN = mean)
print(rescaled_means)

# Recode trial type labels
data$TrialType <- factor(data$TrialType, 
                        levels = c("GP", "GNP", "2back", "LR"),
                        labels = c("Game MP", "Game LP", "2-back", "LR"))

# ============================================================================
# Pain Intensity Analysis
# ============================================================================

# Repeated measures ANOVA for pain intensity
model_PI <- afex::aov_ez(
  id = "SubjectID",
  dv = "PainIntensity100",
  within = "TrialType",
  fun_aggregate = mean,
  data = data
)

# Display ANOVA results
cat("\n=== Pain Intensity (0-100): Repeated Measures ANOVA ===\n")
print(anova(model_PI, es = "pes"))

# Calculate estimated marginal means
emm_PI <- emmeans(model_PI, "TrialType")

# Define planned contrasts
contrasts <- list(
  GameMP_vs_GameLP = c(1, -1, 0, 0),
  GameMP_vs_2back = c(1, 0, -1, 0),
  GameMP_vs_LR = c(1, 0, 0, -1),
  GameLP_vs_2back = c(0, 1, -1, 0),
  GameLP_vs_LR = c(0, 1, 0, -1),
  TwoBack_vs_LR = c(0, 0, 1, -1),
  Games_vs_Controls = c(0.5, 0.5, -0.5, -0.5),
  GameMP_vs_Others = c(3, -1, -1, -1)
)

# Run planned contrasts with Holm correction
cat("\n=== Pain Intensity (0-100): Planned Contrasts ===\n")
print(contrast(emm_PI, method = contrasts, adjust = "holm"))

# ============================================================================
# Unpleasantness Analysis
# ============================================================================

# Repeated measures ANOVA for unpleasantness
model_UP <- afex::aov_ez(
  id = "SubjectID",
  dv = "Unpleasantness",
  within = "TrialType",
  fun_aggregate = mean,
  data = data
)

# Display ANOVA results
cat("\n=== Unpleasantness: Repeated Measures ANOVA ===\n")
print(anova(model_UP, es = "pes"))

# Calculate estimated marginal means
emm_UP <- emmeans(model_UP, "TrialType")

# Run planned contrasts with Holm correction
cat("\n=== Unpleasantness: Planned Contrasts ===\n")
print(contrast(emm_UP, method = contrasts, adjust = "holm"))

# ============================================================================
# Descriptive Statistics
# ============================================================================

# Function to extract summary statistics
get_summary_stats <- function(model) {
  summary_data <- summary(model)
  data.frame(
    TrialType = summary_data$TrialType,
    Mean = summary_data$emmean,
    SE = summary_data$SE,
    Lower.CI = summary_data$lower.CL,
    Upper.CI = summary_data$upper.CL
  )
}

# Display means and standard errors
cat("\n=== Pain Intensity (0-100): Means and Standard Errors ===\n")
print(get_summary_stats(emm_PI))
cat("\n=== Unpleasantness: Means and Standard Errors ===\n")
print(get_summary_stats(emm_UP))

# ============================================================================
# Visualization
# ============================================================================

# Function to create line plots with error bars
create_plot <- function(data, dv, title, ylab) {
  means <- aggregate(data[[dv]] ~ TrialType, data, mean)
  names(means) <- c("TrialType", "mean")
  
  se <- aggregate(data[[dv]] ~ TrialType, data, 
                 function(x) sd(x)/sqrt(length(x)))
  names(se) <- c("TrialType", "se")
  
  plot_data <- merge(means, se)
  
  ggplot(plot_data, aes(x = TrialType, y = mean, group = 1)) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(size = 3, color = "steelblue") +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                 width = 0.2, color = "black") +
    geom_text(aes(label = sprintf("%.1f", mean)), 
              vjust = -1.5, size = 4) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray80")
    ) +
    labs(title = title, x = "Trial Type", y = ylab) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
}

# Create and save plots
pdf("rm_anova_line_plots.pdf", width = 12, height = 6)
p1 <- create_plot(data, "PainIntensity100", 
                  "Pain Intensity by Trial Type",
                  "Pain Intensity Rating (0-100)")
print(p1)

p2 <- create_plot(data, "Unpleasantness", 
                  "Unpleasantness by Trial Type",
                  "Unpleasantness Rating")
print(p2)
dev.off()

# ============================================================================
# Effect Size Calculations
# ============================================================================

# Function to calculate reduction metrics
calculate_reductions <- function(game_mean, control_mean) {
  abs_reduction <- control_mean - game_mean
  scale_pct_reduction <- (abs_reduction / 100) * 100
  relative_pct_reduction <- (abs_reduction / control_mean) * 100
  return(list(
    absolute = abs_reduction,
    scale_percentage = scale_pct_reduction,
    relative_percentage = relative_pct_reduction
  ))
}

# Calculate pain intensity reductions
cat("\n=== Pain Intensity Reductions (0-100 scale) ===\n")
cat("\nPain Intensity:\n")
cat("--------------------------------\n")

game_mp_mean <- get_summary_stats(emm_PI)$Mean[get_summary_stats(emm_PI)$TrialType == "Game.MP"]
twoback_mean <- get_summary_stats(emm_PI)$Mean[get_summary_stats(emm_PI)$TrialType == "X2.back"]
lr_mean <- get_summary_stats(emm_PI)$Mean[get_summary_stats(emm_PI)$TrialType == "LR"]

# Game MP vs 2-back
reductions_2back <- calculate_reductions(game_mp_mean, twoback_mean)
cat(sprintf("Game MP vs 2-back:\n"))
cat(sprintf("- Absolute reduction: %.1f points (from %.1f to %.1f)\n", 
            reductions_2back$absolute, twoback_mean, game_mp_mean))
cat(sprintf("- Percentage of scale (0-100): %.1f%%\n", 
            reductions_2back$scale_percentage))
cat(sprintf("- Relative percentage reduction: %.1f%% of 2-back value\n", 
            reductions_2back$relative_percentage))

# Game MP vs LR
reductions_lr <- calculate_reductions(game_mp_mean, lr_mean)
cat(sprintf("\nGame MP vs LR:\n"))
cat(sprintf("- Absolute reduction: %.1f points (from %.1f to %.1f)\n", 
            reductions_lr$absolute, lr_mean, game_mp_mean))
cat(sprintf("- Percentage of scale (0-100): %.1f%%\n", 
            reductions_lr$scale_percentage))
cat(sprintf("- Relative percentage reduction: %.1f%% of LR value\n", 
            reductions_lr$relative_percentage))

# Calculate unpleasantness reductions
cat("\nUnpleasantness:\n")
cat("--------------------------------\n")

game_mp_mean <- get_summary_stats(emm_UP)$Mean[get_summary_stats(emm_UP)$TrialType == "Game.MP"]
twoback_mean <- get_summary_stats(emm_UP)$Mean[get_summary_stats(emm_UP)$TrialType == "X2.back"]
lr_mean <- get_summary_stats(emm_UP)$Mean[get_summary_stats(emm_UP)$TrialType == "LR"]

# Game MP vs 2-back
reductions_2back <- calculate_reductions(game_mp_mean, twoback_mean)
cat(sprintf("Game MP vs 2-back:\n"))
cat(sprintf("- Absolute reduction: %.1f points (from %.1f to %.1f)\n", 
            reductions_2back$absolute, twoback_mean, game_mp_mean))
cat(sprintf("- Relative percentage reduction: %.1f%% of 2-back value\n", 
            reductions_2back$relative_percentage))

# Game MP vs LR
reductions_lr <- calculate_reductions(game_mp_mean, lr_mean)
cat(sprintf("\nGame MP vs LR:\n"))
cat(sprintf("- Absolute reduction: %.1f points (from %.1f to %.1f)\n", 
            reductions_lr$absolute, lr_mean, game_mp_mean))
cat(sprintf("- Relative percentage reduction: %.1f%% of LR value\n", 
            reductions_lr$relative_percentage))

# ============================================================================
# Save Results
# ============================================================================

# Save comprehensive results to file
sink("detailed_results.txt")

cat("\nCOMPREHENSIVE ANALYSIS RESULTS\n")
cat("============================\n\n")

cat("1. PAIN INTENSITY (0-100 scale)\n")
cat("-----------------\n")
cat("\na) Descriptive Statistics:\n")
print(get_summary_stats(emm_PI))
cat("\nb) Repeated Measures ANOVA:\n")
print(anova(model_PI, es = "pes"))
cat("\nc) Planned Contrasts:\n")
print(contrast(emm_PI, method = contrasts, adjust = "holm"))
cat("\nd) Pain Reductions:\n")
cat("\nGame MP vs 2-back:\n")
cat(sprintf("- Absolute reduction: %.1f points (from %.1f to %.1f)\n", 
            reductions_2back$absolute, twoback_mean, game_mp_mean))
cat(sprintf("- Percentage of scale (0-100): %.1f%%\n", 
            reductions_2back$scale_percentage))
cat(sprintf("- Relative percentage reduction: %.1f%% of 2-back value\n", 
            reductions_2back$relative_percentage))
cat("\nGame MP vs LR:\n")
cat(sprintf("- Absolute reduction: %.1f points (from %.1f to %.1f)\n", 
            reductions_lr$absolute, lr_mean, game_mp_mean))
cat(sprintf("- Percentage of scale (0-100): %.1f%%\n", 
            reductions_lr$scale_percentage))
cat(sprintf("- Relative percentage reduction: %.1f%% of LR value\n", 
            reductions_lr$relative_percentage))

cat("\n\n2. UNPLEASANTNESS\n")
cat("-----------------\n")
cat("\na) Descriptive Statistics:\n")
print(get_summary_stats(emm_UP))
cat("\nb) Repeated Measures ANOVA:\n")
print(anova(model_UP, es = "pes"))
cat("\nc) Planned Contrasts:\n")
print(contrast(emm_UP, method = contrasts, adjust = "holm"))
cat("\nd) Reductions:\n")
cat("\nGame MP vs 2-back:\n")
cat(sprintf("- Absolute reduction: %.1f points (from %.1f to %.1f)\n", 
            reductions_2back$absolute, twoback_mean, game_mp_mean))
cat(sprintf("- Relative percentage reduction: %.1f%% of 2-back value\n", 
            reductions_2back$relative_percentage))
cat("\nGame MP vs LR:\n")
cat(sprintf("- Absolute reduction: %.1f points (from %.1f to %.1f)\n", 
            reductions_lr$absolute, lr_mean, game_mp_mean))
cat(sprintf("- Relative percentage reduction: %.1f%% of LR value\n", 
            reductions_lr$relative_percentage))

sink()
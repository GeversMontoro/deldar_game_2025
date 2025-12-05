# Game Effects on Pain: Statistical Analysis Repository

This repository contains the complete statistical analysis pipeline for examining the effects of game conditions on pain intensity and unpleasantness ratings.

## Overview

This project investigates how different game conditions (Game MP, Game LP, 2-back, and LR control) affect pain perception, with analyses examining direct effects, mediation pathways, game experience factors, and causal inference.

## Repository Structure

### Main Analysis Scripts

#### 3.1 - Repeated Measures ANOVA
- **File**: `3.1_deldar_game2025_rm_anova.R`
- **Purpose**: Performs repeated measures ANOVA to examine the effects of different trial types (game conditions vs. control conditions) on pain intensity and unpleasantness ratings.
- **Outputs**: 
  - `rm_anova_results.txt` - Complete ANOVA results
  - `pain_intensity_plot.png` - Pain intensity line plot
  - `pain_unpleasantness_plot.png` - Pain unpleasantness line plot

#### 3.2 - Game Experience Questionnaire (GEQ) Analysis
- **File**: `3.2_deldar_game2025_geq_anova.R`
- **Purpose**: Performs repeated measures ANOVA analyses for all GEQ components (Flow, Positive Affect, Negative Affect, Immersion, Competence, Tension, Challenge).
- **Outputs**:
  - `geq_complete_analysis_results.txt` - Complete GEQ analysis results
  - Individual plot files for each GEQ component (e.g., `flow_plot.png`, `competence_plot.png`)

#### 3.3 - Game Identity Analysis
- **File**: `3.3_deldar_game_identity_analysis.R`
- **Purpose**: Tests whether the preference effect is due to the specific identity of the games selected (i.e., certain games are inherently more effective) or whether it reflects a general identification effect (i.e., identifying with any game confers benefits regardless of which specific game it is).
- **Method**: Kruskal-Wallis tests and post-hoc Dunn's tests comparing different game identities when they are most identified.
- **Outputs**:
  - `identity_comparison_analysis.txt` - Summary statistics
  - `identity_comparison_plots.pdf` - Combined plots
  - `pain_intensity_comparison.png` - Separate pain intensity plot
  - `unpleasantness_comparison.png` - Separate pain unpleasantness plot

#### 3.4 - Mediation Analysis
- **Files**: 
  - `3.4.1_deldar_game_mediation_analysis_intensity.R` - Pain intensity outcome
  - `3.4.2_deldar_game_mediation_analysis_unpleasantness.R` - Pain unpleasantness outcome
- **Purpose**: Performs multilevel mediation analysis to test whether Flow mediates the effects of game conditions on pain outcomes.
- **Method**: Multilevel modeling approach with bootstrapping to estimate indirect effects and their confidence intervals.
- **Outputs**:
  - `mediation_results_intensity.csv` - Mediation results for pain intensity
  - `mediation_results_unpleasantness.csv` - Mediation results for pain unpleasantness

#### 3.5 - Machine Learning Feature Importance
- **File**: `3.5_deldar_game_ml_feature_importance.ipynb`
- **Purpose**: Performs machine learning analyses to identify the most important features (game experience variables, personality traits, mindfulness) that predict pain intensity and unpleasantness ratings.
- **Method**: Uses SHAP (SHapley Additive exPlanations) values to understand feature importance and contributions to model predictions.
- **Dependencies**: pandas, numpy, scikit-learn, xgboost, shap, pingouin, matplotlib, seaborn, tensorflow

#### 3.5 - Conditional Independence Analysis
- **File**: `3.5_deldar_game_conditional_independence.py`
- **Purpose**: Performs causal structure discovery and conditional independence testing using DAGMA and causal inference methods.
- **Method**: Uses DAGMA (Directed Acyclic Graph via M-matrices) for structure discovery and DoWhy GCM for causal modeling.

## Data Files

- `Game.xlsx` - Main dataset for GEQ analysis
- `Game2.xlsx` - Main dataset for pain intensity/unpleasantness analyses
- `Game2ident.xlsx` - Dataset for identity/preference analysis
- `processed_game_data.csv` - Processed data for conditional independence analysis

## Dependencies

### R Packages
- `readxl` - Reading Excel files
- `dplyr`, `tidyr` - Data manipulation
- `car`, `afex` - ANOVA analyses
- `emmeans` - Post-hoc tests
- `ggplot2` - Plotting
- `nlme` - Multilevel modeling
- `boot` - Bootstrapping
- `rstatix` - Statistical tests
- `ggpubr`, `effectsize` - Additional statistical tools

### Python Packages
- `pandas`, `numpy` - Data manipulation
- `scikit-learn`, `xgboost` - Machine learning
- `shap` - Feature importance
- `pingouin` - Statistical analysis
- `matplotlib`, `seaborn` - Plotting
- `tensorflow` - Deep learning
- `causallearn`, `dowhy`, `dagma` - Causal inference
- `scmtools`, `cdp` - Causal modeling tools
- `torch` - Deep learning framework

## Usage

### Running R Scripts

```bash
Rscript 3.1_deldar_game2025_rm_anova.R
Rscript 3.2_deldar_game2025_geq_anova.R
Rscript 3.3_deldar_game_identity_analysis.R
Rscript 3.4.1_deldar_game_mediation_analysis_intensity.R
Rscript 3.4.2_deldar_game_mediation_analysis_unpleasantness.R
```

### Running Python Scripts

```bash
python 3.5_deldar_game_conditional_independence.py
```

### Running Jupyter Notebooks

```bash
jupyter notebook 3.5_deldar_game_ml_feature_importance.ipynb
```

## Output Files

All analysis results are saved in the `results_plots/` directory, including:
- Statistical test results (`.txt` files)
- Mediation analysis results (`.csv` files)
- Visualization plots (`.png`, `.pdf` files)

## Notes

- All scripts assume data files are in the same directory as the scripts
- Some scripts require specific data file names (see individual script headers)
- Results files are overwritten on each run
- The analysis follows a sequential structure (3.1 → 3.2 → 3.3 → 3.4 → 3.5)

## License

[Add your license information here]

## Contact

[Add contact information here]


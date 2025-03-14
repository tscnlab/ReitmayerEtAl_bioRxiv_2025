<img src="https://avatars.githubusercontent.com/u/78920096?s=200&v=4" width="100"/>

# ReitmayerEtAl_bioRxiv_2025

# Non-linear relationship between evening light exposure and cognitive performance

This README file provides an overview of the dataset and R code used in our paper, "Non-linear Relationship Between Evening Light Exposure and Cognitive Performance". The data analysis was performed using R, encompassing data cleaning, statistical analysis, and plotting.

All materials are made publicly available under an open-source (GPL) or open-access license (CC-BY).

For any questions, comments, or feedback, please contact Manuel Spitschan at manuel.spitschan@tum.de.


## Raw data

Folder: `01_rawdata`

This folder contains the raw data for climate measurements and cognitive performance tests conducted during the experiment. Subfolders include `Climate` (containing environmental data files), Datatable for R (containing the comprehensive datasets `data.csv` and `data_thermal.csv` used for all statistical analyses), NASA-TLX (workload assessments), and PVT (psychomotor vigilance test results). All data is organised by participant, time point and session for subsequent analysis.

## Questionnaire

Folder: `02_questionnaire`

This folder contains the raw data of the questionnaire responses for each participant and session. The data for each questionnaire type (e.g., KSS, MAS, NASA-TLX) is stored in a CSV file.

## Demographics

Folder: `03_demographics`

This folder includes the demographic data for each participant and session, provided in a CSV file.

## Analysis

Folder: `04_analysis`

This folder contains R scripts for statistical analysis and data visualisation.
Each assessment parameter has its own R file for processing, statistical calculations, and graph generation:

- `PVT.R`: Psychomotor Vigilance Task analysis and plots
- `NBACK.R`: N-back task analysis and plots
- `NASA-Mentaldemand.R`: NASA-TLX mental demand subscale analysis and plots
- `NASA-Temporaldemand.R`: NASA-TLX temporal demand subscale analysis and plots
- `NASA-Performance.R`: NASA-TLX performance subscale analysis and plots
- `MAS-Energeticarousal.R`: Momentary Affect Scale energetic arousal subscale analysis and plots
- `MAS-Tensearousal.R`: Momentary Affect Scale tense arousal subscale analysis and plots
- `KSS.R`: Karolinska Sleepiness Scale analysis and plots
- `ClimateDataperSession.py`: Visualisation of operative temperature and ambient lux levels per participant/session

### Data integration and preparation

The R scripts referenced above use a comprehensive dataset stored in `data.csv`. This file combines data from the following sources, previously described in these sections:

1. Raw data (Folder: `01_rawdata`): Climate measurements and cognitive performance test results (N-back and PVT)
2. Questionnaire responses (Folder: `02_questionnaire`): KSS, MAS, and NASA-TLX
3. Demographic information (Folder: `03_demographics`)

We manually aggregated these data using Microsoft Excel to create a single, unified dataset. The resulting `data.csv` file contains detailed records for each participant (ID: 101-116) across all time points (Timepoint: 1-10) and experimental sessions (Scenario: 1, 10, 70, 595) including responses of NASA-TLX, MAS and KSS questionnaires as well as results of the performance tasks.

### Data processing

Participant ID 103 was excluded from all analyses. Outliers were removed using the is_outlier function with a coefficient of 1.5.

Required R packages: <br>
- data.table <br>
- ggplot2 <br>
- patchwork <br>
- plotly <br>
- dplyr <br>
- ggpubr <br>
- Hmisc <br>
- lme4 <br>
- lmerTest <br>
- rstatix <br>

### Creating figures 3-10

Each R file follows the same structure to generate the figures presented in the paper. 
To create Figures 3-10, run the code section labeled "##### Code for Figures" in the respective R files.

The analysis process for each parameter (task results PVT and N-back/questionnaire responses NASA-TLX, MAS and KSS) includes:

1. Data summarisation:
   * Calculate mean, standard deviation, and median of the parameters
   * Group data by Timepoint and Scenario

2. Figure generation:
   Each figure consists of two main components:

   a) Time progression analysis (Graph 1):
      * Displays mean values with standard deviation
      * Shows statistical differences across time points for each scenario
      * Uses line plots with ribbon for standard deviation

   b) Scenario comparison (Graph 2):
      * Uses median values
      * Illustrates statistical differences between scenarios
      * Employs box plots with jittered data points

3. Statistical analysis:
   * Conducts pairwise comparisons between scenarios
   * Uses Tukey's HSD test with Bonferroni correction

4. Visualisation details:
   * Consistent colour scheme across scenarios: "#9F7E2A" for very dim,"#BF9832" for dim,"#DFB13B" for moderately bright and "#FFCA43" for very bright lighting

5. Output:
   * Combines both graphs into a single figure
   * Saves the final figure as a JPG file

<p align="center">
  <img src="https://github.com/tscnlab/ReitmayerEtAl_ProcRSocB_2025/blob/main/05_graphs/Figure3-PVT.jpg" width="480">
</p>
<p align="center">
  PVT by scenario (A) and time (B) as an example of graphical output after running the code (Figure 3).
</p>

While the specific parameters and variable names differ for each parameter, the overall structure and analysis approach remain consistent across all R files.

### Linear mixed model analysis

Each R file includes a section for linear mixed model (LMM) analysis, labeled "##### Linear Mixed Model Analysis". 

This analysis follows these steps:

1. Data preparation:
   * Convert relevant variables to appropriate data types (factor, numeric)
   * Apply log10 transformation to the Scenario variable
   * Convert Timepoint to numeric

2. Model formulation:
   * Dependent variable: Task results/questionnaire responses
   * Fixed effects:
     - Scenario (linear and quadratic terms)
     - Timepoint
     - Interaction between Timepoint and Scenario
   * Random effect: Participant ID

3. Model fitting:
   * Use lmer function from lme4 package to fit the LMM
   * Formula: "Measure ~ Scenario + I(Scenario^2) + Timepoint + Timepoint:Scenario + (1|ID)"

4. Statistical inference:
   * Extract p-values for fixed effects
   * Apply Bonferroni correction to adjust for multiple comparisons

### Creating supplementary figures 

The following files generate supplementary figures:

- `PVT_thermal.R`
- `NBACK_thermal.R`
- `KSS_thermal.R`
- `ClimateDataperSession.py`

The R scripts utilise `data_thermal.csv`, which includes environmental data, particularly operative temperature. This additional data allows for the exploration of potential relationships between thermal conditions and the parameters PVT, N-back, KSS.

Each R file follows a consistent structure to create two types of figures for the supplement:

1. A combined graph showing the relationship between the parameters (PVT, N-back, KSS) and operative temperature across all scenarios.
2. Separate graphs for each lighting scenario, illustrating the parameter's relationship with operative temperature.

<p align="center">
  <img src="https://github.com/tscnlab/ReitmayerEtAl_ProcRSocB_2025/blob/main/05_graphs/Supplementary_PVT_thermal.jpg" width="480">
</p>
<p align="center">
  Relationship between PVT and operative temperature as an example of graphical output after running the code.
</p>

The process for each file involves:

- Data Preparation: Loading data, removing outliers
- Summarisation: Calculating statistics by environmental conditions and scenario
- Statistical Analysis: Normality tests, ANOVA
- Figure Generation: Each file produces two graphs:
  a) Scenario-specific Analysis: Faceted plots showing regressions for each lighting scenario
  b) Combined Analysis: Single plot with all scenarios, using colour-coding to distinguish between them. Includes an overall regression line and scenario-specific mean values
- Output: Both graphs combined into a single figure, saved as JPG

The R file `Correlation.R` generates additional supplementary figures using `data.csv`. This script introduces new libraries, including corrplot, hms, boot, gridExtra, grid, png and PerformanceAnalytics.

As with previous analyses, participant ID 103 and outliers are excluded. The script performs three types of correlation analyses:

1. Overall correlation matrix:
   * Calculates mean values for all parameters by participant
   * Creates a correlation matrix using the chart.Correlation function
   * Outputs a comprehensive correlation plot as `Supplementary_Corr-matrix.jpg`

<p align="center">
  <img src="https://github.com/tscnlab/ReitmayerEtAl_ProcRSocB_2025/blob/main/05_graphs/Supplementary_Corr-matrix.jpg" width="480">
</p>
<p align="center">
  Correlation matrix as graphical output after running the code.
</p>

2. Pairwise correlations:
   * Computes mean values for parameters by participant and scenario
   * Generates scatter plots for all combinations of the values N-back Accuracy, PVT, KSS, and NASA-TLX subscales mental and temporal demand
   * Saves each plot as a jpg file (`Supplementary_Corr-Acc.KSS.jpg`,`Supplementary_Corr-Acc.MD.jpg`,`Supplementary_Corr-Acc.RT.jpg`,`Supplementary_Corr-Acc.TD.jpg`, `Supplementary_Corr-RT.KSS.jpg`,`Supplementary_Corr-RT.MD.jpg`,`Supplementary_Corr-RT.TD.jpg`)

<p align="center">
  <img src="https://github.com/tscnlab/ReitmayerEtAl_ProcRSocB_2025/blob/main/05_graphs/Supplementary_Corr-Acc.KSS.jpg" width="480">
</p>
<p align="center">
 Correlation between N-back Accuracy and KSS responses as an example of graphical output after running the code.
</p>

3. Individual participant analysis:
   * For each participant, creates box plots showing the relationship between each parameter (KSS, MAS, NASA-TLX, PVT, N-back) and the different lighting scenarios
   * Saved as `Supplementary_matrixP1.jpg`-`Supplementary_matrixP6.jpg`

<p align="center">
  <img src="https://github.com/tscnlab/ReitmayerEtAl_ProcRSocB_2025/blob/main/05_graphs/Supplementary_matrixP2.jpg" width="480">
</p>
<p align="center">
  Participant ID's 104-106 as an example of graphical output after running the code.
</p>

The Python file `ClimateDataperSession.py` generates the supplementary figure `Supplementary_OpTemp-Light.png` using environmental data from `TimeClimaAct.csv`. The code calculates operative temperature, computes mean and standard deviation of temperature, humidity, and light parameters by session ID, and exports the processed statistics to '99_ClimaStatsperSession_luxtop.csv'.
The visualisation presents a two-panel figure with operative temperature (top panel) and ambient light levels (bottom panel, log scale). Data points represent means across participant sessions with error bars showing standard deviation, organised by scenario and participant ID.

<p align="center">
  <img src="https://github.com/tscnlab/ReitmayerEtAl_ProcRSocB_2025/blob/main/05_graphs/Supplementary_OpTemp-Light.png" width="480">
</p>
<p align="center">
  Operative temperature and ambient lux levels per participant and session.
</p>

## Graphs

Folder: `05_graphs`

This folder contains the generated figures from all R scripts described above, saved as JPG files. These figures include visualisations of the main analyses, supplementary analyses, and correlation analyses.

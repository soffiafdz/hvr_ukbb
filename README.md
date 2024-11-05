# HVR and Head-Size Correction Analysis in Healthy Aging

This repository contains scripts and tools for investigating sex differences in hippocampal volume and the effects of different head-size correction methods, with a specific focus on hippocampal volume ratio (HVR). The project leverages both cross-sectional and longitudinal data from the UK Biobank to study healthy aging, and includes analysis on a subcohort of head-size matched males and females.

## Key Objectives

- **Head-Size Correction Methods**: Explore and compare different methods for adjusting hippocampal volumes by head size, such as intracranial volume (ICC), and how these methods affect the analysis of sex differences in hippocampal volumes.
- **Hippocampal Volume Ratio (HVR)**: Assess HVR as a potential alternative to traditional head-size correction methods and its relevance in analyzing sex differences in brain structure during healthy aging.
- **Cross-Sectional and Longitudinal Analysis**: Apply these correction methods to both cross-sectional and longitudinal samples to observe differences in hippocampal volume changes over time.
- **Head-Size Matched Subcohort**: Perform analyses on a subsample of males and females matched on head size to investigate whether matching improves the accuracy of sex difference comparisons in hippocampal volumes.
- **Cognitive Analysis**: Examine the relationship between hippocampal volume (and its correction methods) and cognitive performance. A latent variable for cognition will be created using confirmatory factor analysis (CFA).

## Data

This project uses data from the UK Biobank, including:
- **MRI-Derived Hippocampal Volumes**: Structural brain imaging data to measure hippocampal volumes and related regions.
- **Cognitive Test Scores**: A variety of cognitive performance measures to explore the relationship between brain structure and cognitive aging.
- **Demographic and Clinical Data**: Covariates such as age, sex, education, and head size (ICC), which are essential for adjusting models and controlling confounding variables.

## License

This project is licensed under the **GNU GPLv3 License**. See the `LICENSE` file for more details.

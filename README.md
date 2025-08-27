# k-complexity: Kolmogorov-based complexity in spoken English varieties


#### Replication data and scripts for: Assessing the role of socio-demographic triggers on Kolmogorov-based complexity in spoken English varieties

DOI: tba

### Description

This repository comprises the original data, scripts, graphs and additional statistics as described in the related publication

* Ehret, Katharina. "Assessing the role of socio-demographic triggers on Kolmogorov-based complexity in spoken English varieties". Submitted to *Entropy*.


### Overview of materials


#### data

* k-complexity_data.csv: The final formatted dataset for analysis containing the complexity ratios obtained with the  [*compression technique*](https://github.com/katehret/measuring-language-complexity) and the socio-demographic data from [*A socio-demographic Dataset fOr Varieties of English*](LINK) (DOVE, https://doi.org/10.5281/zenodo.16794586). ❗Note that the demographic data is subject to copyright as specified in the original sources. For details consult the respective files in DOVE.❗

* FRED_complexityRatios.csv: The output of the compression technique for FRED.

* ICE_complexityRatios.zip: A zip archive with the output of the compression technique for ICE. Extract before usage.

* SBCSAE_complexityRatios.csv: The output of the compression technique for the SBCSAE.



#### scripts

* format.data.r: Script to generate the final formatted dataset

* explore.ggplots.r: R script to visually explore the data and create some nice plots.

* regression.r: R script with detailed steps to replicate the regression analyses, all statistics, and plots described in the related publication.

* transform.variables.r: R script to apply transformations necessary for modelling. Automatically called and referenced in the script regression.r
  
#### stats

Statistics and data generated in the analysis.

* syn_outlier.csv: Outliers for syntactic complexity

* morph_outliers.csv: Outliers for morphological complexity

* corr.csv: The correlation coefficients of socio-demographic triggers

* pvalues_corr.csv: The corresponding p-values of the correlation coefficients in corr.csv

* m1_anova_REML.txt: Results of the anova tests for the random effects in the morphological complexity regression (with REML=T)

* m1_coefs_confint.csv: Profile confidence intervals for the fixed effects in the morphological complexity model

* m1_LRT.txt: Likelihood ratio tests for the fixed effects in the morphological complexity model

* m1_summary.txt: Regression summary of the morphological complexity model as discussed in the paper

* m1_VIFs: Variance inflation factors for the morphological complexity model

* s1_VIFs: Variance inflation factors for the syntactic complexity model (s1)

* s2_VIFs: Variance inflation factors for the syntactic complexity model (s2)

* s2_ranefEstimates.csv: Random effects variance in the syntactic complexity model (s2)


#### pics

Contains the images used in the related publication.

 

### How to cite

Versions released on zenodo can be cited as tba

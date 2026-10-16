REPAIR IN CHILD LANGUAGE ACQUISITION: SHARED FORMATS AND PATTERNS OF VARIATION

Public Repository: https://github.com/lingresearchers/Repairs
License: Creative Commons Attribution 4.0 International (CC BY 4.0)

This repository provides the complete replication package, statistical models, posterior draws, analysis code, figures, and data for:

Feurstein, L., Arisci, N., Furrer, R., & Stoll, S. (in press / under review). Repair in Child Language Acquisition: Shared Formats and Patterns of Variation. Cognitive Science.

================================================================================
AUTHORS & AFFILIATIONS
================================================================================

- Liudmyla Feurstein (Corresponding Author)
  Institute for the Interdisciplinary Study of Language Evolution (ISLE), University of Zurich, Switzerland
  Address: Institut für Interdisziplinäre Sprachevolutionswissenschaft (ISLE-Institut), Universität Zürich, Affolternstrasse 56, CH-8050 Zürich, Switzerland
  Email: liudmyla.feurstein@uzh.ch
  ORCID: https://orcid.org/0000-0003-1448-3501

- Nina Arisci
  Institute for the Interdisciplinary Study of Language Evolution (ISLE), University of Zurich, Switzerland
  Email: nina.arisci@uzh.ch
  ORCID: https://orcid.org/0000-0002-1190-1897

- Reinhard Furrer
  Department of Mathematical Modeling and Machine Learning, University of Zurich, Switzerland
  Email: reinhard.furrer@math.uzh.ch
  ORCID: https://orcid.org/0000-0002-6319-2332

- Sabine Stoll
  Institute for the Interdisciplinary Study of Language Evolution (ISLE), University of Zurich, Switzerland
  Email: sabine.stoll@uzh.ch
  ORCID: https://orcid.org/0000-0001-6328-5654

================================================================================
OVERVIEW
================================================================================

This statistical package accompanies our study of other-initiated conversational repair (OIR) in early child language acquisition. It contains aggregate session counts, saved Bayesian fits, posterior draws, analysis code, statistical tables, and high-resolution figures for three typologically and culturally diverse language corpora:
- Chintang (Sino-Tibetan, Nepal)
- Jakarta Indonesian (Austronesian)
- Russian (Indo-European)

The sample comprises 36 standardized recording sessions from six target children followed longitudinally between 25 and 40 months, with 72 paired child-speech (CS) and child-directed speech (CDS) observations, 49,168 utterances, and 1,452 other-initiated repairs.

================================================================================
PACKAGE CONTENTS
================================================================================

data/original/
  Two supplied aggregate count datasets: prototypes_new.csv and repairs_primary_cs_cds_long.csv. Preserved unchanged.

data/analysis/
  Reconciled session/source counts, child-model predictors, scaling constants, age reconciliation, and reported pooled detection contingency table.

scripts/
  Complete R pipeline for data preparation, model fitting, posterior summaries, predictive checks, figures, validation, and checksum verification. Main entry point: run_all.R.

stan/
  Stan model code: beta-binomial rate models and Dirichlet-multinomial format likelihoods.

models/primary/
  Three self-contained RDS fits: child repair rate, joint child/CDS repair rate, and repair formats.

models/sensitivity/
  Twelve saved sensitivity fits covering age coding, linear age trends, varying-effect structures, input decomposition, and format priors.

models/draws/
  Retained-draw CSV chains (gzip-compressed) for all 15 fits, plus chain provenance records.

tables/
  Observed counts and rates, primary posterior contrasts, expected-rate curves, all sensitivity contrasts, and quadratic age coefficients.

diagnostics/
  Sampler diagnostics, Monte Carlo standard errors, and posterior/prior predictive summaries and draws.

figures/
  Figures 1 to 5 and Figures S1 to S4 in both publication-ready vector PDF and high-res PNG formats.

documentation/
  In-depth dictionaries: DATA_DICTIONARY.md, MODEL_DICTIONARY.md, ESTIMAND_DICTIONARY.md, and AGE_MODEL_CHECKS.md.

multiplicity/
  Directional-error analysis script and results for the full family of 75 primary zero-referenced estimands (5% false-sign threshold).

MANIFEST.sha256
  SHA-256 checksums for all distributed files.

The saved fits contain 96,000 retained posterior draws in total: 8,000 per primary fit and 6,000 per sensitivity fit. The RDS objects are self-contained and do not depend on external fitted-model files.

================================================================================
FIGURES
================================================================================

All figures are provided in the figures/ folder in both .pdf and .png formats:

- Figure 1 (Fig1_Longitudinal_OIR_Rates):
  Observed session rates and joint-model expected-rate curves.

- Figure 2 (Fig2_H1_H2_Posterior_Contrasts):
  Input associations and pairwise differences in age-related change.

- Figure 3 (Fig3_Repair_Types_Combined):
  Observed repair-format composition, with counts and percentages.

- Figure 4 (Fig4_H3_CrossLinguistic_Differentiation):
  Posterior child-minus-CDS dispersion across the three fitted corpora.

- Figure 5 (Fig5_SpeechSource_Posterior_Contrasts):
  Mean-age expected rates and child-minus-CDS contrasts.

- Figure S1 (FigS1_Format_Expected_Rates):
  Expected incidence of the three repair formats over age.

- Figure S2 (FigS2_Posterior_Predictive_Checks):
  Selected posterior predictive discrepancies.

- Figure S3 (FigS3_Prior_Predictive_Checks):
  Prior predictive expected-rate distributions.

- Figure S4 (FigS4_Sensitivity_Contrasts):
  Selected contrasts across primary and sensitivity fits.

================================================================================
REPRODUCTION INSTRUCTIONS
================================================================================

1. CACHED REPRODUCTION (Recommended - No Compiler or CmdStan Required)

Cached reproduction runs entirely in R using the precomputed posterior draws and saved fits. It requires R (version 4.3.3 or higher) and the packages posterior, dplyr, tidyr, ggplot2, gridExtra, jsonlite, and digest. Python 3 is only needed for checksum verification.

From the repository root, run:

  Rscript scripts/install_dependencies.R
  python3 scripts/verify_manifest.py --root .
  Rscript scripts/run_all.R --mode cached
  Rscript multiplicity/check_multiple_comparisons.R --package . --out multiplicity/results

The cached pipeline loads all 15 fits, verifies configurations against sources, regenerates all tables, diagnostics, and figures, and creates a verification/ folder containing numerical checks and a verification report.

2. FULL REFITTING (Requires CmdStan & C++ Toolchain)

Fresh MCMC sampling requires cmdstanr (tested with CmdStan 2.40.0) and a C++ compiler:

  Rscript scripts/install_dependencies.R --refit
  Rscript scripts/run_all.R --mode refit
  Rscript multiplicity/check_multiple_comparisons.R --package . --out multiplicity/results

Note: Refitting replaces saved fits, chain exports, and derived tables; MCMC sampling is stochastic and will not produce identical hashes.

================================================================================
DATA SCOPE AND LIMITATIONS
================================================================================

1. Age reconciliation:
   The primary analysis uses structured age (40 months) for one Chintang session where the raw numeric age was 39. Separate sensitivity fits evaluate this coding (see data/analysis/age_reconciliation.csv).

2. Aggregated counts:
   The distributed files contain session-level and turn-level aggregate counts. Primary audio/video recordings and full CHAT/ACQDIV transcripts are governed by the access conditions of the respective ACQDIV corpora.

3. Reliability counts:
   reliability_detection_contingency_reported.csv provides the reported pooled detection counts for calculating Cohen's kappa.

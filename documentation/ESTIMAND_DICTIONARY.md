# Estimand dictionary

All intervals are equal-tailed 95% posterior credible intervals, with posterior medians as point summaries. Directional probabilities are proportions of retained draws above/below zero, not frequentist p-values. An empirical proportion of zero or one is not mathematical certainty. Monte Carlo precision is supplied in `diagnostics/contrast_monte_carlo_precision.csv`.

## Rate-model quantities

Primary rate contrasts use fixed-effect predictions with child deviations set to zero. They are not predictions marginalized over the distribution of new children, and they are not averages over the fitted children's outcomes. The inverse-logit is applied within each posterior draw before computing rate differences or cross-corpus SDs.

| Output | Definition |
|---|---|
| `H1_input_endpoints.csv` | Empirical CDS P16 and P84 within each language (R quantile type 7), plus their language-standardized values |
| `H1_input_associations.csv` | Child expected-rate difference at the high versus low observed CDS percentile, at physical mean age 32.5 months; units OIRs/1,000 utterances |
| `equal_language_average` | Arithmetic mean of the three within-language H1 contrasts, within each draw; equal corpus weights |
| `child_age_changes.csv` | Expected child-rate change from 27.3038476 to 37.6961524 months, with standardized CDS rate zero |
| `H2_pairwise_changes.csv` | Differences between language-specific age changes or language-specific percentile input associations; not differences between logit slopes |
| `joint_baseline_rates.csv` | Joint-model expected rates at mean age, with child deviations zero |
| `speech_source_contrasts.csv` | Child minus CDS expected rates at mean age within each language |
| `H3_cross_corpus_dispersion.csv` | Within each draw, sample SD across the three fixed-effect corpus rates for child and CDS, then child SD minus CDS SD, at early/mean/late ages |
| `H3_within_draw_changes.csv` | Late-minus-early changes in child SD, CDS SD and their difference, computed within draws |
| `joint_expected_rate_curves.csv` | Joint fixed-effect expected-rate summaries on the 25--40-month plotting grid |

H3 describes these three fitted corpora. It is not a fitted population variance across languages. Subtracting stage-specific posterior medians is not the same as summarizing within-draw changes.

The within-/between-child sensitivity evaluates the **within-child** rate association at between-child standardized input zero using the primary percentile span. Its target therefore differs from the primary total association; that span need not be observed within every individual child. Between-child coefficients are retained in the model exports and parameter summaries.

## Format quantities

Primary format summaries are sample-conditional: predict each of the two fitted children within a language, then take an equal-child arithmetic mean of probabilities within each posterior draw. Do not substitute the inverse-logit/softmax of the mean child effect. They are a different estimand from the rate model's zero-deviation predictions.

| Quantity | Definition |
|---|---|
| Incidence | `1,000 * p(format)` per utterance, averaged equally over the two fitted children |
| Composition among OIRs | `100 * p(format) / sum(p(T1),p(T2),p(T3))`, calculated within each child's draw then averaged equally over children |
| Native baseline | Fixed log relative odds of the format versus non-repair at mean age, with child effect zero |
| Native linear age coefficient | Local log-relative-odds slope versus non-repair at mean age, per age SD; quadratic terms change that slope elsewhere |
| Format age-rate change | Late-minus-early incidence change, using the same physical ages as the rate contrasts |
| Child minus CDS T3 | Source difference in restricted-offer incidence, or separately in conditional composition, at mean age |
| Russian minus Chintang child T3 | Corpus difference in child restricted-offer incidence, or separately in conditional composition |

The empirical bars in Figure 3 use pooled observed repair counts, not equal-child model predictions. Their denominator is the number of observed OIRs. They cannot be interpreted as OIRs per utterance.

Primary age curves include quadratic terms. Early-to-late endpoint contrasts summarize only those two ages, not all variation between them. The quadratic coefficients are reported separately in `age_curvature_coefficients.csv`; linear-only alternatives assess sensitivity to this post-audit change.

## Predictive and sensitivity outputs

Posterior predictive replications condition on the observed design, utterance denominators and fitted child effects. Expected-rate credible bands differ from prediction intervals for future session counts.

The marginal Pearson discrepancy sums standardized squared count residuals using the beta-binomial/Dirichlet-multinomial marginal variance. DM categories are dependent; its sum is a diagnostic statistic, not a chi-square reference test. Other checks compare zeros, pooled counts, residual age and squared-age association, adjacent-session residual association within child/source groups, and paired child/CDS residual association in the joint model.

The diagnostic `Pr_replicated_gt_observed` is calculated from draw-paired discrepancies; ties are reported separately for discrete checks. Values near the extremes motivate inspection but do not certify or disprove the whole model. Prior checks draw fixed effects, child effects and precision from the full primary priors, then simulate counts for the observed design.

`sensitivity_all_contrasts.csv` distinguishes incidence units (per 1,000 utterances), composition units (percentage points), cross-corpus SD units (per 1,000 utterances) and native age log ratios in their corresponding tables. Figure S4 shows three selected contrasts; the complete results are in the CSV, not only the plotted subset.

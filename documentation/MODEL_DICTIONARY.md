# Model dictionary

The primary and sensitivity specifications are executable in `scripts/config.R`, `scripts/prepare_data.R` and the three Stan files. These choices constitute a post-audit reanalysis; they are not represented as preregistered models.

## Rates

For observation i with utterance count N and repair count Y:

`Y ~ beta_binomial(N, mu * phi, (1 - mu) * phi)`

`logit(mu) = X beta + Z u_child`

Each rate model has one positive precision parameter phi, shared across its observations. Its variance is `N * mu * (1-mu) * (N+phi)/(1+phi)`. Larger phi implies less extra-binomial dispersion. Conditional independence of session observations is a model assumption after accounting for fixed and child effects; posterior predictive residual checks are supplied.

The child-only primary model uses 36 rows and ten fixed coefficients: three language baselines, three language-specific linear age coefficients, three language-specific observed CDS-rate coefficients, and one shared quadratic age coefficient. Each of the six children has an intercept, linear age deviation and input deviation. Three varying-effect SDs are shared across languages.

The joint primary rate model uses 72 rows and 14 fixed coefficients: six language/source baselines, six language/source linear age coefficients, and two source-specific quadratic age coefficients shared across languages. Each child has a child-speech intercept, CDS intercept, child-speech linear age deviation and CDS linear age deviation. Four varying-effect SDs are shared across languages. Child and CDS rows from the same recording share child identity; additional same-session residual covariance is not explicitly fitted.

Primary fixed baseline coefficients have `Normal(-3.5, 1.25)` priors; linear/quadratic age and CDS coefficients have `Normal(0, 0.6)` priors. Varying-effect SDs have half-`Normal(0, 0.5)` priors. Standardized latent child effects have independent standard normal priors. Precision has `Gamma(shape=2, rate=0.05)`. Primary child effects are uncorrelated. Correlated rate sensitivities use a Cholesky correlation factor with `LKJ(2)` and the same SD priors.

The within-/between-child sensitivity adds three language-specific between-child coefficients and replaces the three total CDS associations with within-child coefficients. Its varying input deviations apply to within-child input. Between-child information consists of only two child means per language; the decomposition does not establish a causal input effect.

## Repair formats

The four categories are non-repair, open request (T1), restricted request (T2), and restricted offer (T3). Each of the 72 session/source observations contributes one four-count vector:

`y[1:4] ~ dirichlet_multinomial(phi * p[1:4])`, with `sum(y) = N`.

`p = softmax(0, eta_T1, eta_T2, eta_T3)`

`eta_k = baseline[language, source, k] + age_slope[language, source, k] * age_z + quadratic[source] * age_z^2 + u[child, source, k]`

There are 18 fixed baseline coefficients, 18 fixed linear age coefficients, two source-specific quadratic age coefficients shared across languages and formats, six source/type child-effect SDs, and six source/type deviations for each child. The six SDs are pooled across languages. Primary deviations are independent; they are not language-specific covariance estimates from two children. A single phi is shared across observations. This likelihood models the four mutually exclusive outcomes jointly and allows extra-multinomial dispersion.

Baseline log relative odds against non-repair have `Normal(-4.5, 1.5)` priors. Linear/quadratic age log-relative-odds coefficients have `Normal(0, 0.6)` priors. Varying-effect SDs have half-`Normal(0, 0.5)` priors. Standardized deviations are standard normal. Precision is `Gamma(shape=2, rate=0.05)`.

Narrower format priors use baseline SD 1, age SD 0.4, and varying SD scale 0.35. Wider format priors use baseline SD 2, age SD 1, and varying SD scale 1. The shared-child sensitivity uses a single intercept deviation per child across both sources and all three formats. All other choices are retained unless explicitly changed in `config.R`.

Native linear age coefficients compare each repair category with non-repair locally at mean age. Away from mean age, the log-relative-odds slope also includes `2 * quadratic[source] * age_z`. They do not by themselves determine the direction of change in marginal category incidence. Reported endpoint changes are calculated from the entire softmax at the specified physical ages; they cannot exclude within-interval curvature.

## Why quadratic terms are in the primary models

Posterior predictive squared-age residual checks identified curvature missed by linear age trends. A shared quadratic coefficient in the child model and source-specific shared coefficients in the joint/format models addressed that selected feature while limiting the number of additional parameters in this six-child sample. This was a data-informed revision during the audit, not an independent confirmatory prediction. The full linear-only alternatives are preserved as three sensitivity refits. `tables/age_curvature_coefficients.csv` reports the actual coefficients; all predictive results are supplied, including those for the linear alternatives. No formal model-ranking or universal developmental curve is claimed.

## Scaling and age sensitivities

All primary models use the same ages (mean 32.5 months; SD 5.1961524227 across the 36 sessions). Observed CDS rates are standardized across the 12 unique sessions within each language, with sample SDs. They are not standardized across duplicated source rows.

The age-39 sensitivities change one Chintang session from 40 to 39 and recompute the overall age center and SD. Comparisons to the primary fit nevertheless use the same physical early, mean and late ages and the same empirical CDS percentile endpoints. This distinguishes an age-data change from an accidental scaling change.

## Portable RDS structure

Each RDS is an `oir_model` plain list with:

| Field | Meaning |
|---|---|
| `name`, `config` | Fit identity and all model/prior choices |
| `sampling`, `seed`, `runtime` | Chain settings, random seed, R/Stan/interface versions |
| `draws` | `posterior::draws_array`: retained iterations x 4 chains x variables |
| `sampler` | All retained sampler diagnostics with chain and iteration IDs |
| `stan_data` | Exact design matrices, response counts, denominators and prior data used in Stan |
| `analysis_data` | Exact analysis rows, factors and predictors |
| `children`, `child_language` | Index-to-child and index-to-language mappings |
| `age_center`, `age_scale` | Fit-specific age transformation |
| `stan_file`, `stan_code` | Relative source filename and full source text |
| `original_data_sha256` | Hashes of the two original supplied data files |
| rate-only design fields | R design formula, coefficient names and varying-effect column names |
| format-only `condition_levels` | Mapping of six language/source baseline cells |

No object depends on an external CSV path or a compiled executable. Retained posterior draws and sampler diagnostics are also provided as gzip CSV files. The twelve age-39 chain exports were reconstructed from their saved RDS objects; original CmdStan comments and timing footers are unavailable. Their source and export hashes are recorded in `models/draws/recovered_age39_chain_provenance.json`. The reporting loader adds a temporary in-memory matrix cache; this is not needed in the saved RDS.

## Software references

The likelihood definitions were checked against the official [Stan bounded discrete distribution reference](https://mc-stan.org/docs/functions-reference/bounded_discrete_distributions.html) and [multivariate discrete distribution reference](https://mc-stan.org/docs/functions-reference/multivariate_discrete_distributions.html). Sampler checks follow the official [Stan diagnostics guidance](https://mc-stan.org/learn-stan/diagnostics-warnings.html). Fitting versions are recorded in the runtime field of each saved fit. Cached reproduction writes its current R session to `verification/sessionInfo.txt`. The fitting runtime was CmdStan 2.40.0 through cmdstanr 0.8.0.

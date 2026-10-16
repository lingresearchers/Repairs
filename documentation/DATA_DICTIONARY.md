# Data dictionary

`data/original/prototypes_new.csv` contains one row per recording session (36 rows). `data/original/repairs_primary_cs_cds_long.csv` contains one child row and one CDS row per session (72 rows). Both are preserved byte for byte; the analysis never writes over them.

## Original long-format fields

| Field | Meaning |
|---|---|
| `language` | chintang, indonesian or russian |
| `target_child` | Supplied child identifier; unique within language |
| `target_age` | Supplied structured age: years;months, e.g. 3;04.00 |
| `age_months` | Numeric age in the supplied long file; matches structured target_age |
| `session_id` | Supplied session identifier; retained as an opaque key |
| `n_participants` | Supplied participant-count field; not a model predictor |
| `planned_duration_min` | Supplied recording-duration field; not the count-model denominator or an offset |
| `speech_source` | child or CDS (child-directed speech) |
| `utterance_n` | Total utterance denominator for this session/source |
| `oir_n` | Utterances classified as other-initiated repair |
| `oir_T1_n` | Open requests |
| `oir_T2_n` | Restricted requests |
| `oir_T3_n` | Restricted offers/candidate understandings |
| `non_oir_utterance_n` | Non-repair utterances |

The three type counts sum to `oir_n`, and `oir_n + non_oir_utterance_n = utterance_n`. Counts are finite, nonnegative integers. No individual utterance sequence or repair transcript is supplied in these aggregate files.

## Original session-summary fields

`language`, `target_child`, `target_age` and `age_months` identify the session. `utterances_TCH` and `utterances_CDS` give the denominators for target-child and child-directed speech. `OIR_cs_all`/`OIR_cds_all` and their `_T1`, `_T2`, `_T3` fields give the corresponding repair counts. CSS fields, where present, concern child-surrounding speech and are excluded from these child/CDS count models. Other provided summary fields are preserved without reinterpretation.

All child/CDS counts are reconciled between these two originals by language, child and structured age, rather than by trusting row order.

## Derived analysis fields

| Field/output | Meaning |
|---|---|
| `child_id` | Language plus supplied child identifier, preventing cross-language collisions |
| `age_z` | (Age months - 32.5) / 5.1961524227 in primary data |
| `cds_rate_per1000` | Same-session observed CDS count/utterance ratio times 1,000, mapped to child and CDS rows |
| `cds_rate_z_lang` | Observed CDS rate centered/scaled over the 12 unique sessions per language |
| `child_cds_mean` | Mean observed CDS rate across that child's six sessions |
| `lang_cds_mean`, `lang_cds_sd` | Across the 12 unique sessions in the language |
| `cds_within_z` | (Session CDS rate - child's CDS mean) / language CDS SD |
| `cds_between_z` | (Child's CDS mean - language CDS mean) / language CDS SD |
| `standardization.csv` | Actual primary age and CDS centers/scales |
| `age_reconciliation.csv` | The single discrepancy between numeric ages in the supplied originals and its documented handling |

The within- and between-child predictors sum exactly to the total language-standardized CDS predictor. They are sensitivity-model predictors, not additional original measurements.

Primary analysis rows are sorted by language, child, age and speech source. The response counts are unchanged. The age-39 sensitivities change only the interpretation of the one discrepant numeric age and the resulting age standardization; their exact data are stored with each model.

## Totals and scope

There are six children, two per language, with six sessions each. Child speech contributes 18,426 utterances and 421 OIRs; CDS contributes 30,742 utterances and 1,031 OIRs. Together these total 49,168 utterances and 1,452 OIRs. `tables/observed_counts_and_rates.csv` supplies all corpus/source/type totals.

The package contains no paired rater annotation data and cannot independently recompute annotation reliability statistics.

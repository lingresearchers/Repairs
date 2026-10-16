input_endpoints <- function() {
  do.call(rbind,lapply(LANGUAGES,function(lang) {
    v <- DATA$child$cds_rate_per1000[DATA$child$language==lang]
    q <- unname(quantile(v,c(.16,.84),type=7))
    data.frame(language=lang,raw_low=q[1],raw_high=q[2],
      z_low=(q[1]-mean(v))/sd(v),z_high=(q[2]-mean(v))/sd(v))
  }))
}
fixed_rate <- function(model,lang,src="child",age_months=DATA$mean_age,input_z=0) {
  z <- (age_months-model$age_center)/model$age_scale
  new <- data.frame(language=factor(lang,levels=LANGUAGES),
    speech_source=factor(src,levels=SOURCES),age_z=z,cds_rate_z_lang=input_z,
    cds_within_z=input_z,cds_between_z=0)
  1000*rate_predictions(model,new,conditional=FALSE)[,1]
}
child_contrasts <- function(model) {
  endpoints <- input_endpoints()
  input <- age <- list()
  early <- DATA$mean_age-DATA$sd_age; late <- DATA$mean_age+DATA$sd_age
  for(lang in LANGUAGES) {
    q <- endpoints[endpoints$language==lang,]
    input[[lang]] <- fixed_rate(model,lang,input_z=q$z_high)-
                     fixed_rate(model,lang,input_z=q$z_low)
    age[[lang]] <- fixed_rate(model,lang,age_months=late)-
                   fixed_rate(model,lang,age_months=early)
  }
  input$equal_language_average <- Reduce("+",input)/3
  pairwise <- list()
  for(pair in list(c("chintang","russian"),c("indonesian","russian"),c("chintang","indonesian"))) {
    name <- paste(pair,collapse="_minus_")
    pairwise[[paste0("age_",name)]] <- age[[pair[1]]]-age[[pair[2]]]
    pairwise[[paste0("input_",name)]] <- input[[pair[1]]]-input[[pair[2]]]
  }
  list(input=input,age=age,pairwise=pairwise)
}
joint_contrasts <- function(model) {
  source <- list();h3 <- list()
  for(lang in LANGUAGES)
    source[[lang]] <- fixed_rate(model,lang,"child")-fixed_rate(model,lang,"CDS")
  stages <- c(early=DATA$mean_age-DATA$sd_age,mean=DATA$mean_age,late=DATA$mean_age+DATA$sd_age)
  for(stage in names(stages)) {
    child <- sapply(LANGUAGES,function(l) fixed_rate(model,l,"child",stages[stage]))
    cds <- sapply(LANGUAGES,function(l) fixed_rate(model,l,"CDS",stages[stage]))
    child_sd <- apply(child,1,sd);cds_sd <- apply(cds,1,sd)
    h3[[stage]] <- list(child_SD=child_sd,CDS_SD=cds_sd,delta_SD=child_sd-cds_sd)
  }
  changes <- list(child_SD_late_minus_early=h3$late$child_SD-h3$early$child_SD,
    CDS_SD_late_minus_early=h3$late$CDS_SD-h3$early$CDS_SD,
    delta_SD_late_minus_early=h3$late$delta_SD-h3$early$delta_SD)
  list(source=source,h3=h3,changes=changes,stages=stages)
}
format_contrasts <- function(model) {
  z0 <- (DATA$mean_age-model$age_center)/model$age_scale
  ze <- (DATA$mean_age-DATA$sd_age-model$age_center)/model$age_scale
  zl <- (DATA$mean_age+DATA$sd_age-model$age_center)/model$age_scale
  incidence <- composition <- list()
  for(lang in LANGUAGES) {
    c <- format_cohort(model,lang,"child",z0)
    a <- format_cohort(model,lang,"CDS",z0)
    cs <- format_cohort(model,lang,"child",z0,TRUE)
    as <- format_cohort(model,lang,"CDS",z0,TRUE)
    incidence[[paste0(lang,"_child_minus_CDS_T3")]] <- 1000*(c[,3]-a[,3])
    composition[[paste0(lang,"_child_minus_CDS_T3")]] <- 100*(cs[,3]-as[,3])
  }
  r <- format_cohort(model,"russian","child",z0)
  c <- format_cohort(model,"chintang","child",z0)
  rs <- format_cohort(model,"russian","child",z0,TRUE)
  cs <- format_cohort(model,"chintang","child",z0,TRUE)
  incidence$Russian_minus_Chintang_child_T3 <- 1000*(r[,3]-c[,3])
  composition$Russian_minus_Chintang_child_T3 <- 100*(rs[,3]-cs[,3])
  incidence$Russian_child_T3_minus_T1 <- 1000*(r[,3]-r[,1])
  composition$Russian_child_T3_minus_T1 <- 100*(rs[,3]-rs[,1])
  incidence$Chintang_child_T2_minus_T3 <- 1000*(c[,2]-c[,3])
  composition$Chintang_child_T2_minus_T3 <- 100*(cs[,2]-cs[,3])
  age <- list()
  for(lang in LANGUAGES) for(src in SOURCES) {
    e <- format_cohort(model,lang,src,ze)
    l <- format_cohort(model,lang,src,zl)
    for(k in 1:3) age[[paste(lang,src,TYPES[k],sep="_")]] <- 1000*(l[,k]-e[,k])
  }
  list(incidence=incidence,composition=composition,age=age)
}
rows_from_draws <- function(values,label="estimand") {
  do.call(rbind,lapply(names(values),function(n) {
    out <- data.frame(name=n,summarize_vector(values[[n]]))
    names(out)[1] <- label;out
  }))
}
write_latex_table <- function(df,name,caption,label) {
  # Booktabs-ready fragment. Numeric summaries are generated from the model draws.
  escape <- function(s) gsub("_","\\_",as.character(s),fixed=TRUE)
  show <- function(v) if(is.numeric(v)) ifelse(is.finite(v),formatC(v,digits=3,format="f"),"") else escape(v)
  lines <- c("\\begin{table}[htbp]","\\centering","\\small",
    paste0("\\caption{",caption,"}"),paste0("\\label{",label,"}"),
    "\\begin{adjustbox}{max width=\\linewidth}",
    paste0("\\begin{tabular}{",paste(rep("l",ncol(df)),collapse=""),"}"),
    "\\toprule",paste0(paste(escape(names(df)),collapse=" & ")," \\\\"),
    "\\midrule")
  for(i in seq_len(nrow(df))) lines <- c(lines,
    paste0(paste(vapply(df[i,,drop=FALSE],show,character(1)),collapse=" & ")," \\\\"))
  lines <- c(lines,"\\bottomrule","\\end{tabular}","\\end{adjustbox}","\\end{table}")
  writeLines(lines,file.path(ROOT,"documentation",name))
}
summarize_models <- function(manuscript_fragments=FALSE) {
  observed <- DATA$long |>
    dplyr::group_by(language,speech_source) |>
    dplyr::summarise(sessions=dplyr::n(),utterances=sum(utterance_n),OIR=sum(oir_n),
      T1=sum(oir_T1_n),T2=sum(oir_T2_n),T3=sum(oir_T3_n),.groups="drop") |>
    dplyr::mutate(rate_per1000=1000*OIR/utterances,T1_share_percent=100*T1/OIR,
      T2_share_percent=100*T2/OIR,T3_share_percent=100*T3/OIR)
  write_table(observed,"observed_counts_and_rates.csv")
  write_table(input_endpoints(),"H1_input_endpoints.csv")
  H1H2 <<- child_contrasts(MODELS$child_rate)
  H3SOURCE <<- joint_contrasts(MODELS$joint_rate)
  FORMAT_CONTRASTS <<- format_contrasts(MODELS$repair_formats)
  write_table(rows_from_draws(H1H2$input),"H1_input_associations.csv")
  write_table(rows_from_draws(H1H2$age),"child_age_changes.csv")
  write_table(rows_from_draws(H1H2$pairwise),"H2_pairwise_changes.csv")
  write_table(rows_from_draws(H3SOURCE$source,"language"),"speech_source_contrasts.csv")
  h3 <- do.call(rbind,lapply(names(H3SOURCE$h3),function(stage) {
    v <- H3SOURCE$h3[[stage]]
    data.frame(stage=stage,age_months=unname(H3SOURCE$stages[stage]),
      child_SD_median=median(v$child_SD),CDS_SD_median=median(v$CDS_SD),
      summarize_vector(v$delta_SD))
  }))
  write_table(h3,"H3_cross_corpus_dispersion.csv")
  write_table(rows_from_draws(H3SOURCE$changes),"H3_within_draw_changes.csv")
  baseline <- list()
  for(lang in LANGUAGES) for(src in SOURCES)
    baseline[[paste(lang,src,sep="_")]] <- fixed_rate(MODELS$joint_rate,lang,src)
  write_table(rows_from_draws(baseline),"joint_baseline_rates.csv")
  format_rows <- list();m <- MODELS$repair_formats;d <- draw_matrix(m)
  for(lang in LANGUAGES) for(src in SOURCES) {
    g <- match(paste(lang,src,sep="_"),m$condition_levels)
    p <- format_cohort(m,lang,src,0)
    share <- format_cohort(m,lang,src,0,TRUE)
    for(k in 1:3) {
      b <- summarize_vector(d[,paste0("baseline[",g,",",k,"]")])
      a <- summarize_vector(d[,paste0("age_slope[",g,",",k,"]")])
      r <- summarize_vector(1000*p[,k])
      sh <- summarize_vector(100*share[,k])
      names(b) <- paste0("fixed_logratio_",names(b))
      names(a) <- paste0("age_logratio_",names(a))
      names(r) <- paste0("rate_per1000_",names(r))
      names(sh) <- paste0("conditional_share_percent_",names(sh))
      format_rows[[paste(lang,src,k)]] <- data.frame(language=lang,source=src,type=TYPES[k],b,a,r,sh)
    }
  }
  format_table <- do.call(rbind,format_rows)
  write_table(format_table,"format_baselines_and_native_coefficients.csv")
  write_table(rows_from_draws(FORMAT_CONTRASTS$age),"format_age_rate_changes.csv")
  write_table(rows_from_draws(FORMAT_CONTRASTS$incidence),"format_incidence_contrasts.csv")
  write_table(rows_from_draws(FORMAT_CONTRASTS$composition),"format_conditional_composition_contrasts.csv")
  # New figures and all reported numbers use these same posterior objects.
  curves <- list();zage <- seq(25,40,length.out=61)
  for(lang in LANGUAGES) for(src in SOURCES) for(age in zage) {
    v <- fixed_rate(MODELS$joint_rate,lang,src,age)
    curves[[paste(lang,src,age)]] <- data.frame(language=lang,source=src,age_months=age,
      summarize_vector(v))
  }
  JOINT_CURVES <<- do.call(rbind,curves)
  write_table(JOINT_CURVES,"joint_expected_rate_curves.csv")
  curves <- list()
  for(lang in LANGUAGES) for(src in SOURCES) for(age in zage) {
    p <- format_cohort(m,lang,src,(age-m$age_center)/m$age_scale)
    for(k in 1:3) curves[[paste(lang,src,age,k)]] <- data.frame(language=lang,source=src,
      age_months=age,type=TYPES[k],summarize_vector(1000*p[,k]))
  }
  FORMAT_CURVES <<- do.call(rbind,curves)
  write_table(FORMAT_CURVES,"format_expected_rate_curves.csv")
  # Full sensitivity outputs, including an explicit label for the changed WB input target.
  sens_rows <- list();sens_draws <- list()
  all <- c(MODELS,SENSITIVITY)
  for(n in names(all)) {
    model <- all[[n]]
    values <- if(model$config$kind=="child") {
      c <- child_contrasts(model)
      c(list(H1_equal_language_average=c$input$equal_language_average),
        setNames(c$age,paste0("child_age_",names(c$age))))
    } else if(model$config$kind=="joint") {
      c <- joint_contrasts(model)
      c(setNames(c$source,paste0("source_",names(c$source))),
        list(H3_late_delta_SD=c$h3$late$delta_SD,
          H3_delta_SD_change=c$changes$delta_SD_late_minus_early))
    } else {
      c <- format_contrasts(model)
      c(setNames(c$incidence,paste0("incidence_",names(c$incidence))),
        setNames(c$composition,paste0("composition_",names(c$composition))),c$age)
    }
    for(key in names(values)) {
      target <- if(isTRUE(model$config$within_between) && startsWith(key,"H1"))
        "Within-child input association at mean between-child CDS rate" else
          "Same physical ages and endpoints as primary; see estimand dictionary"
      sens_rows[[paste(n,key)]] <- data.frame(model=n,kind=model$config$kind,
        estimand=key,target=target,summarize_vector(values[[key]]))
    }
    sens_draws[[n]] <- values
  }
  SENSITIVITY_SUMMARIES <<- do.call(rbind,sens_rows)
  SENSITIVITY_CONTRAST_DRAWS <<- sens_draws
  write_table(SENSITIVITY_SUMMARIES,"sensitivity_all_contrasts.csv")
  curvature <- list()
  for(n in names(all)) {
    model <- all[[n]]
    if(!isTRUE(model$config$quadratic)) next
    values <- if(model$config$kind=="format") {
      d <- draw_matrix(model)
      setNames(lapply(1:2,function(s) d[,paste0("quadratic[",s,"]")]),SOURCES)
    } else {
      b <- coefficient_draws(model)
      indices <- which(grepl("I(age_z^2)",colnames(b),fixed=TRUE))
      setNames(lapply(indices,function(k) b[,k]),colnames(b)[indices])
    }
    curvature[[n]] <- data.frame(model=n,rows_from_draws(values,"coefficient"))
  }
  write_table(do.call(rbind,curvature),"age_curvature_coefficients.csv")
  if(!isTRUE(manuscript_fragments)) return(invisible(TRUE))
  write_latex_table(rows_from_draws(H1H2$input),"table_H1.tex",
    "Observed CDS P16--P84 associations at mean age with child deviations set to zero; changes per 1,000 utterances.","tab:h1_results")
  write_latex_table(rows_from_draws(H1H2$pairwise),"table_H2.tex",
    "Pairwise differences in age-related rate change and language-specific CDS percentile associations; not logit-slope differences.","tab:h2_results")
  write_latex_table(h3,"table_H3.tex",
    "Posterior descriptive dispersion across the three fitted corpus rates. The contrast is child SD minus CDS SD.","tab:h3_results")
  write_latex_table(rows_from_draws(H3SOURCE$source,"language"),"table_speech_sources.tex",
    "Child minus CDS expected repair rates at mean age and zero child deviations, per 1,000 utterances.","tab:speech_source_contrasts")
  write_latex_table(rows_from_draws(FORMAT_CONTRASTS$age),"table_format_age_changes.tex",
    "Sample-conditional mean format incidence changes from early to late age, per 1,000 utterances.","tab:format_age_changes")
  interval <- function(md,lo,hi) sprintf("%.2f [%.2f, %.2f]",md,lo,hi)
  type_table <- data.frame(Language=unname(LANGUAGE_LABELS[format_table$language]),
    Source=ifelse(format_table$source=="child","CS","CDS"),
    Format=as.character(match(format_table$type,TYPES)),
    Incidence=interval(format_table$rate_per1000_median,format_table$rate_per1000_lower,format_table$rate_per1000_upper),
    Composition=interval(format_table$conditional_share_percent_median,format_table$conditional_share_percent_lower,format_table$conditional_share_percent_upper),
    Age_coefficient=interval(format_table$age_logratio_median,format_table$age_logratio_lower,format_table$age_logratio_upper))
  write_latex_table(type_table,"table_type_model.tex",
    "Dirichlet--multinomial format summaries at mean age: incidence per 1,000 utterances and composition among repairs are equal-child means of probabilities; linear age coefficients are local log-relative-odds slopes against non-repair at mean age, per age SD, in models with quadratic age terms. Entries are posterior medians with 95\\% credible intervals. Formats 1--3 denote open requests, restricted requests, and restricted offers.","tab:type_model_results")
  write_latex_table(rows_from_draws(FORMAT_CONTRASTS$incidence),"table_format_incidence_contrasts.tex",
    "Format incidence contrasts at mean age, per 1,000 utterances; equal-child mean predicted probabilities.","tab:format_incidence_contrasts")
  write_latex_table(rows_from_draws(FORMAT_CONTRASTS$composition),"table_format_composition_contrasts.tex",
    "Format composition contrasts at mean age, percentage points among repairs; equal-child mean conditional probabilities.","tab:format_composition_contrasts")
  write_manuscript_notes()
  write_results_replacements()
}
write_results_replacements <- function() {
  get <- function(v,k) v[[k]]
  report <- function(v,direction="positive") {
    x <- summarize_vector(v)
    sprintf("%.2f, 95\\%% CrI [%.2f, %.2f], $P(\\Delta %s 0)=%.3f$",
      x$median,x$lower,x$upper,if(direction=="positive") ">" else "<",
      if(direction=="positive") x$P_gt0 else x$P_lt0)
  }
  h <- report(H1H2$input$equal_language_average)
  ages <- paste(vapply(LANGUAGES,function(l)
    paste0(LANGUAGE_LABELS[l],": ",report(H1H2$age[[l]])),character(1)),collapse="; ")
  stages <- paste(vapply(names(H3SOURCE$h3),function(stage)
    paste0(stage,": ",report(H3SOURCE$h3[[stage]]$delta_SD,"negative")),character(1)),collapse="; ")
  sources <- paste(vapply(LANGUAGES,function(l)
    paste0(LANGUAGE_LABELS[l],": ",report(H3SOURCE$source[[l]],"negative")),character(1)),collapse="; ")
  formats <- paste(vapply(LANGUAGES,function(l)
    paste0(LANGUAGE_LABELS[l],": ",report(FORMAT_CONTRASTS$incidence[[paste0(l,"_child_minus_CDS_T3")]],"negative")),character(1)),collapse="; ")
  input_rows <- rows_from_draws(H1H2$input[LANGUAGES])
  input_text <- if(all(input_rows$lower<=0 & input_rows$upper>=0))
    "The language-specific 95\\% credible intervals all included zero" else
      "Language-specific posterior estimates and intervals are reported"
  ind <- summarize_vector(H3SOURCE$source$indonesian)
  ind_text <- if(ind$lower<=0 && ind$upper>=0)
    "Although the Indonesian posterior favored a lower child rate, its 95\\% credible interval included zero. The corrected analysis therefore does not retain the previous claim of an Indonesian interval excluding zero." else
      "The Indonesian contrast's 95\\% credible interval excluded zero. This is an association within the recorded material, not a causal community effect."
  fa <- rows_from_draws(FORMAT_CONTRASTS$age)
  resolved <- fa$lower>0 | fa$upper<0
  age_text <- if(!any(resolved)) sprintf("All %d primary early-to-late format-incidence intervals included zero.",nrow(fa)) else
    sprintf("Of %d early-to-late format-incidence contrasts, %d had a 95\\%% interval excluding zero; their exact estimates and directions are supplied in the supplementary age-change table.",nrow(fa),sum(resolved))
  lines <- c("% Generated from the final model draws. See MANUSCRIPT_UPDATE_NOTES.md for replacement boundaries.",
    "% Abstract: replace the complete existing abstract.",
    "\\begin{abstract}",
    "Human conversation depends on coordinating understanding and resolving communicative breakdowns. Other-initiated repair provides a means of doing so, but its development across linguistic settings remains poorly understood. We analyzed longitudinal spontaneous interactions involving six children aged 25--40 months and their interlocutors in Chintang, Indonesian, and Russian. Bayesian multilevel count models evaluated concurrent caregiver--child rate associations, age-related variation, and repair formats. Children in all three corpora used open and restricted clarification requests, while repair-format distributions varied across corpora and conversational roles. Estimated role contrasts favored fewer restricted offers in child speech than in child-directed speech. Concurrent caregiver--child rate associations and comparisons of early-to-late rate change remained uncertain. Predictive checks motivated allowing curved age-related patterns rather than assuming constant age trends. These findings document a shared repair repertoire alongside variation in its use, while leaving the relative contributions of experience, grammatical resources, and participation frameworks open to further investigation.",
    "\\end{abstract}","",
    "% Results -> H1: Same-Session Caregiver Input Association on Child Repair Rate: replace all narrative paragraphs in this subsection and its old table; insert table_H1.tex.",
    paste0("We estimated child repair-rate differences between the observed 16th and 84th percentiles of same-session CDS repair rates within each language, at mean age with child deviations set to zero. The equal-language-weight association was ",h," OIRs per 1,000 utterances. ",input_text," (Table~\\ref{tab:h1_results}). These estimates do not establish independence from input, equivalence, or a causal effect of caregiver behavior."),"",
    "% Results -> H2: Cross-Linguistic Comparison of Developmental Trajectories and Input Sensitivity: replace all narrative paragraphs and old table; insert table_H2.tex.",
    sprintf("We compared expected child repair-rate changes over the same physical age interval, %.2f--%.2f months, holding the language-standardized CDS predictor at zero. The estimated changes per 1,000 utterances were %s. Pairwise differences in these age-related changes are reported in Table~\\ref{tab:h2_results}. Input contrasts in that table compare language-specific observed CDS percentile associations; they are not contrasts between regression slopes. Wide credible intervals preclude strong claims that developmental trajectories are identical or reliably different.",DATA$mean_age-DATA$sd_age,DATA$mean_age+DATA$sd_age,ages),"",
    "% Results -> H3: Cross-Linguistic Differentiation Across Development: replace all narrative paragraphs and old table; insert table_H3.tex.",
    paste0("At each age, we calculated the standard deviation across the three fitted corpus rates separately for child speech and CDS, with child deviations set to zero. The child-minus-CDS SD contrasts per 1,000 utterances were ",stages,". These are descriptive comparisons among the three study corpora. Stage-specific contrasts and within-draw changes across age are supplied separately; neither establishes population-level convergence across languages."),"",
    "% Results -> Localized Speech Source Contrast: The Indonesian Speech-Source Gap: replace narrative and old table; insert table_speech_sources.tex. Rename the heading to Speech-Source Contrasts at Mean Age, retaining its existing label.",
    paste0("Child-minus-CDS expected rate contrasts at mean age were ",sources," OIRs per 1,000 utterances (Table~\\ref{tab:speech_source_contrasts}). ",ind_text," Correlated-effects, linear-age and age-reconciliation sensitivities are reported in the supplementary materials."),"",
    "% Results -> Distribution of Repair Initiation Types Across Languages and Speech Sources -> Bayesian Multilevel Findings and Posterior Contrasts: replace all narrative paragraphs and old table until the next subsection; insert table_type_model.tex and the two format-contrast tables as needed.",
    paste0("The four-category Dirichlet--multinomial model jointly estimated non-repair and the three repair formats. Table~\\ref{tab:type_model_results} distinguishes format incidence per utterance from composition among repairs. At mean age, sample-conditional restricted-offer incidence contrasts between child speech and CDS were ",formats," occurrences per 1,000 utterances. The Russian-minus-Chintang child restricted-offer incidence contrast was ",report(FORMAT_CONTRASTS$incidence$Russian_minus_Chintang_child_T3)," per 1,000 utterances. These estimates describe the fitted children and recordings. They do not identify causal effects of morphological structure or universal developmental constraints."),
    paste0(age_text," These endpoint contrasts do not rule out curved age-related patterns. Native linear age coefficients describe local log-relative-odds slopes against non-repair at mean age, in models that also include source-specific quadratic age terms. They should not be interpreted as marginal binomial logit slopes or as evidence that every child repair format decreased with age. Prior, linear-age and child-effect sensitivities are provided in the supplementary tables."),"",
    "% Discussion -> H1: replace the opening statistical summary paragraph.",
    paste0("The corrected analysis left the concurrent caregiver--child repair-rate association uncertain (equal-language-weight contrast: ",h," per 1,000 utterances). This analysis concerns session-level associations over observed CDS rate ranges. It does not test developmental independence from input or rule out cumulative experience, sequential contingencies, or effects at other timescales."),"",
    "% Discussion -> H2: replace claims of universal declines or causally determined grammar effects.",
    "The recordings show a shared set of repair formats and corpus-specific differences in their use. Comparisons of early-to-late endpoints do not capture all within-interval age variation: predictive checks motivated allowing curved age patterns. Morphosyntactic resources remain plausible contributors to observed differences, but language, setting, and participant composition are confounded in this design; the estimates cannot assign those differences to grammar alone.","",
    "% Discussion -> H3: replace claims of statistically demonstrated convergence and a decisive Indonesian gap.",
    paste0("The stage-specific dispersion contrasts and within-draw changes must be interpreted as descriptive summaries of the three fitted corpora. ",ind_text," These patterns motivate further study of participation frameworks; they do not establish a population-wide developmental convergence or a causal community effect."))
  writeLines(lines,file.path(ROOT,"documentation","manuscript_results_replacements.tex"))
}
write_manuscript_notes <- function() {
  h <- rows_from_draws(H1H2$input)
  a <- h[h$estimand=="equal_language_average",]
  source <- rows_from_draws(H3SOURCE$source)
  ind <- source[source$estimand=="indonesian",]
  late <- H3SOURCE$h3$late$delta_SD
  sampler <- do.call(rbind,lapply(MODELS,portable_diagnostics))
  lines <- c("# Manuscript alignment for the corrected supplementary analysis","",
    "The previous numerical replacements described earlier fits. The final package uses new model specifications and estimates; do not keep the old numbers or likelihood descriptions.",
    "",
    "## Exact replacement locations and files",
    "",
    "Keep the existing Methods subsection heading `Bayesian Multilevel Modeling Framework` and replace its full body, including the old child/joint/format equations, priors, diagnostics and sensitivity paragraphs, up to the `Results` section, with `statistical_methods_replacement.tex`.",
    "Replace the complete abstract and the Results/Discussion passages at the section boundaries identified in comments in `manuscript_results_replacements.tex`. Replace the old H1, H2, H3, speech-source and format-model tables with the corresponding `table_*.tex` files; their existing main-table labels are retained. Use the incidence and conditional-composition tables separately.",
    "Replace the image files for main Figures 1--5 with the files of the same names in `figures/`, then replace their captions using `figure_caption_replacements.tex`. Existing figure labels are retained. The supplemental caption snippets cover Figures S1--S4.",
    "These fragments require the existing manuscript's `amsmath`, `booktabs`, `adjustbox` and figure support packages. They are replacement snippets, not a second full manuscript. The released snippets were syntax-checked by LaTeX draft compilation.",
    "",
    "## Methods: Bayesian Multilevel Modeling Framework",
    "",
    "All three models use the same canonical ages and standardization (mean 32.5 months; SD 5.19615242). Both child-only and joint source rate models are beta-binomial. Their child-effect SDs are pooled across languages. The format model is a four-category Dirichlet-multinomial with non-repair as reference and source/type-specific child deviations, whose SDs are pooled across languages. Primary models include regularized quadratic age terms: one shared coefficient in the child-only model and one per source in the joint and format models. These terms were introduced after squared-age residual checks found inadequate linear trends; linear-age alternatives are retained as actual sensitivity refits. The implementation is Stan through cmdstanr, not brms; saved RDS objects are portable lists documented in the model dictionary.",
    "",
    "Child rate priors: baseline Normal(-3.5,1.25); linear/quadratic age and input coefficients Normal(0,0.6); varying-effect SDs half-Normal(0,0.5); precision Gamma(2,0.05), with the second Gamma argument a rate. Format priors: baseline log relative odds versus non-repair Normal(-4.5,1.5); linear/quadratic age coefficients Normal(0,0.6); source/type child SDs half-Normal(0,0.5); precision Gamma(2,0.05). Primary child effects are uncorrelated; correlated rate-model sensitivities use LKJ(2).",
    "",
    "The 39/40 discrepancy is resolved for primary analysis using structured target_age = 3;04.00 and the matching long-data age of 40. Originals are preserved. All three model families are refit with age 39 as a sensitivity. This is cross-field reconciliation, not independent verification against the recordings.",
    "",
    "## Results that must be replaced",
    "",
    sprintf("H1: the equal-language-weight association across observed CDS P16--P84 endpoints is %.2f OIR per 1,000 [%.2f, %.2f], P(positive)=%.4f. This is an observed-rate association, not a causal or equivalence test.",a$median,a$lower,a$upper,a$P_gt0),
    "",
    "H2: replace the full table with documentation/table_H2.tex and use tables/H2_pairwise_changes.csv. Age contrasts compare changes over the same physical early/late interval. Input contrasts use each language's empirical CDS percentiles and are not differences between regression slopes.",
    "",
    sprintf("H3: at late age the child-minus-CDS descriptive cross-corpus SD contrast is %.2f [%.2f, %.2f], P(negative)=%.4f. Report the within-draw changes in tables/H3_within_draw_changes.csv separately from stage contrasts.",
      median(late),quantile(late,.025),quantile(late,.975),mean(late<0)),
    "",
    sprintf("Indonesian source contrast at mean age: %.2f OIR per 1,000 [%.2f, %.2f], P(child<CDS)=%.4f. Replace all old source-contrast numbers and old sensitivity claims.",ind$median,ind$lower,ind$upper,ind$P_lt0),
    "",
    "Repair formats: replace the binomial equation, old baseline/slopes table, all format contrasts and age-effect statements. Native coefficients compare each repair category with non-repair; the linear age coefficient is local at mean age in the quadratic model. The primary reported format rates average the fitted children's probabilities, not their logits. Per-utterance incidence and composition conditional on OIR are provided in separate tables. Endpoint changes do not capture all curved within-interval variation; use the curves and curvature coefficient table as well.",
    "",
    "## Figure captions",
    "",
    "Figure 1: joint beta-binomial expected rates with 95% credible bands; zero child deviations. Points use reconciled analysis ages and observed rates. No artificial age offsets. The shaded bands describe uncertainty in expected rates, not prediction intervals for individual session counts.",
    "Figure 2: H1 observed CDS percentile association and H2 pairwise age-change contrasts from the child beta-binomial. No unsupported numerical annotation is entered manually.",
    "Figure 4: descriptive child-minus-CDS SD over these three fitted language curves at early, mean and late age. It is not population language variance or proof of convergence.",
    "Figure 5: joint model baseline rate intervals and child-minus-CDS contrasts at mean age; zero child deviations.",
    "Figure 3: empirical percentages among OIRs, calculated from the original count data. This figure is descriptive and is not a per-utterance model rate.",
    "Figure S1: Dirichlet-multinomial sample-conditional format rates; equal-child mean probabilities with 95% credible bands.",
    "Figures S2--S4: predictive adequacy, prior predictive behavior and sensitivity of key posterior contrasts.",
    "",
    "## Claims requiring interpretation changes",
    "",
    "An interval containing zero does not establish no association, trajectory equivalence or independence from input. These six children do not identify universal mechanisms or causal grammatical/community effects. Do not retain a claim solely because it was supported by the earlier underdispersed binomial model.",
    "",
    "## Annotation reliability",
    "",
    "The supplied original archive contains session-level counts, not paired rater annotations. This package reproduces the count-model analyses; it cannot recompute the manuscript's kappa values. Do not claim that those reliability values were independently verified here.")
  writeLines(lines,file.path(ROOT,"documentation","MANUSCRIPT_UPDATE_NOTES.md"))
  methods <- c("% Replace the statistical model descriptions; retain original citation keys as appropriate.",
    "\\paragraph{Corrected rate models.}",
    "We fitted child-only and joint child--CDS beta-binomial models in Stan through \\texttt{cmdstanr}. For each session--source observation,",
    "\\begin{align}",
    "Y_i &\\sim \\operatorname{BetaBinomial}(N_i,\\mu_i\\phi,(1-\\mu_i)\\phi),\\\\",
    "\\operatorname{logit}(\\mu_i)&=\\mathbf{x}_i^{\\mathsf T}\\boldsymbol\\beta+\\mathbf{z}_i^{\\mathsf T}\\mathbf{u}_{c[i]}.",
    "\\end{align}",
    "The child-only model includes language-specific baselines, linear age associations, observed same-session CDS rate associations, and a shared quadratic age coefficient. The joint model includes language--source baselines and linear age associations, with source-specific quadratic age coefficients shared across languages. Child-effect standard deviations are pooled across languages; primary varying effects are uncorrelated. Age is standardized across the 36 sessions (mean 32.5 months, SD 5.196 months). CDS rates are standardized across the 12 sessions within each language.",
    "Child-only varying effects comprise an intercept, linear age deviation and CDS-rate deviation for each child. The joint model includes an intercept and linear age deviation for each speech source within child. Each component's standard deviation is pooled across the six children.",
    "\\paragraph{Corrected repair-format model.}",
    "We modeled non-repair, open requests, restricted requests and restricted offers jointly:",
    "\\begin{align}",
    "\\mathbf{Y}_i&\\sim\\operatorname{DirichletMultinomial}(N_i,\\phi\\boldsymbol\\pi_i),\\\\",
    "\\boldsymbol\\pi_i&=\\operatorname{softmax}(0,\\eta_{i1},\\eta_{i2},\\eta_{i3}),\\\\",
    "\\eta_{ik}&=\\beta_{0,L[i],S[i],k}+\\beta_{\\mathrm{age},L[i],S[i],k}\\mathrm{Age}_{z,i}+\\gamma_{S[i]}\\mathrm{Age}_{z,i}^{2}+u_{c[i],S[i],k}.",
    "\\end{align}",
    "Source--format child deviations have regularized standard deviations pooled across languages. Each source has a regularized quadratic age coefficient shared across languages and repair types. Format-rate summaries average predicted probabilities over the two fitted children within each corpus. Format incidence per utterance and format composition conditional on repair are reported separately. Native coefficients describe log relative odds against non-repair; linear age coefficients are local slopes at mean age.",
    "\\paragraph{Priors and estimation.}",
    "Rate-model fixed baselines have $\\operatorname{Normal}(-3.5,1.25)$ priors; format baselines have $\\operatorname{Normal}(-4.5,1.5)$ priors. Linear and quadratic age coefficients and observed CDS-rate coefficients have $\\operatorname{Normal}(0,0.6)$ priors. Varying-effect standard deviations have half-$\\operatorname{Normal}(0,0.5)$ priors, with standard-normal non-centered latent effects. Precision parameters have $\\operatorname{Gamma}(2,0.05)$ priors, where the second argument is a rate.",
    "Each primary model was sampled with four NUTS chains, 2,000 warmup and 2,000 retained iterations per chain, yielding 8,000 posterior draws. Sensitivity fits used four chains with 1,500 warmup and 1,500 retained iterations, yielding 6,000 draws each. The target acceptance probability was 0.995 and maximum tree depth was 15. We report posterior medians, equal-tailed 95\\% credible intervals and draw-based directional probabilities; these are not frequentist significance tests.",
    sprintf("Across primary fits, the maximum $\\hat{R}$ was %.4f, minimum bulk ESS was %.1f, minimum tail ESS was %.1f and minimum E-BFMI was %.3f. There were no divergent transitions or maximum-tree-depth hits. Full parameter and contrast diagnostics are supplied with the model exports.",max(sampler$max_Rhat),min(sampler$min_bulk_ESS),min(sampler$min_tail_ESS),min(sampler$min_E_BFMI)),
    "\\paragraph{Validation and sensitivity analyses.}",
    "Quadratic terms were added during this post-audit reanalysis after posterior predictive squared-age residual checks identified inadequate linear trends. We evaluated sampler diagnostics, prior and posterior predictive behavior, and sensitivity to linear-only age trends, correlated rate-model child effects, within-/between-child input decomposition, format priors, a shared format child effect and the alternative 39-month age entry. Numerical diagnostics and all 12 sensitivity fits are supplied with the reproducible analysis.")
  writeLines(methods,file.path(ROOT,"documentation","statistical_methods_replacement.tex"))
}

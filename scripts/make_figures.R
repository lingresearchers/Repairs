FIGURE_NAMES <- c("Fig1_Longitudinal_OIR_Rates","Fig2_H1_H2_Posterior_Contrasts",
  "Fig4_H3_CrossLinguistic_Differentiation","Fig5_SpeechSource_Posterior_Contrasts",
  "Fig3_Repair_Types_Combined","FigS1_Format_Expected_Rates",
  "FigS2_Posterior_Predictive_Checks","FigS3_Prior_Predictive_Checks",
  "FigS4_Sensitivity_Contrasts")
make_figures <- function() {
  library(ggplot2)
  role_colors <- c(child="#0072B2",CDS="#D55E00")
  language_colors <- c(chintang="#009E73",indonesian="#D55E00",russian="#0072B2",
    equal_language_average="#333333")
  format_colors <- c(T1_Open="#E69F00",T2_RestrictedReq="#0072B2",T3_RestrictedOff="#009E73")
  figure_theme <- theme_bw(base_size=12)+theme(panel.grid.minor=element_blank(),
    strip.background=element_rect(fill="#F2F2F2"),legend.position="bottom",
    plot.title=element_text(face="bold",size=13),
    plot.subtitle=element_text(size=10),plot.caption=element_text(hjust=0,size=9),
    plot.margin=margin(10,12,10,10))
  save_figure <- function(plot,name,width=10,height=5.5) {
    # Validate a completed temporary export before replacing a released figure.
    for(extension in c("pdf","png")) {
      target <- file.path(ROOT,"figures",paste0(name,".",extension))
      temporary <- tempfile("oir_figure_",tmpdir=file.path(ROOT,"figures"),
        fileext=paste0(".",extension))
      expected <- if(extension=="pdf") charToRaw("%PDF") else
        as.raw(c(137,80,78,71,13,10,26,10))
      bytes <- raw()
      for(attempt in 1:2) {
        if(extension=="pdf")
          ggsave(temporary,plot,width=width,height=height,
            device=grDevices::cairo_pdf,bg="white")
        else ggsave(temporary,plot,width=width,height=height,
          device="png",dpi=300,bg="white",limitsize=TRUE)
        size <- file.info(temporary)$size
        bytes <- if(is.finite(size) && size>5000)
          readBin(temporary,"raw",n=size) else raw()
        if(length(bytes)>5000 && identical(bytes[seq_along(expected)],expected)) break
      }
      unlink(temporary)
      if(length(bytes)<=5000 || !identical(bytes[seq_along(expected)],expected))
        stop("Incomplete figure export: ",name,".",extension)
      writeBin(bytes,target)
    }
  }
  facet_lang <- function(d) {d$language <- factor(d$language,LANGUAGES,LANGUAGE_LABELS);d}
  curves <- facet_lang(JOINT_CURVES);points <- facet_lang(DATA$long)
  p <- ggplot(curves,aes(age_months,median,color=source,fill=source))+
    geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.15,color=NA,show.legend=FALSE)+geom_line(linewidth=.85)+
    geom_point(data=points,aes(age_months,1000*oir_n/utterance_n,color=speech_source,
      shape=speech_source),inherit.aes=FALSE,size=1.8,alpha=.65)+
    facet_wrap(~language,ncol=1,scales="free_y")+scale_color_manual(values=role_colors,breaks=SOURCES,limits=SOURCES,labels=SOURCE_LABELS)+
    scale_fill_manual(values=role_colors,breaks=SOURCES,limits=SOURCES,labels=SOURCE_LABELS,guide="none")+
    scale_shape_manual(values=c(child=16,CDS=17),breaks=SOURCES,limits=SOURCES,labels=SOURCE_LABELS)+
    labs(x="Child age (months)",y="OIRs per 1,000 utterances",color=NULL,fill=NULL,shape=NULL,
      title="Expected repair rates and observed sessions",
      subtitle="Joint beta-binomial model; zero child deviations; 95% credible bands",
      caption="Points use reconciled analysis ages. Bands describe expected-rate uncertainty, not session prediction intervals.")+figure_theme
  save_figure(p,FIGURE_NAMES[1],8,10)
  input <- rows_from_draws(H1H2$input)
  input$label <- factor(input$estimand,rev(c(LANGUAGES,"equal_language_average")),
    rev(c(unname(LANGUAGE_LABELS),"Equal-language average")))
  p1 <- ggplot(input,aes(median,label,color=estimand))+geom_vline(xintercept=0,color="grey55",linetype=2)+
    geom_errorbarh(aes(xmin=lower,xmax=upper),height=.15)+geom_point(size=2.5)+
    scale_color_manual(values=language_colors,guide="none")+
    labs(x="Change per 1,000 utterances",y=NULL,title="A. H1: Observed CDS percentile association",
      subtitle="Within-language P16 to P84; mean age; zero child deviations")+figure_theme
  age <- rows_from_draws(H1H2$pairwise)
  age <- age[startsWith(age$estimand,"age_"),]
  age$label <- c("Chintang minus Russian","Indonesian minus Russian","Chintang minus Indonesian")
  age$label <- factor(age$label,rev(age$label))
  p2 <- ggplot(age,aes(median,label))+geom_vline(xintercept=0,color="grey55",linetype=2)+
    geom_errorbarh(aes(xmin=lower,xmax=upper),height=.15,color="#0072B2")+
    geom_point(size=2.5,color="#0072B2")+
    labs(x="Difference in age change per 1,000",y=NULL,title="B. H2: Differences in age-related change",
      subtitle=sprintf("Same age interval: %.2f to %.2f months",DATA$mean_age-DATA$sd_age,DATA$mean_age+DATA$sd_age))+figure_theme
  save_figure(gridExtra::arrangeGrob(p1,p2,ncol=1),FIGURE_NAMES[2],9,8)
  dens <- annotations <- list()
  for(stage in names(H3SOURCE$h3)) {
    v <- H3SOURCE$h3[[stage]]$delta_SD
    dens[[stage]] <- data.frame(stage=stage,value=v)
    annotations[[stage]] <- data.frame(stage=stage,label=sprintf("P(child SD < CDS SD) = %.3f",mean(v<0)))
  }
  dens <- do.call(rbind,dens);annotations <- do.call(rbind,annotations)
  stages <- names(H3SOURCE$h3)
  stage_labels <- sprintf("%s age: %.2f months",c("Early","Mean","Late"),H3SOURCE$stages)
  dens$stage <- factor(dens$stage,stages,stage_labels)
  annotations$stage <- factor(annotations$stage,stages,stage_labels)
  p <- ggplot(dens,aes(value))+geom_density(fill="#0072B2",color="#0072B2",alpha=.25)+
    geom_vline(xintercept=0,linetype=2,color="grey45")+facet_wrap(~stage,ncol=1)+
    geom_text(data=annotations,aes(x=Inf,y=Inf,label=label),inherit.aes=FALSE,
      hjust=1.05,vjust=1.5,size=3)+
    labs(x="Child SD minus CDS SD (OIRs per 1,000)",y="Posterior density",
      title="Q3: Descriptive dispersion across the three fitted corpus rates",
      subtitle="Joint beta-binomial; zero child deviations",
      caption="This summarizes the three study corpora, not population-level variation across languages.")+figure_theme
  save_figure(p,FIGURE_NAMES[3],8,8)
  baseline <- read.csv(file.path(ROOT,"tables","joint_baseline_rates.csv"))
  baseline$source <- ifelse(endsWith(baseline$estimand,"_child"),"child","CDS")
  baseline$language <- sub("_(child|CDS)$","",baseline$estimand)
  baseline <- facet_lang(baseline)
  baseline$label <- factor(baseline$source,rev(SOURCES),rev(SOURCE_LABELS))
  p1 <- ggplot(baseline,aes(median,label,color=source))+
    geom_errorbarh(aes(xmin=lower,xmax=upper),height=.15)+geom_point(size=2.7)+
    facet_wrap(~language,nrow=1)+scale_color_manual(values=role_colors,guide="none")+
    labs(x="Expected OIRs per 1,000 utterances",y=NULL,title="A. Mean-age expected rates")+figure_theme
  source <- rows_from_draws(H3SOURCE$source,"language")
  source$label <- factor(source$language,rev(LANGUAGES),rev(LANGUAGE_LABELS))
  p2 <- ggplot(source,aes(median,label,color=language))+
    geom_vline(xintercept=0,linetype=2,color="grey50")+
    geom_errorbarh(aes(xmin=lower,xmax=upper),height=.15)+geom_point(size=2.7)+
    scale_color_manual(values=language_colors,guide="none")+
    labs(x="Child minus CDS expected OIRs per 1,000",y=NULL,
      title="B. Posterior speech-source contrasts",subtitle="95% credible intervals; zero child deviations")+figure_theme
  save_figure(gridExtra::arrangeGrob(p1,p2,ncol=1),FIGURE_NAMES[4],8,7)
  obs <- read.csv(file.path(ROOT,"tables","observed_counts_and_rates.csv"))
  bars <- tidyr::pivot_longer(obs,c("T1","T2","T3"),names_to="type",values_to="count")
  bars$type <- factor(bars$type,c("T1","T2","T3"),TYPES)
  bars$percentage <- 100*bars$count/bars$OIR
  bars$language <- factor(bars$language,LANGUAGES,LANGUAGE_LABELS)
  bars$source <- factor(bars$speech_source,SOURCES,SOURCE_LABELS)
  bars$label <- sprintf("%.1f%% (%d)",bars$percentage,bars$count)
  p <- ggplot(bars,aes(source,percentage,fill=type))+geom_col(width=.7,color="white")+
    geom_text(aes(label=label),position=position_stack(vjust=.5),size=3.1,color="#111111")+
    facet_wrap(~language,nrow=1)+scale_fill_manual(values=format_colors,labels=TYPE_LABELS)+
    labs(x=NULL,y="Percentage of observed OIRs",fill=NULL,
      title="Observed repair-format composition",
      subtitle="Labels show percent (count), calculated from original data; denominators are OIRs")+
    scale_y_continuous(breaks=seq(0,100,25),expand=expansion(mult=c(0,.02)))+figure_theme+
    theme(axis.text.x=element_text(size=10))
  save_figure(p,FIGURE_NAMES[5],9,6)
  f <- facet_lang(FORMAT_CURVES)
  f$type <- factor(f$type,TYPES,TYPE_LABELS)
  p <- ggplot(f,aes(age_months,median,color=source,fill=source))+
    geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.15,color=NA,show.legend=FALSE)+geom_line(linewidth=.75)+
    facet_grid(type~language,scales="free_y")+
    scale_color_manual(values=role_colors,breaks=SOURCES,limits=SOURCES,labels=SOURCE_LABELS)+
    scale_fill_manual(values=role_colors,breaks=SOURCES,limits=SOURCES,labels=SOURCE_LABELS,guide="none")+
    labs(x="Child age (months)",y="Format occurrences per 1,000 utterances",color=NULL,fill=NULL,
      title="Expected repair-format incidence",
      subtitle="Dirichlet-multinomial; equal-child mean probabilities; 95% credible bands")+figure_theme
  save_figure(p,FIGURE_NAMES[6],11,8)
  ppc <- list()
  model_labels <- c(child_rate="Child rate",joint_rate="Joint rate",repair_formats="Repair formats")
  for(n in names(MODELS)) for(metric in c("Pearson_marginal","squared_age_residual_correlation")) {
    v <- PPC_DRAWS[[n]][[metric]]
    ppc[[paste(n,metric)]] <- data.frame(model=unname(model_labels[n]),metric=metric,
      discrepancy=v$replicated-v$observed)
  }
  ppc <- do.call(rbind,ppc)
  ppc$metric <- factor(ppc$metric,c("Pearson_marginal","squared_age_residual_correlation"),
    c("Marginal Pearson discrepancy","Squared-age residual correlation"))
  p <- ggplot(ppc,aes(discrepancy))+geom_density(fill="#009E73",color="#009E73",alpha=.25)+
    geom_vline(xintercept=0,linetype=2,color="grey50")+facet_wrap(~metric+model,ncol=3,scales="free")+
    labs(x="Replicated discrepancy minus observed discrepancy",y="Posterior predictive density",
      title="Posterior predictive checks for the primary models",
      subtitle="Conditional replication of observed sessions; all retained posterior draws",
      caption="A centered distribution is reassuring for this feature. These checks are not conventional significance tests.")+figure_theme
  save_figure(p,FIGURE_NAMES[7],11,7)
  prior <- PRIOR_DRAWS_TABLE
  prior$model <- factor(prior$model,names(model_labels),model_labels)
  prior$language <- factor(prior$language,LANGUAGES,LANGUAGE_LABELS)
  observed <- unique(prior[c("model","language","source","observed_rate_per1000")])
  p <- ggplot(prior,aes(expected_rate_per1000,color=source,fill=source))+
    geom_density(alpha=.12,linewidth=.6)+
    geom_vline(data=observed,aes(xintercept=observed_rate_per1000,color=source),linetype=2,linewidth=.6)+
    facet_grid(model~language)+scale_x_continuous(trans="log1p",breaks=c(0,10,30,100,300,1000))+
    scale_color_manual(values=role_colors,labels=SOURCE_LABELS)+
    scale_fill_manual(values=role_colors,labels=SOURCE_LABELS)+
    labs(x="Prior expected OIRs per 1,000 utterances",y="Prior density",color=NULL,fill=NULL,
      title="Prior predictive rate distributions",
      subtitle="Full primary priors include child effects and the observed design",
      caption="Dashed lines: observed aggregate rates. Full prior predictive count summaries are in diagnostics/prior_predictive_summary.csv.")+figure_theme
  save_figure(p,FIGURE_NAMES[8],11,8)
  sens <- SENSITIVITY_SUMMARIES
  keys <- c("H1_equal_language_average","source_indonesian","incidence_Russian_minus_Chintang_child_T3")
  sens <- sens[sens$estimand %in% keys,]
  labels <- c(child_rate="Primary child rate",
    child_correlated="Correlated child effects",
    child_within_between="Within-child input association*",
    child_age39="Age entry = 39",
    child_linear="Linear age only",
    joint_rate="Primary joint rate",
    joint_correlated="Correlated child effects",
    joint_age39="Age entry = 39",
    joint_linear="Linear age only",
    repair_formats="Primary format model",
    format_narrower_priors="Narrower format priors",
    format_wider_priors="Wider format priors",
    format_shared_child="Shared format child effect",
    format_age39="Age entry = 39",
    format_linear="Linear age only")
  sens$label <- unname(labels[sens$model]);sens$primary <- sens$model %in% names(MODELS)
  sens$label <- factor(sens$label,rev(unique(sens$label)))
  sens$estimand <- factor(sens$estimand,keys,c("H1: Child input association",
    "Indonesian: Child minus CDS rate","Child T3: Russian minus Chintang"))
  p <- ggplot(sens,aes(median,label,color=primary))+
    geom_vline(xintercept=0,linetype=2,color="grey55")+
    geom_errorbarh(aes(xmin=lower,xmax=upper),height=.15)+geom_point(size=2.5)+
    facet_wrap(~estimand,ncol=1,scales="free")+
    scale_color_manual(values=c("TRUE"="#0072B2","FALSE"="#555555"),guide="none")+
    labs(x="Posterior contrast per 1,000 utterances (median and 95% interval)",y=NULL,
      title="Sensitivity of selected posterior contrasts",
      caption="*Within-child decomposition changes the input estimand. All contrasts use the same physical age endpoints. Full results: tables/sensitivity_all_contrasts.csv.")+figure_theme
  save_figure(p,FIGURE_NAMES[9],11,9)
}

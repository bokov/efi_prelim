#' ---
#' title: "Rockwood index as a predictor of patient safety events and
#'         readmissions."
#' author: 'Alex Bokov ^1,✉^, Sara Espinoza ^1^, Chandana Tripathy ^1^,
#'   ..., ..., ..., and Kathleen R. Stevens ^1^'
#' css: "production.css"
#' abstract: 'We have found that the electronic frailty index (EFI), a risk
#'   score developed using the Rockwood deficit-accumulation framework, is
#'   a strong predictor of ED visits, 30-day readmissions, healthcare-associated
#'   non-procedural trauma, cardiac complications, adverse drug events, venous
#'   thromboembolism, fluid management complications, as well as patient safety
#'   events in general and severe patient safety events.'
#' documentclass: article
#' clean: false
#' bibliography: efi_paper.bib
#' csl: harvard-cite-them-right.csl
#' output:
#'   bookdown::html_document2:
#'     fig_caption: true
#'     self_contained: true
#'     number_sections: false
#'     keep_md: true
#'     clean: false
#'   bookdown::word_document2:
#'     reference_docx: efi_template.docx
#'     fig_caption: true
#'     self_contained: true
#'     number_sections: false
#' ---
#+ load_deps, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
# setup ----
.projpackages <- c('GGally','pander','dplyr','ggplot2','data.table'
                   ,'survival','broom','forcats','table1','english');
.deps <- c( '' );
.debug <- 0;
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=FALSE
                             ,local=TRUE));
# Set some formatting options for this document
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','right');
panderOptions('table.split.table',Inf);
panderOptions('p.wrap','');
panderOptions('p.copula',', and ');
panderOptions('graph.fontfamily','serif');
# theme_get() gives default theme
theme_set(theme_bw(base_family = 'serif',base_size=14));
knitr::opts_chunk$set(echo=.debug>0, warning=.debug>0, message=.debug>0);

# What format are we rendering the output?
.outfmt <- knitr::opts_knit$get('rmarkdown.pandoc.to');
if(is.null(.outfmt)){
  if(knitr::is_html_output()) .outfmt <- 'html' else {
    if(knitr::is_latex_output()) .outfmt <- 'latex' else {
      .outfmt <- 'unknown';
    }}};

.currentscript <- current_scriptname('efi_quality.R');
.figcount <- 0;

# Load files ----

# data
if(file.exists(inputdata['dat03'])){
  dat03 <- fread(.dat03new<-inputdata['dat03']) } else {
    .infilepatt <- paste0('^[0-9]{10}_[a-z0-9]{4,12}_'
                          ,gsub('^[0-9]*_|\\.[^.]*$',''
                                ,basename(inputdata['dat01'])),'_dev.tsv$');
    if(length(.dat03candidates <- list.files(patt=.infilepatt))>0){
      .dat03new <- file.info(.dat03candidates,extra_cols = F)[
        ,'mtime',drop=F] %>% arrange(desc(mtime)) %>% rownames %>% head(1);
      dat03 <- fread(.dat03new);
      message(inputdata['dat03'],' not found, loading most recent available: '
              ,.dat03new);
    } else {
      #.srcenv1 <- new.env();
      unlink('data.R.rdata',force = T);
      .loadedobjects <- load_deps2('data.R',cachedir = .workdir,debug=.debug);
      #source('data.R',local=.srcenv1);
      .dat03candidates <- list.files(patt=.infilepatt);
      .dat03new <- file.info(.dat03candidates,extra_cols = F)[
        ,'mtime',drop=F] %>% arrange(desc(mtime)) %>% rownames %>% head(1);
      dat03 <- fread(.dat03new);
      message(inputdata['dat03'],' not found, nor any other valid input files.'
              ,'Ran data.R and loaded the resulting ',.dat03new,' file.')
    }
  }

# data dictionary
.srcenv0 <- new.env();
if(!file.exists('varmap.csv')) source('dictionary.R',local=.srcenv0);
# Then load `varmap.csv`
dct0 <- import('varmap.csv');

efi_pats <- unique(subset(dat03,a_efi>0)$patient_num);
dat03 <- subset(dat03,patient_num %in% efi_pats);

# syncronize dictionary with newly-loaded data
dct0<-sync_dictionary(dat03);

#+ fits
# fits ----
fits <- list();
for(ii in v(c_safetypaper)){
  # note: the cumsum(cumsum(%s))<=1 expression below is the part that cuts off
  # each patient at their first post-index event for the respective events
  .iidata <- gsub('%s',ii,"copy(dat03)[,c('keep','xx','Frail') :=
  list(cumsum(cumsum(%s))<=1, %s, a_efi>0.19) ,by=patient_num][,xx:=%s][(keep)
                     ,c('patient_num','a_t0','a_t1','xx','Frail','a_efi'
                  ,'age_at_visit_days')]") %>%
    parse(text=.) %>% eval;
  fits[[ii]]$dispname <- dct0[dct0$colname==ii,'dispname'];
  fits[[ii]]$plot <- survfit(Surv(a_t0,a_t1,xx)~Frail,data=.iidata) %>%
    ggsurv(plot.cens=F, main=fits[[ii]]$dispname
           ,ylab='% Patients event-free'
           ,xlab='Days since randomly selected index visit'
           ,order.legend=F) +
    #scale_x_continuous(limits=c(0,1096)) +
    scale_y_continuous(labels=scales::percent_format(1)) +
    geom_ribbon(aes(ymin=low,ymax=up,fill=group),alpha=0.3,show.legend = F) +
    scale_fill_discrete(type=c('#00BFC4','#F8766D'),breaks=c('FALSE','TRUE')) +
    scale_color_discrete(type=c('#00BFC4','#F8766D'),breaks=c('FALSE','TRUE')) +
    coord_cartesian(ylim=c(.5,1),xlim=c(0,1096));
  fits[[ii]]$models$Frailty <- coxph(Surv(a_t0,a_t1,xx)~I(10*a_efi),data=.iidata);
  fits[[ii]]$models$`Patient Age` <- update(fits[[ii]]$models$Frailty,. ~age_at_visit_days);
};

#+ los
# los ----
.losdat03 <- copy(dat03)[,Frail:=a_efi>0.19][,c('age_at_visit_days','a_efi'
                                                ,'Frail','a_los')];
fits$a_los$plot <- survfit(Surv(a_los)~Frail,.losdat03) %>%
  ggsurv(plot.cens=F, main='Length of Stay'
         ,xlab='Days since inpatient admission'
         ,ylab='% Patients still in hospital'
         ,order.legend = F) +
  scale_y_continuous(labels=scales::percent_format(1)) +
  geom_ribbon(aes(ymin=low,ymax=up,fill=group),alpha=0.3,show.legend=F) +
  scale_fill_discrete(type=c('#00BFC4','#F8766D'),breaks=c('FALSE','TRUE')) +
  scale_color_discrete(type=c('#00BFC4','#F8766D'),breaks=c('FALSE','TRUE'));
fits$a_los$models$Frailty <- coxph(Surv(a_los)~I(10*a_efi),.losdat03);
fits$a_los$models$`Patient Age` <- update(fits$a_los$models$Frailty
                                          ,.~age_at_visit_days);

#+ tb2_create
# tb2_create ----
tb2 <- sapply(fits,function(xx){
  with(xx$models,cbind(tidy(Frailty,conf.int=T)
                       ,exp=tidy(Frailty,expon=T,conf.int=T)[
                         ,c('estimate','conf.low','conf.high')]))}
  ,simplify=F) %>% bind_rows(.id='outcomevar') %>%
  mutate(betahat=sprintf('%.2f (%.2f, %.2f)',estimate,conf.low,conf.high)
         ,foldchange=sprintf('%.2f (%.2f, %.2f)',exp.estimate,exp.conf.low
                             ,exp.conf.high),P=p.adjust(p.value)
         ,Outcome=submulti(outcomevar,dct0[,c('colname','dispname')])) %>%
  rename(SE=std.error,Z=statistic);

# intro ----
#'
#' # Introduction
#'
#' Frailty is the lifelong erosion of stress resistance and accumulation of
#' impairments across multiple physiological systems. Among older community
#' dwelling adults 32% have been classified as pre-frail and 24% have been
#' classified as frail [@hoover13]. Frailty predicts disability,
#' injurious falls, and mortality [@pajewski19], emergency room visits
#' and hospitalizations [@fried01a], and long-term care admissions
#' [@pajewski19; @rockwood06; @rockwood05]. The Fried phenotype
#' [@fried01a] and Rockwood deficit accumulation index
#' [@rockwood07; @mitnitski01a], are the most commonly used methods for
#' operationalizing frailty. There is reasonable convergence between these
#' two approaches [@li15; @malmstrom14] but the deficit accumulation
#' approach does not require individual questionnaires nor physical
#' assessments yet offers similar [@malmstrom14] or possibly better
#' [@kulminski08] predictive accuracy.
#'
#' `r fs('Possible points left to cover:')`
#'
#+ n2s0, results='asis'
fs(c(
  'frailty measures as an emerging best practice?'
  ,'Do we have any remarks about how quality improvement relates to implementation science?'
  ,'outcomes important for improving quality and reducing costs'
  ,'how care could be improved with better modeling of these outcomes'
  ,'information about our site'
)) %>% paste('*',.) %>% cat(sep='\n');

#'
#'
# methods ----
#' # Methods
#'
#' ## Population
#'
#' ::: {.note2self custom-style="bnote2self"}
#' Yes, I realize at the moment this is identical to the corresponding section
#' in the aging paper. The phrasing will naturally diverge from the
#' aging paper over subsequent edits. For now I left it all here
#' because these facts are relevant to this paper as well and I didn't want to
#' lose any of them.
#' :::
#'
#' A random 1% sample (N=14,844) was drawn from the deidentified patient
#' records of a large academic health center and its teaching hospital
#' partner. Visits during which patient age was less than 18 years old were
#' excluded and then patients who had fewer than three visits in the
#' remaining data were excluded. To avoid bias, for each patient an index
#' visit date was chosen and only data recorded on or after that date was
#' used in analysis. To avoid distorted results in patients with sparse
#' visit histories, those who had fewer than two visit-dates after index
#' visit assignment were removed from the sample as were patients whose EFI
#' was never higher than 0. Finally, the patients were randomly assigned to
#' a development cohort (N=2,497 patients, 52,372 visit-days) or a testing
#' cohort `r fs('(N=3,220 patients, 56,320 visit-days)')`. Sensitivity analysis
#' was done to see the effect of leaving in data from all visits by adult
#' patients and the overall direction of EFI's effect was the same but
#' `r fs('[the performance improvement relative to patient age was inflated]')`.
#' All decisions about data processing and statistical analysis were made
#' using only the development cohort and blinded to the testing cohort. For
#' publication, the same analysis scripts were run on the testing cohort
#' and used to create all results reported here (the development version of
#' each table and figure is available in the supplemental materials). The
#' baseline characteristics of the testing cohort are shown in Table
#' \@ref(tab:table1)
#'
#' ***
# . table 1 ----
dat04 <- dat03[,lapply(.SD,head,1),by=patient_num,.SDcols=v(c_patdata)[1:5]] %>%
  # the [,-1] in the following line and at the end are needed to avoid
  # duplicates of patient_num
  cbind(dat03[,lapply(.SD,any),by=patient_num,.SDcol=v(c_response)][,-1]
        ,dat03[,.(`Patient age (years)`=max(age_at_visit_days)/365.25
                  ,Frailty=tail(a_efi,1)
                  ,`Median Frailty`=median(a_efi,na.rm=T)
                  ,`Number of Visits`=.N
                  ,a_los=as.numeric(median(a_los,na.rm=T))
                  ,`Frailty Stage`=cut(tail(a_efi,1),c(0,0.1,0.19,1)
                                       ,include.lowest = T
                                       ,labels=c('Nonfrail, < 0.1'
                                                 ,'Prefrail, 0.1 - 0.19'
                                                 ,'Frail, > 0.2')))
               ,by=patient_num][,-1]);

.tb1formula <- setdiff(names(dat04),c('language_cd','Frailty Stage'
                                      ,'Median Frailty','patient_num'
                                      ,'age_at_death_days'
                                      ,'age_at_visit_days'
                                      ,setdiff(v(c_response)
                                               ,v(c_safetypaper)))) %>%
  paste0('`',.,'`',collapse='+') %>% paste('~',.,'|`Frailty Stage`') %>%
  formula;

#+ table1,results='asis'
tb1 <- table1(.tb1formula,data=dat04) %>%
  submulti(dct0[,c('colname','dispname')],'partial');
if(.outfmt == 'html'){
  print(tb1);
  cat("\n\nTable: (\\#tab:table1) Cohort demographics")
  } else {
    pander(data.frame(X='placeholder',row.names = 'Y')
           ,caption='(\\#tab:table1) Cohort demographics');
  };

#'
#' ## Electronic Frailty Index
#'
#' @clegg16a developed an electronic frailty index (EFI) for UK health systems
#' following the methodology of Mitnitsky, Rockwood, et al.
#' [@mitnitski01a; @song10; @searle08a].
#'
#' ::: {.note2self custom-style="bnote2self"}
#' Alex TODO: Points which were omitted or briefly covered in aging paper and
#' might be appropriate here (probably more diplomatically phrased though):
#' :::
#'
#+ n2s2, results='asis'
fs(c(
'EFI has been used in the UK for several years [@clegg16a], and has only recently been adapted to a US EHR system, [@pajewski19]'
,'Our version has fewer EHR-specific dependencies'
,'Ours is easier to maintain because it uses value-flags instead of manually curated threshold values'
,'Our EFI is longitudinal, compare our results to those of [@stow18a]'
,'The importance of an open software license-- not only lower costs but also greater transparency and easier for stakeholders to contribute improvements.'
),retfun=function(xx) cat(paste('*',xx),sep='\n'))

#'
# . outcomes ----
#' ## Outcomes
#'
#' The primary outcomes we predicted with EFI were ED visits, readmission
#' within 30 days of a prior discharge from an inpatient stay, non-procedural
#' hospital trauma, cardiac complications, adverse drug events, venous
#' thromboembolisms, fluid management events, CNS complications, and GI
#' complications. All outcomes except ED visits and readmissions were
#' defined as visits during which at least one ICD10 code was recorded from the
#' corresponding patient safety indicator (PSI) groups published by @southern17 .
#' Of the other 10 PSIs defined by @southern17, 2 were excluded
#' because pregnancies were outside the scope of our protocol, 1 (hospital
#' infections) we report in a separate publications
#' `r fs('(manuscript in preparation)')` and the remaining
#' 7 had prevalences too low to reliably analyze in this sample size. However,
#' all the diagnoses from @southern17 except maternal and infant
#' complications contributed to the 'any patient safety event' outcome. A subset
#' of these were aggregated into the 'severe patient safety' event defined as
#' "proximally threatening to life or to major vital organs" [@southern17].
#'
#' ::: {.note2self custom-style="bnote2self"}
#' If anybody has any context to contribute on any of the above , you are most
#' welcome to do so. Also welcome are suggestions for refining outcomes by
#' changing the combinations of raw diagnoses/vitals/labs/etc. that are
#' criteria for those outcomes.
#' :::
#'
# . stats ----
#' ## Statistical Analysis
#'
#' For each outcome of interest, we used a Cox proportional hazard model to
#' estimate the risk of the first occurrences of the outcome after the
#' patients' respective index visits using EFI as a time-varying predictor.
#' For each outcome we compared the predictive performance of EFI to that
#' patient age at visit.
#'
# results ----
.resultsfold <- subset(tb2,outcomevar %in% v(c_truefalse)) %>%
  with(paste0(round(exp(estimate),1),'-fold for ',Outcome)) %>% tolower %>%
  submulti(cbind(c('icf','snf')
                 ,c('ICF','SNF after having been admitted from home')));
#' # Results
#'
#' Table \@ref(tab:tb2) shows the results of Cox proportional hazard models for
#' each of the responses, with EFI as the predictor. For each 0.1 increase in
#' EFI, we found at least a doubling of risk: `r pander(.resultsfold)`.
#' Furthermore, the probability of delayed discharge increased and thus longer
#' stays were observed in patients with an EFI > 0.19.
#' The p-values shown have been adjusted for multiple comparisons
#' (`r as.english(nrow(tb2)) %>% as.character` outcomes reported in
#' this study) using the @holm79 method and remain significant.
#'
#' Older patients trend toward frailty and one might ask whether EFI merely
#' reflect patient age. To test this we compared the predictive
#' accuracy of the EFI to that of patient age. For each outcome, we fit an
#' additional Cox proportional hazard model using age at visit as the
#' predictor instead of EFI. We found that EFI predicts all outcomes better
#' than patient age. Models using both patient age and EFI offered no
#' significant improvement over EFI alone `r fs('[not shown?]')`.
#'
# . coxph fits ----
#'
#' *****
#'
#'


# . table 2 ----
#+ tb2

tb2[,c('Outcome','betahat','foldchange','SE','Z','P')] %>%
  rename(`β^ (95% CI)`=betahat,`fold-change (95% CI)`=foldchange ) %>%
  pander(digits=3, caption='(\\#tab:tb2) Cox-proportional hazards with EFI as a predictor');
#'
#'
#' In Table \@ref(tab:tb3) we compare the performance of EFI and age as
#' predictors for each of the `r as.english(nrow(tb2)) %>% as.character`
#' outcomes. In all cases, the EFI models has a robustly lower AIC and
#' log-likelihood than the respective patient-age models, as well as a higher
#' concordance (in all cases greater than 0.7).
#'
# . table 3, age vs efi ----
#+ tb3
bind_rows(sapply(fits,function(xx){
  do.call(bind_rows,c(sapply(xx$models,function(yy){
    glance(yy)[,c('concordance','logLik','AIC')]
  },simplify=F),.id='Predictor'))
},simplify=F),.id='Outcome') %>%
  mutate(Outcome=ifelse(Predictor=='Frailty'
                        ,submulti(Outcome,dct0[,c('colname','dispname')])
                        ,' ')) %>%
  rename(Concordance=concordance,`Log Likelihood`=logLik) %>%
  pander(caption='(\\#tab:tb3) Comparing EFI and age as predictors');
#'
#' In figures \@ref(fig:kmplots) a-k we show Kaplan-Meier plots for each of the
#' outcomes stratified by whether EFI is greater than 0.19 (Frail=TRUE) or less
#' (Frail=FALSE) [@stow18].
#' `r fs('[Let\'s talk about whether to put length of stay here or aging paper]')`
#'
#+ kmplots, fig.show="hold", out.width=if(.outfmt=='docx') "2.75in" else "50%", fig.cap=" - "
# . kmplots ----
# if(.outfmt != 'docx'){
  .subfig <- 0;
  for(jj in fits) {
    .subfig <- .subfig + 1;
    print(jj$plot + labs(caption=paste0('Figure 1 ',letters[.subfig],'. \n')) +
            theme(plot.caption=element_text(size=15)) )};
  # } else {
  #   matrix(rep_len('placeholder',length(fits)),ncol=2) %>% pander();
  #
  # };
#'
# discussion ----
#' # Discussion
#'
#' Our data shares the fundamental limitation of the EHR system from which
#' it was obtained: like all EHR systems, it only has information that
#' providers and coders put into it. Events taking place outside the health
#' system or at un-connected health systems are not visible to our analysis.
#' On the other hand, providers who rely on EHR systems at point of care
#' are also working under these limitations on. The data we used is
#' representative of this scenario, and despite the limitations EFI
#' provides accurate predictions of poor patient outcomes. Because our
#' implementation of the Rockwood index real EHR data, it is more directly
#' transferable to clinical use than implementations based on curated
#' registries. This suggests that EFI is most accurate for patients who
#' have accumulated a reasonable in-system visit-history. Further work is
#' needed to find a more precise relationship between the length of a
#' patient's visit-history and the accuracy of EFI and better distinguish
#' genuinely non-frail patients from those who get most of their care
#' outside the researchers' health system.
#'
#' Assessing frailty helps clinicians identify high risk patients and tailor
#' interventions to prevent health decline and poor outcomes. Currently
#' available frailty assessment tools used in geriatric practice have good
#' validity (for example @fried01a) but these are time intensive and often
#' difficult to implement in a busy general practice. Because EFI can be
#' calculated automatically it is more likely to be adopted by clinicians in a
#' busy practice. Also, EFI can be calculated retroactively
#' on historic records of patients who never received in-person assessments,
#' giving a clearer picture of a health system's overall performance.
#' Furthermore, our version of the EFI algorithm can be made vendor-independent
#' since it relies only on diagnoses, medications, lab codes, and vitals which
#' are data in modern EHR systems. This, together with the fact that this
#' algorithm is publicly available under an open-source license, is intended
#' to facilitate adoption and enhancement by diverse health systems.
#'
#' `r fs('Other possible points to cover:')`
#'
#+ n2s1, results='asis'
fs(c(
 'Tie-in to implementation science'
,'What agrees and what disagrees with previous work but from a more quantitative
    point of view than in the aging paper: distributions and prevalences compared
    to [@southern17], [@clegg16a], [@stow18], and [@stow18a] for trajectories, maybe.'
,'Maybe examples from other industries of how open source software
    encourages collaboration and innovation, and that healthcare field lags
    in understanding how to leverage open source.'
,'Future work'
,'Theoretical and practical implications'
)) %>% paste('*',.) %>% cat(sep='\n')
#'
# conclusions ----
#' # Conclusions
#'
#' `r fs('TBD')`
#'
#' # Acknowledgments
#'
#' This work was supported by NIH/NCATS UL1TR001120 (AFB, KRS), the Long School
#' of Medicine KL2 Award (AFB), ...
#'
# refs ----
#' # References
#'

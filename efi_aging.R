#' ---
#' title: "Rockwood index outperforms patient age as a predictor of poor patient
#'         outcomes."
#' author: 'Alex Bokov ^1,✉^, Sara Espinoza ^1^, Chandana Tripathy ^1^,
#'   ..., ..., ..., and Kathleen R. Stevens ^1^'
#' css: "production.css"
#' abstract: 'We have found that the electronic frailty index (EFI), a risk
#'   score developed using the Rockwood deficit-accumulation framework, is
#'   a strong predictor of falls, hospitalizations, hospital-acquired
#'   infections, and loss of independence without relying on any predictors
#'   including patient age. In fact, EFI outperformed patient age as a
#'   predictor. EFI''s accuracy was not negated by the fact that we used
#'   patients of all ages, not just older patients.'
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

# detect current output format
.outfmt <- knitr::opts_knit$get('rmarkdown.pandoc.to');
if(is.null(.outfmt)){
  if(knitr::is_html_output()) .outfmt <- 'html' else {
    if(knitr::is_latex_output()) .outfmt <- 'latex' else {
      .outfmt <- 'unknown';
    }}};

.currentscript <- current_scriptname('efi_aging.R');
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
fits <- yfits <- ofits <- list();
kmplotlayers <- list(
  scale_y_continuous(labels=scales::percent_format(1))
  ,geom_ribbon(aes(ymin=low,ymax=up,fill=group),alpha=0.3,show.legend = F)
  ,scale_fill_discrete(type=c('#00BFC4','#F8766D'),breaks=c('FALSE','TRUE'))
  ,scale_color_discrete(type=c('#00BFC4','#F8766D'),breaks=c('FALSE','TRUE'))
);
for(ii in v(c_agingpaper)){
  # note: the cumsum(cumsum(%s))<=1 expression below is the part that cuts off
  # each patient at their first post-index event for the respective events
  .iidata <- gsub('%s',ii,"copy(dat03)[,c('keep','xx','Frail') :=
  list(cumsum(cumsum(%s))<=1, %s, a_efi>0.19) ,by=patient_num][,xx:=%s][(keep)
                     ,c('patient_num','a_t0','a_t1','xx','Frail','a_efi'
                  ,'age_at_visit_days')]") %>%
    parse(text=.) %>% eval;
  fits[[ii]]$dispname <- dct0[dct0$colname==ii,'dispname'];
  yfits[[ii]]$dispname <- paste0(fits[[ii]]$dispname,', Younger than 45');
  ofits[[ii]]$dispname <- paste0(fits[[ii]]$dispname,', 45 and up');
  .iifit <- survfit(Surv(a_t0,a_t1,xx)~Frail,data=.iidata);
  fits[[ii]]$plot <-  .iifit %>%
    ggsurv(plot.cens=F, main=fits[[ii]]$dispname, order.legend=F
           ,ylab='% Patients event-free'
           ,xlab='Days since randomly selected index visit') + kmplotlayers +
    coord_cartesian(ylim=c(.5,1),xlim=c(0,1096));
  yfits[[ii]]$plot <-  update(.iifit,subset=age_at_visit_days<45*365.25) %>%
    ggsurv(plot.cens=F, main=yfits[[ii]]$dispname, order.legend=F
           ,ylab='% Patients event-free'
           ,xlab='Days since randomly selected index visit') + kmplotlayers +
    coord_cartesian(ylim=c(.5,1),xlim=c(0,1096));
  ofits[[ii]]$plot <-  update(.iifit,subset=age_at_visit_days>=45*365.25) %>%
    ggsurv(plot.cens=F, main=ofits[[ii]]$dispname, order.legend=F
           ,ylab='% Patients event-free'
           ,xlab='Days since randomly selected index visit') + kmplotlayers +
    coord_cartesian(ylim=c(.5,1),xlim=c(0,1096));
  #scale_x_continuous(limits=c(0,1096)) +
    # scale_y_continuous(labels=scales::percent_format(1)) +
    # geom_ribbon(aes(ymin=low,ymax=up,fill=group),alpha=0.3,show.legend = F) +
    # scale_fill_discrete(type=c('#00BFC4','#F8766D'),breaks=c('FALSE','TRUE')) +
    # scale_color_discrete(type=c('#00BFC4','#F8766D'),breaks=c('FALSE','TRUE')) +
    # coord_cartesian(ylim=c(.5,1),xlim=c(0,1096));
  fits[[ii]]$models$Frailty <- coxph(Surv(a_t0,a_t1,xx)~I(10*a_efi)
                                     ,model=TRUE,data=.iidata);
  fits[[ii]]$models$`Patient Age` <- update(fits[[ii]]$models$Frailty
                                            ,. ~age_at_visit_days);
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
#' Possible points still left to cover:
#'
#' -   outcomes important to clinical care of older patients
#'
#' -   how care could be improved with better modeling of these outcomes
#'
#' -   information about our site
#'
# methods ----
#' # Methods
#'
#' ## Population
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
#' cohort (N=3,220 patients, 56,320 visit-days). Sensitivity analysis was
#' done to see the effect of leaving in data from all visits by adult
#' patients and the overall direction of EFI's effect was the same but
#' [the performance improvement relative to patient age was inflated].
#' All decisions about data processing and statistical analysis were made
#' using only the development cohort and blinded to the testing cohort. For
#' publication, the same analysis scripts were run on the testing cohort
#' and used to create all results reported here (the development version of
#' each table and figure is available in the supplemental materials). The
#' baseline characteristics of the testing cohort are shown in Table
#' \@ref(tab:table1).
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
                                               ,v(c_agingpaper)))) %>%
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
#' [@mitnitski01a; @song10; @searle08a]. Recently, @pajewski19 adapted the EFI
#' to use ICD10 and ICD9. We further built on this work by also mapping
#' laboratory tests to specific LOINC codes. For each patient visit, all
#' distinct diagnoses and abnormal lab results over the preceding two-year
#' window for that patient were aggregated into a single EFI, a numeric value
#' that can range from 0 to 1 (but in practice seldom exceeded 0.6). As
#' mentioned above, we omitted all visits prior to a randomly select index
#' visit for each patient. This did not interfere with EFI calculation because
#' those EFI values were calculated separately for every distinct patient-date
#' in our health system, and then joined to the EHR data.
#'
#' ## Outcomes
#'
#' The primary outcomes we predicted with EFI were falls (any ICD9 codes in
#' E880-E888.9, E987.x, V00.x, or ICD10 codes in W00-W19), hospital
#' admissions (EHR encounter type = 'inpatient'), hospital-acquired
#' infections (having any ICD10 code associated with hospital-acquired
#' infections per Southern et al. @southern17 ), discharge to
#' intermediate care facilities (ICF) or skilled nursing facilities (SNF)
#' (using discharge disposition codes, for patients whose admit-source for
#' that encounter was 'Home'), and all-cause mortality insofar as it can be
#' ascertained from the EHR discharge disposition or vital status (which,
#' in the absence of linkage to external death indexes are likely to
#' under-report the actual mortality and so should be interpreted with
#' caution).
#'
# . stats ----
#' ## Statistical Analysis
#'
#' For each outcome of interest, we used a Cox proportional hazard model to
#' estimate the risk of the first occurrences of the outcome after the
#' patients' respective index visits using EFI as the predictor. Unlike
#' earlier studies, we treated EFI as a time-varying numeric predictor with
#' multiple followups per patient. Since only the first occurrence was
#' being predicted, a recurring event model was not necessary. As a
#' comparison, for each outcome a second analysis was done that was
#' identical to the first except that patient age at visit (in days) was
#' used as the predictor.
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
#' one study) using the @holm79 method and in all cases are highly
#' significant. It is well established in gerontology that an enormous and
#' diverse range of poor health outcomes are correlated with an individual's
#' age, but chronological age provides no information about the progression of
#' biological age in an individual. It has long been a priority in aging
#' research to find a metric for aging that is subject to individual variation
#' and intervention. Rockford et al. have pointed out {ref} that deficit
#' accumulation has the hallmarks of such a measure.
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
#' Therefore, we compared the predictive accuracy of the EFI to that of
#' patient age. For each outcome, we fit an additional Cox proportional
#' hazard model, this time using age at visit as the predictor variable. It
#' appears, at least in this population, that not only can EFI accurately
#' predict mortality and poor patient outcomes without any knowledge of
#' patient age but it does so better than patient age.
#'
# . table 3, age vs efi ----
#+ tb3
# [v(c_agingpaper)]
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
#' In Table \@ref(tab:tb3) we compare the performance of EFI and age as
#' predictors for each of the `r as.english(nrow(tb2)) %>% as.character`
#' outcomes. In all cases, the EFI models has a robustly lower AIC and
#' log-likelihood than the respective patient-age models, as well as a higher
#' concordance (in all cases greater than 0.7). Cox models incorporating EFI
#' together with age, with or without an interaction term, did not have a
#' significantly better fit than the univariate EFI model (not shown, available
#' in supplement).
#'
#' In figures \@ref(fig:kmplots) a-f we show Kaplan-Meier plots for each of the
#' outcomes stratified by whether EFI is greater than 0.19 (Frail=TRUE) or less
#' (Frail=FALSE) [@stow18].
#'
#+ kmplots, fig.show="hold", out.width="50%", fig.cap=" - "
# . kmplots ----
.subfig <- 0;
for(jj in fits) {
  .subfig <- .subfig + 1;
  print(jj$plot + labs(caption=paste0('Figure 1 ',letters[.subfig],'. \n')) +
          theme(plot.caption=element_text(size=15)) );
}
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
#' Vital status is often out of date in EHR systems if the patient did not
#' die in the hospital or shortly after their visit, so our data may
#' under-represent the true mortality rates. Nevertheless, the relationship
#' between EFI and mortality risk observed here agrees with previous EFI
#' studies. Unlike previous studies we sampled all adults rather than just
#' older adults so we could assess the performance of EFI across the
#' lifespan and include individuals who might have early-onset frailty.
#'
#' There is no gold standard method to assess Frailty in clinical practice.
#' Currently available frailty assessment tools used in geriatric practice have
#' good validity (for example @fried01a) but these are time
#' intensive and often difficult to implement in a busy general practice.
#' Assessing frailty helps clinicians identify high risk patients and tailor
#' interventions to prevent health decline and poor outcomes. Because of the
#' simplicity of the Rockwood Frailty Index, it is more likely to be adopted by
#' clinicians in a busy practice.
#'
#' Other possible points to cover:
#'
#' -   What we learned by not restricting the sample to older adults (or
#'     move to methods/theory paper?)
#'
#' -   What agrees and what disagrees with previous work?
#' -   What is new?
#'     -   Per-visit EFI allows plotting an EFI trajectory over time
#'     -   Strictly limiting data elements to those we can count on being
#'         available in any EHR system, at any site
#' -   future work
#' -   theoretical and practical implications
#'
#'
# conclusions ----
#' # Conclusions
#'
#' TBD
#'
#' # Acknowledgments
#'
#' This work was supported by NIH/NCATS UL1TR001120 (AFB, KRS), the Long School
#' of Medicine KL2 Award (AFB), ...
#'
# refs ----
#' # References
#'

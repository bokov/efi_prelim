#' ---
#' title: "Electronic Frailty Index"
#' subtitle: "Preliminary Results, Short Version"
#' author:
#'  - Alex Bokov, Ph.D.
#' css: "production.css"
#' output:
#'   html_document:
#'     keep_md: true
#'     toc: true
#'     toc_float: true
#' ---
#'
#+ load_deps, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
# Settings ----
#
# In the below two lines are the minimum script-level settings you need.
# The `.projpackages` object has the names of the packages you need installed
# if necessary and then loaded for this scriport. The `.deps` object contains
# other scriports on which this one _directly_ depends (you don't need to worry
# about the indirect ones-- each scriport manages its own dependencies for
# packages and scriports). The recommended value to start with is the one shown
# here. You can add more if the need arises later. For more information, please
# see the [overview](overview.html) scriport.
.projpackages <- c('GGally','pander','dplyr','ggplot2','data.table'
                   ,'survival','broom','forcats','table1');
.deps <- c( 'analysis.R' );
.debug <- 0;
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=FALSE
                             ,local=TRUE));
# Set some formatting options for this document
pander::panderOptions('table.alignment.default','right');
pander::panderOptions('table.alignment.rownames','right');
pander::panderOptions('table.split.table',Inf);
pander::panderOptions('p.wrap','');
pander::panderOptions('p.copula',', and ');
knitr::opts_chunk$set(echo=.debug>0, warning=.debug>0, message=.debug>0);
.outcomenames <- c('vi_ed','vi_ip','vi_c_hospinfect','vi_c_hosptrauma'
                   ,'vi_c_cardiac','vi_c_falls','vi_readm30','vi_c_death');

.currentscript <- current_scriptname('analysis_abridged.R');
# Load files ----

# survival curves and tables ----
#' ## Survival Curves
#'
#' All the survival curves below report time (in days) elapsed from a randomly
#' selected index visit to the first occurrence of the respective outcomes. In
#' the case of 30-day readmissions the outcome is a readmission within 30 days
#' of a previous discharge (not within 30 days of the index visit). In all these
#' curves the predictor is the Electronic Frailty Index, a numeric variable from
#' 0 (non-frail) to 1 (all possible deficits present) with 0.19 considered the
#' threshold between non-frail and frail.
#'
#+ survcurves,message=FALSE,results='asis'
panderOptions('knitr.auto.asis', FALSE);
for(jj in fits[.outcomenames]) {
  message(jj$dispname);
  cat('\n###',jj$dispname,'\n\n');
  print(jj$plot + ggtitle(''));
  # .jjresult <- sapply(jj$models,function(xx){
  #   c(glance(xx)[,c('statistic.wald','p.value.wald','concordance','logLik'
  #                   ,'AIC')]
  #     #,zph=cox.zph(xx)$table['GLOBAL',c('chisq','p')]
  # )} %>% data.frame ) %>% t;
  cat('\n\n\n');
  # pander(.jjresult);
  cat("\n******\n");
  };

#'
# Table 1 ----
# #'
# #' ******
# #'
# #' ## Cohort table
# #'
# #' The obligatory 'table-1': key variables stratified by frailty status.
# #'
# #+ tb1
# panderOptions('knitr.auto.asis', TRUE);
# .tb1formula <- setdiff(names(dat04),c('language_cd','Frailty Stage'
#                                       ,'Median Frailty','patient_num'
#                                       ,'age_at_death_days'
#                                       ,'age_at_visit_days'
#                                       ,setdiff(v(c_response)
#                                                ,.outcomenames))) %>%
#   paste0('`',.,'`',collapse='+') %>% paste('~',.,'|`Frailty Stage`') %>%
#   formula;
#
# tb1 <- table1(.tb1formula,data=dat04) %>%
#   submulti(dct0[,c('colname','dispname')],'partial');
#
# tb1;

# Table 2 ----
#'
#' *****
#'
#' ## Statistical results
#'
#+ tb2
panderOptions('knitr.auto.asis', TRUE);
subset(tb2,outcomevar %in% .outcomenames)[,c('Outcome','betahat','foldchange'
                                               ,'SE','Z','P')] %>%
  rename(`β^ (95% CI)`=betahat,`fold-change (95% CI)`=foldchange ) %>%
  pander(digits=3,row.names=FALSE);

# Model performance  ----
#' ## Model Performance
#'
#+ tb3
bind_rows(sapply(fits[.outcomenames],function(xx){
  do.call(bind_rows,c(sapply(xx$models,function(yy){
    glance(yy)[,c('concordance','logLik','AIC')]
    },simplify=F),.id='Predictor'))
  },simplify=F),.id='Outcome') %>%
  mutate(Outcome=ifelse(Predictor=='Frailty'
                        ,submulti(Outcome,dct0[,c('colname','dispname')])
                        ,' ')) %>%
  rename(Concordance=concordance,`Log Likelihood`=logLik) %>% pander;

# Response vars ----
# #' *****
# #' ## Which variables are common enough to analyze?
# #'
# #' Which events are most common (by distinct patient) in this dataset?
# pander(.resps <- dat04[
#   ,lapply(.SD,any),by=patient_num
#   ,.SDcols=v(c_response)] %>% select(-patient_num) %>%
#     colSums() %>% sort() %>% rev() %>%
#     setNames(.,submulti(names(.),dct0[,c('colname','dispname')])) %>%
#     cbind(`N Patients`=.
#           ,`Fraction Patients`=(.)/length(unique(dat04$patient_num))));

# Reproducibility ----
reproducibility <- tidbits:::git_status(print=F);
if(identical(reproducibility$status,'')){
  .repinfo0 <- '[%5$s commit](https://%3$s/%4$s/tree/%5$s) of the [%4$s](https://%3$s/%4$s) repository **%1$s** branch. You can download these scripts [here](https://%3$s/%4$s/archive/%5$s.zip)';
  .repinfo1 <- 'you will generate a report that is identical to this one';
  } else {
    .repinfo0 <- '[%4$s](https://%3$s/%4$s) repository **%1$s** branch';
    .repinfo1 <- 'you should be able to generate a report that is similar to this one, but since the copy you are reading is a draft version, there may be differences due to subsequent revisions';
    }
.repinfo0 <- with(reproducibility
                  ,sprintf(.repinfo0,branch,tracking,githost,repo,hash));
#' ## Reproducibility of these results.
#'
#' This report was automatically generated using scripts and lookup tables
#' publicly shared in the `r .repinfo0`. In addition you will need the following
#' data files (at minimum either the first two of them or just the third one)
#' which we are not able to publicly share:
inputdata[1:2] %>% c(prepared_data_file) %>% cbind(file=basename(.),MD5sum=tools::md5sum(.)) %>%
  `[`(,-1) %>% pander;

#' If you run the version of the R scripts linked above on the files whose MD5
#' sums are identical to the ones shown in the above table, then `r .repinfo1`.
#' If you are already part of our grant-writing team and/or our IRB
#' determination, please contact me (Alex Bokov, bokov 'at' uthscsa 'dot' edu)
#' directly to get the data. All others please contact (Kathleen Stevens,
#' stevens 'at' uthscsa.edu).
#'
#+ echo=FALSE, results='hide'
#
# Save results ----
#
# Export trimmed-down version of current data dictionary
.dctout <- subset(dct0,dct0[[getOption('tb.retcol')]] %in% names(dat03));

.outfile <- export(.dctout,tempfile(),format='csv');
file.rename(.outfile,paste0('dictionary_'
                            ,substr(tools::md5sum(.outfile),1,5),'_'
                            ,submulti(basename(inputdata['dat01'])
                                      ,rbind(c('\\.[^.]*$','.csv')
                                             ,c('^[0-9]{11,13}_','')))));

#
# Now the results are saved and available for use by other scriports if you
# place `r sprintf("\x60'%s'\x60",.currentscript)` among the values in their
# `.deps` variables.
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
c()

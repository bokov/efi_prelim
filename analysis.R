#' ---
#' title: "Electronic Frailty Index"
#' subtitle: "A simple and powerful predictor for patient outcomes"
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
.projpackages <- c('GGally','tableone','pander','dplyr','ggplot2','data.table'
                   ,'survival','broom','forcats','table1');
.deps <- c( '' );
.debug <- 0;
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=FALSE));
# Set some formatting options for this document
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','right');
panderOptions('table.split.table',Inf);
panderOptions('p.wrap','');
panderOptions('p.copula',', and ');
knitr::opts_chunk$set(echo=.debug>0, warning=.debug>0, message=.debug>0);


.currentscript <- current_scriptname('analysis.R');
# Load files ----

# data dictionary
.srcenv0 <- new.env();
if(!file.exists('varmap.csv')) source('dictionary.R',local=.srcenv0);
# Then load `varmap.csv`
dct0 <- import('varmap.csv');

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
      .srcenv1 <- new.env();
      source('data.R',local=.srcenv1);
      .dat03candidates <- list.files(patt=.infilepatt);
      .dat03new <- file.info(.dat03candidates,extra_cols = F)[
        ,'mtime',drop=F] %>% arrange(desc(mtime)) %>% rownames %>% head(1);
      dat03 <- fread(.dat03new);
      message(inputdata['dat03'],' not found, nor any other valid input files.'
              ,'Ran data.R and loaded the resulting ',.dat03new,' file.')
    }
  }

# syncronize dictionary with newly-loaded data
dct0<-sync_dictionary(dat03);

# Fits ----
#' ## Univariate survival models
#'
#' For each of the response variables below, the survival curve represents
#' event-free survival from a randomly chosen index visit for Frail vs Non-frail
#' patients. Below the table is a comparison of frailty as a predictor versus
#' patient age as a predictor. The Wald statistic, concordance, log-likelihood,
#' and AIC are all different ways to compare the performance of these
#' predictors. The **p.value.wald** is the hypothesis test (the lower it is,
#' the greater the confidence with which we can reject the null hypothesis). The
#' concordance is the agreement between the predictions of the respective models
#' and actual outcomes-- the higher it is, the better the model. The
#' log-likelihood is the goodness-of-fit (the less negative, the better the
#' fit, all the models here have one degree of freedom). Finally the AIC is
#' Akaike's Information Criterion, another goodness-of-fit metric that adjusts
#' for the number of parameters (the smaller it is, the better the fit).
#'
fits <- list();
for(ii in v(c_mainresponse)){
  .iidata <- gsub('%s',ii,"copy(dat03)[,c('keep','xx','Frail') :=
  list(cumsum(cumsum(%s))<=1, %s, a_efi>0.2) ,by=patient_num][,xx:=%s][(keep)
                     ,c('patient_num','a_t0','a_t1','xx','Frail','a_efi'
                  ,'age_at_visit_days')]") %>%
    parse(text=.) %>% eval;
  fits[[ii]]$dispname <- dct0[dct0$colname==ii,'dispname'];
  fits[[ii]]$plot <- survfit(Surv(a_t0,a_t1,xx)~Frail,data=.iidata) %>%
    ggsurv(plot.cens=F, main=fits[[ii]]$dispname
           ,surv.col = c('#00BFC4','#F8766D')
           ,ylab='% Patients event-free'
           ,xlab='Days since randomly selected index visit') +
    #scale_x_continuous(limits=c(0,1096)) +
    scale_y_continuous(labels=scales::percent_format(1)) +
    geom_ribbon(aes(ymin=low,ymax=up,fill=group),alpha=0.3,show.legend = F) +
    scale_fill_manual(values=c('#00BFC4','#F8766D')) +
    coord_cartesian(ylim=c(.5,1),xlim=c(0,1096));
  fits[[ii]]$models$Frailty <- coxph(Surv(a_t0,a_t1,xx)~a_efi,data=.iidata);
  fits[[ii]]$models$`Patient Age` <- update(fits[[ii]]$models$Frailty,. ~age_at_visit_days);
  # getting rid of efiage because it doesn't really add anything new beyond
  # what comparing EFI to age univariate models gets us, and on the vi_c_severe
  # cox.zph() crashes the R session!
  #fits[[ii]]$models$efiage <- update(fits[[ii]]$models$Frailty
  #                                   ,.~.+age_at_visit_days);
};

#+ survcurves,message=FALSE,results='asis'
panderOptions('knitr.auto.asis', FALSE);
for(jj in fits) {
  message(jj$dispname);
  cat('\n###',jj$dispname,'\n\n');
  print(jj$plot);
  .jjresult <- sapply(jj$models,function(xx){
    c(glance(xx)[,c('statistic.wald','p.value.wald','concordance','logLik'
                    ,'AIC')]
      #,zph=cox.zph(xx)$table['GLOBAL',c('chisq','p')]
  )} %>% data.frame ) %>% t;
  cat('\n\n\n');
  pander(.jjresult);
  cat("\n******\n");
  };

#' ## Length of stay
#'
.losdat03 <- copy(dat03)[,Frail:=a_efi>0.2][,c('age_at_visit_days','a_efi'
                                               ,'Frail','a_los')];
survfit(Surv(a_los)~Frail,.losdat03) %>%
  ggsurv(plot.cens=F,surv.col=c('#00BFC4','#F8766D')
         ,xlab='Days since inpatient admission'
         ,ylab='% Patients still in hospital'
         ,main='Length of Stay') +
  geom_ribbon(aes(ymin=low,ymax=up,fill=group),alpha=0.3,show.legend=F);
.coxlosefi <- coxph(Surv(a_los)~a_efi,.losdat03);
.coxlosage <- update(.coxlosefi,.~age_at_visit_days);
#+ los_stats
panderOptions('knitr.auto.asis',TRUE);
rbind(Frailty=c(glance(.coxlosefi)[,c('statistic.wald','p.value.wald'
                                      ,'concordance','logLik','AIC')])
      ,`Patient Age`=c(glance(.coxlosage)[,c('statistic.wald','p.value.wald'
                                             ,'concordance','logLik'
                                             ,'AIC')])) %>% pander;
#'
#' ******
#'
# Table 1 ----
panderOptions('knitr.auto.asis',TRUE);
#' ## Cohort table
#'
#' The obligatory 'table-1': key variables stratified by frailty status.
#'
dat04 <- dat03[,lapply(.SD,head,1),by=patient_num,.SDcols=v(c_patdata)[1:5]] %>%
  # the [,-1] in the following line and at the end are needed to avoid
  # duplicates of patient_num
  cbind(dat03[,lapply(.SD,any),by=patient_num,.SDcol=v(c_response)][,-1]
        ,dat03[,.(Frailty=tail(a_efi,1),`Median Frailty`=median(a_efi,na.rm=T)
                  ,a_los=as.numeric(median(a_los,na.rm=T))
                  ,`Frailty Stage`=cut(tail(a_efi,1),c(0,0.1,0.2,1)
                                       ,include.lowest = T
                                       ,labels=c('Nonfrail, < 0.1'
                                                 ,'Prefrail, 0.1 - 0.2'
                                                 ,'Frail, > 0.2')))
               ,by=patient_num][,-1]);

.tb1formula <- setdiff(names(dat04),c('language_cd','Frailty Stage'
                                      ,'Median Frailty','patient_num')) %>%
  paste0('`',.,'`',collapse='+') %>% paste('~',.,'|`Frailty Stage`') %>%
  formula;

tb1 <- table1(.tb1formula,data=dat04) %>%
  submulti(dct0[,c('colname','dispname')],'partial');

tb1;

# Response vars ----
#' *****
#' ## Which variables are common enough to analyze?
#'
#' Which events are most common (by distinct patient) in this dataset?
pander(.resps <- dat04[
  ,lapply(.SD,any),by=patient_num
  ,.SDcols=v(c_response)] %>% select(-patient_num) %>%
    colSums() %>% sort() %>% rev() %>%
    setNames(.,submulti(names(.),dct0[,c('colname','dispname')])) %>%
    cbind(`N Patients`=.
          ,`Fraction Patients`=(.)/length(unique(dat04$patient_num))));

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
inputdata[1:2] %>% c(.dat03new) %>% cbind(file=basename(.),MD5sum=tools::md5sum(.)) %>%
  `[`(,-1) %>% pander;

#' If you run the version of the R scripts linked above on the files whose MD5
#' sums are identical to the ones shown in the above table, then `r .repinfo1`.
#' If you are already part of our grant-writing team and/or our IRB
#' determination, please contact me (Alex Bokov, bokov 'at' uthscsa 'dot' edu)
#' directly to get the data. All others please contact (_Kathleen, may I put
#' your email address here?_).
#'
# Save results ----
#
# Now the results are saved and available for use by other scriports if you
# place `r sprintf("\x60'%s'\x60",.currentscript)` among the values in their
# `.deps` variables.
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
#+ echo=FALSE, results='hide'
c()

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
  dat03 <- fread(inputdata['dat03']) } else {
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
#' an actual outcomes-- the higher it is, the better the model. The 
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

#'
#' 
# Table 1 ----
#' ## Cohort table 
#' 
#' The obligatory 'table-1': key variables stratified by frailty status.
#' 
panderOptions('knitr.auto.asis',TRUE);
dat04 <- dat03[,lapply(.SD,head,1),by=patient_num,.SDcols=v(c_patdata)[1:5]] %>%
  cbind(dat03[,lapply(.SD,any),by=patient_num,.SDcol=v(c_response)]
        ,dat03[,.(Frailty=tail(a_efi,1),`Median Frailty`=median(a_efi,na.rm=T)
                  ,`Frailty Stage`=cut(tail(a_efi,1),c(0,0.1,0.2,1)
                                       ,include.lowest = T
                                       ,labels=c('Nonfrail, < 0.1'
                                                 ,'Prefrail, 0.1 - 0.2'
                                                 ,'Frail, > 0.2')))
               ,by=patient_num]);

tb1 <- paste0('~',paste0('`',setdiff(names(dat04),c('language_cd'
                                                    ,'Frailty Stage'
                                                    ,'Median Frailty'
                                                    ,'patient_num'))
                         ,'`',collapse='+'),'|`Frailty Stage`') %>% formula %>%
  table1(data=dat04);
tb1;

# Response vars ----
#' *****
#' ## Which variables are common enough to analyze?
#' 
#' Which events are most common (by distinct patient) in this dataset?
pander(.resps <- dat03[
  ,lapply(.SD,any),by=patient_num
  ,.SDcols=v(c_response)] %>% select(-patient_num) %>%
    colSums() %>% sort() %>% rev() %>%
    cbind(`N Patients`=.
          ,`Fraction Patients`=(.)/length(unique(dat03$patient_num)))); 


# Save results ----
# 
# Now the results are saved and available for use by other scriports if you
# place `r sprintf("\x60'%s'\x60",.currentscript)` among the values in their 
# `.deps` variables.
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
#+ echo=FALSE, results='hide'
c()

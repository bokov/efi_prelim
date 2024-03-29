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
# Init ----
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
.deps <- c( '' );
.debug <- 0;
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=FALSE
                             ,local=TRUE));
# Settings ----
# Set some formatting options for this document
pander::panderOptions('table.alignment.default','right');
pander::panderOptions('table.alignment.rownames','right');
pander::panderOptions('table.split.table',Inf);
pander::panderOptions('p.wrap','');
pander::panderOptions('p.copula',', and ');
theme_set(theme_bw(base_family = 'serif',base_size=14) +
            theme(strip.background = element_rect(fill=NA,color=NA)
                  ,strip.text = element_text(size=15)));
knitr::opts_chunk$set(echo=.debug>0, warning=.debug>0, message=.debug>0);


.currentscript <- current_scriptname('analysis.R');
# Load files ----

# data
# This is a large file and takes a while to process so we cache it when
# possible, and specify a cached version in local.config.R (this does NOT
# mean these cached files should ever be committed to the repo, they are
# strictly local, that's why they are in local.config.R!!!). If that cached
# version actually exists, we load that and move on.
if(file.exists(inputdata['dat03'])){
  dat03 <- fread(.dat03new<-inputdata['dat03'])
} else {
  # otherwise, we check to see if any other cached versions exist and load the
  # most recent one (WARNING: this means if you have a messed up cache file
  # it's on you to delete it to avoid it being used by the script). Or, if there
  # is a specific cached file you want read, update local.config.R.
  # Here is a regexp for identifying cached files. Note hard-coded '_dev.tsv'
  .infilepatt <- paste0('^[0-9]{10}_[a-z0-9]{4,12}_'
                        ,gsub('^[0-9]*_|\\.[^.]*$',''
                              ,basename(inputdata['dat01'])),'_dev.tsv$');
  # if any cached files are found, find and read the most recently changed one
  if(length(.dat03candidates <- list.files(patt=.infilepatt))>0){
    .dat03new <- file.info(.dat03candidates,extra_cols = F)[
      ,'mtime',drop=F] %>% arrange(desc(mtime)) %>% rownames %>% head(1);
    dat03 <- fread(.dat03new);
    message(inputdata['dat03'],' not found, loading most recent available: '
            ,.dat03new);
  } else {
    # If no cached files are found at all, only then run the 'data.R' dependency
    # as you would in a standard scriport. But, again because of file sizes and
    # the fact that data.R generates at least two copies of the data, data.R
    # creates a blank data.R.rdata unless its debug > 0. Instead, we just
    # repeat the .infilepatt filename search and load the most recent.
    unlink('data.R.rdata',force = T);
    .loadedobjects <- load_deps2('data.R',cachedir = .workdir,debug=.debug);
    .dat03candidates <- list.files(patt=.infilepatt);
    .dat03new <- file.info(.dat03candidates,extra_cols = F)[
      ,'mtime',drop=F] %>% arrange(desc(mtime)) %>% rownames %>% head(1);
    dat03 <- fread(.dat03new);
    message(inputdata['dat03'],' not found, nor any other valid input files.'
            ,'Ran data.R and loaded the resulting ',.dat03new,' file.')
  }
}

# Enabling validation data is intentionally a manual process-- no
# auto-searching or auto-running. If I don't know where my validation dataset
# is, I'm not ready to do it for real! On the other hand, to test the *code*
# for validation, we create a resampled copy of dat03 as a placeholder for the
# real thing.

if(file.exists(inputdata['dat03v'])){
  dat03v <- fread(inputdata['dat03v']);
} else {
  set.seed(project_seed);
  dat03v <- copy(dat03)[sample(1:.N,.N,rep=T),] };

# consort diagram ----
.dat03consort <- list.files(patt=paste0('^',gsub('_.*tsv','',.dat03new)
                                        ,'.*consort.tsv'))[1];
consort <- import(.dat03consort);
.consortnodes <- mutate(consort
                        ,label=paste0(label,'\\n(Patients='
                                      ,format(patients,big.m=',')
                                      ,', Patient-Days='
                                      ,format(patdays,big.m=',')
                                      ,')')) %>%
  with(paste0(node,' [label = "',label,'"'
              ,ifelse(grepl('branchpoint',node),', shape="point", width=0 ','')
              ,'];'));
.consortsubg <- subset(consort,grepl('branchpoint'
                                     ,paste(node,previous)))$node %>%
  paste0('; ',collapse='');

consortgv <- c(
  'digraph { splines = "ortho"; node [shape = "box"];'
  ,.consortnodes
  ,'subgraph { rank = "same";'
  ,.consortsubg
  ,'}'
  # edges are hardcoded for now
  ,'dev -> branchpoint0 [dir="back"];
branchpoint0 -> test;
start -> adultvis -> haveefi -> haveefigt0 -> droptrailing -> droppostmortem -> postidxefivisgt1;
postidxefivisgt1 -> branchpoint0 [arrowhead=none];'
  ,'}');



# data dictionary
.srcenv0 <- new.env();
if(!file.exists('varmap.csv')) source('dictionary.R',local=.srcenv0);
# Then load `varmap.csv`
dct0 <- import('varmap.csv');


#efi_pats <- unique(subset(dat03,a_efi>0)$patient_num);
#dat03 <- subset(dat03,patient_num %in% efi_pats & !z_trailing);

# syncronize dictionary with newly-loaded data
dct0<-sync_dictionary(dat03);

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
#'
# Local functions ----

# rename variables
rname <- function(xx,dispname='dispname'
                  ,method='startsends'
                  # can also be "partial", "full", "exact", "starts", or "ends"
                  ,colname=getOption('tb.retcol','colname')
                  ,dictionary=get('dct0')
                  ,searchrep=na.omit(dictionary[,c(colname,dispname)])
                  ,...){
  submulti(xx,searchrep,method);
}

rnameshort <- rname; formals(rnameshort)$dispname <- 'dispname_short';

# tb1 tweak
table1cat00 <- function(xx,...) {
  if(identical(levels(xx),c('Yes','No'))) {
    return(parse.abbrev.render.code('Freq (Pct%)')(xx,...)['Yes'])};
  if(identical(levels(xx),'')) return(' ');
  return(render.categorical.default(xx))};

# summarizing coxph results
summsurv00 <- function(fit
                       # simultaneously set columns to choose from glance and
                       # what to rename them to. To leave a name as-is, leave
                       # out the label (e.g. AIC)
                       ,columns=c(statistic.wald='Wald Statistic'
                                  ,p.value.wald='P'
                                  ,concordance='Concordance'
                                  ,std.error.concordance='SE Concordance'
                                  ,logLik='Log Likelihood'
                                  ,'AIC'
                                  ,nevent='Events'
                                  ,n='Visits'
                                  ,subjects='Patients'
                                  ,zph.chisq='χ² (ZPH)'
                                  ,zph.df='DF (ZPH)'
                                  ,zph.p ='P (ZPH)')
                       ,env=parent.frame()
                       # number at risk
                      ,subjects=nrow(unique(select(eval(fit$call$data,env=env)
                                                   ,'patient_num')))
                      # function to post-process result. Set to return,
                      # identity, or hidden to do nothing
                      ,postprocess=identity){
  if(!is(fit,'coxph')) return(NULL);
  searchrep <- cbind(names(columns),columns);
  searchrep[,1] <- ifelse(searchrep[,1] %in% c(NA,'')
                          ,searchrep[,2],searchrep[,1]);
  out <- c(glance(fit),subjects=subjects
           ,zph=tryCatch(cox.zph(fit)$table[1,]
                         ,error=function(ee) c(chisq=NA,df=NA,p=NA)));
  out <- out[intersect(searchrep[,1],names(out))] %>%
    setNames(.,submulti(names(.),searchrep,'startsends')) %>%
    data.frame(check.names=FALSE);
  postprocess(out);
}

# more detailed analysis of univariate survival results
summsurv01 <- function(fit){
  cbind(tidy(fit,conf.int=T)
        ,exp=tidy(fit,expon=TRUE,conf.int=TRUE)[
          ,c('estimate','conf.low','conf.high')]
        ,glance(fit)[,c('n','nevent','nobs')]) %>%
    mutate(betahat=sprintf('%.2f (%.2f, %.2f)',estimate,conf.low,conf.high)
           ,foldchange=sprintf('%.2f (%.2f, %.2f)',exp.estimate,exp.conf.low
                               ,exp.conf.high)) %>%
    rename(SE=std.error,Z=statistic,`β^ (95% CI)`=betahat
           ,`fold-change (95% CI)`=foldchange,`# Events`=nevent
           ,`# Visits`=nobs)};

# reusable code for plotting survival curves in this project
plotsurv00 <- function(data,dispname=''
                       ,formula=Surv(a_t0,a_t1,xx)~Frail
                       ,xlim=c(0,1096),ylim=c(.5,1)
                       ,ylab='% Patients event-free'
                       ,xlab='Days since randomly selected index visit'
                       ,ribbonalpha=0.3,colors=c('#00BFC4','#F8766D')
                       ,colorbreaks=c('FALSE','TRUE')
                       ,scaley=scale_y_continuous(labels=scales::percent_format(1))
                       ,confint=geom_ribbon(aes(ymin=low,ymax=up,fill=group)
                                            ,alpha=ribbonalpha,show.legend = F)
                       ,sfill=scale_fill_discrete(type=colors,breaks=colorbreaks)
                       ,scolor=scale_color_discrete(type=colors,breaks=colorbreaks)
                       ,coords=coord_cartesian(ylim=ylim,xlim=xlim)
                       ,model=NA
                       ,...){
  if(!missing(model) && is(model,'coxph')){
    model <- update(model,formula);
    model <- update(model,.~.);
    sf <- survfit(model);
    sf$call$formula <- model$call$formula;
    sf <- update(sf,.~.,data=data);
  } else {
    sf <- survfit(formula=formula,data=data)};
  ggsurv(sf,plot.cens=F, main=dispname,ylab=ylab,xlab=xlab
           ,order.legend=F) + scaley + confint + sfill + scolor + coords +
    list(...)
};

# Fit all the models and prepare all the plots, whether for developmental
# data (dat03) or validation (dat03v)
f_makefits <- function(dataname='dat03'
                       ,vars=intersect(v(c_response),v(c_truefalse))){
  fits <- list();
  for(ii in vars){
    message('Fitting: ',ii);
    # note: the cumsum(cumsum(%s))<=1 expression below is the part that cuts off
    # each patient at their first post-index event for the respective events
    .iidata <- gsub('%s',ii,"copy(DATANAME)[,c('keep','xx','Frail') :=
  list(cumsum(cumsum(%s))<=1, %s, a_frailtf), by=patient_num][,xx:=%s][(keep)
                     ,c('patient_num','a_t0','a_t1','xx','Frail','a_efi'
                  ,'age_at_visit_days','a_agegrp')]");
    .iidata <- gsub('DATANAME',dataname,.iidata);
    fits[[ii]]$data <- parse(text=.iidata) %>% eval;
    #fitsval[[ii]]$data <- parse(text=gsub('dat03','dat04',.iidata)) %>% eval;
    #fitsval[[ii]]$dispname <-
    fits[[ii]]$dispname <- rname(ii);
    #coalesce(dct0[dct0$colname==ii,'dispname'],ii);
    #fits[[ii]]$data <- .iidata;
    fits[[ii]]$plot <- with(fits[[ii]],plotsurv00(data,dispname));
    #fitsval[[ii]]$plot <- with(fitsval[[ii]],plotsurv00(data,dispname));
    fits[[ii]]$multidata <- with(fits[[ii]],split(data,data$a_agegrp));
    #fitsval[[ii]]$multidata <- with(fitsval[[ii]],split(data,data$a_agegrp));
    fits[[ii]]$multiplot <-  with(fits[[ii]],lapply(multidata,plotsurv00
                                                    ,dispname));
    #fitsval[[ii]]$multiplot <-  with(fitsval[[ii]],lapply(multidata,plotsurv00
    #                                                      ,dispname));
    fits[[ii]]$models$Frailty <- coxph(Surv(a_t0,a_t1,xx)~I(10*a_efi)
                                       ,data=fits[[ii]]$data
                                       ,x=TRUE,y=TRUE,model=TRUE);
    #fitsval[[ii]]$models$Frailty <- update(fits[[ii]]$models$Frailty
    #                                       ,data=fitsval[[ii]]$data);
    fits[[ii]]$models$Frailty$call$data <- substitute(fits[[ii]]$data,list(ii=ii));
    #fitsval[[ii]]$models$Frailty$call$data <- substitute(fitsval[[ii]]$data,list(ii=ii));
    for(jj in names(fits[[ii]]$multidata)){
      jjlabel <- paste0('Frailty, age:',jj);
      fits[[ii]]$models[[jjlabel]] <-
        update(fits[[ii]]$models$Frailty,data=fits[[ii]]$multidata[[jj]]);
      # fitsval[[ii]]$models[[jjlabel]] <-
      #   update(fits[[ii]]$models$Frailty,data=fitsval[[ii]]$multidata[[jj]]);
      fits[[ii]]$models[[jjlabel]]$call$data <-
        substitute(fits[[ii]]$multidata[[jj]],list(jj=jj));
      # fitsval[[ii]]$models[[jjlabel]]$call$data <-
      #   substitute(fitsval[[ii]]$multidata[[jj]],list(jj=jj));
    }

    fits[[ii]]$models$`Patient Age` <- update(fits[[ii]]$models$Frailty
                                              ,. ~age_at_visit_days);
    # fitsval[[ii]]$models$`Patient Age` <- update(fitsval[[ii]]$models$Frailty
    #                                           ,. ~age_at_visit_days);
    fits[[ii]]$models$`Patient Age, 65+` <-
      update(fits[[ii]]$models$`Frailty, age:65+`,. ~age_at_visit_days);
    # fitsval[[ii]]$models$`Patient Age, 65+` <-
    #   update(fitsval[[ii]]$models$`Frailty, age:65+`,. ~age_at_visit_days);
    fits[[ii]]$modelsummary <- sapply(fits[[ii]]$models,function(xx){
      summsurv00(xx,subjects=length(unique(eval(xx$call$data)$patient_num)))}) %>%
      apply(2,unlist) %>% t %>% data.frame(check.names=FALSE);
    # fitsval[[ii]]$modelsummary <- sapply(fitsval[[ii]]$models,summsurv00) %>%
    #   apply(2,unlist) %>% t %>% data.frame(check.names=FALSE);
    fits[[ii]]$modelsummary[,'P adjusted'] <- p.adjust(fits[[ii]]$modelsummary[,'P']);
    # fitsval[[ii]]$modelsummary[,'P adjusted'] <- p.adjust(fitsval[[ii]]$modelsummary[,'P']);
  }
  fits$a_los$data <- copy(get(dataname))[,c('age_at_visit_days','a_efi'
                                             ,'a_los','patient_num'
                                             ,'a_agegrp','a_frailtf')][
                                       ,Frail:=a_frailtf][!is.na(a_los)][
                                         ,.SD[1],by='patient_num'];
  fits$a_los$dispname <- 'Length of Stay';
  fits$a_los$plot <- with(fits$a_los
                          ,plotsurv00(data,dispname,Surv(a_los)~Frail
                                      ,xlab = 'Days since first admission'
                                      ,ylab='% Patients still in hospital'
                                      ,coords = NULL));
  fits$a_los$multidata <- with(fits$a_los,split(data,data$a_agegrp));

  fits$a_los$multiplot <-  with(fits$a_los
                                ,lapply(multidata,plotsurv00,dispname
                                        ,Surv(a_los)~Frail
                                        ,xlab = 'Days since first admission'
                                        ,ylab='% Patients still in hospital'
                                        ,ylim=c(0,1),xlim=c(0,20)));
  fits$a_los$models$Frailty <- coxph(Surv(a_los)~I(10*a_efi),fits$a_los$data,x=TRUE,y=TRUE,model=TRUE);
  fits$a_los$models$`Patient Age` <- update(fits$a_los$models$Frailty
                                            ,.~age_at_visit_days);

  for(jj in names(fits$a_los$multidata)){
    jjlabel <- paste0('Frailty, age:',jj);
    fits$a_los$models[[jjlabel]] <-
      update(fits$a_los$models$Frailty,data=fits$a_los$multidata[[jj]]);
    fits$a_los$models[[jjlabel]]$call$data <-
      substitute(fits$a_los$multidata[[jj]],list(jj=jj));
  }
  fits$a_los$modelsummary <- sapply(fits$a_los$models,function(xx) {
    summsurv00(xx,subjects=length(unique(eval(xx$call$data)$patient_num)))}) %>%
    apply(2,unlist) %>% t %>% data.frame(check.names=FALSE);
  fits$a_los$modelsummary[,'P adjusted'] <- p.adjust(fits$a_los$modelsummary[,'P']);

  return(fits);
};


f_validate <- function(fit1,fit2,dat1,dat2,groupcol='Frail',lty=2,...){
  if(missing(fit2)) fit2 <- update(fit1,data=dat2);
  dat1$selfpred <- predict(fit1);
  dat2$selfpred <- predict(fit2);
  dat2$devpred <- predict(fit1,newdata=dat2);
  .crosscheck0 <- vcov(fit2);
  .fit2call <- paste(fit2$call,collapse=", ");
  fit2 <- update(fit2,.~.,data=dat2);
  fit2$call$data <- dat2;
  .crosscheck1 <- vcov(fit2);
  if(!identical(.crosscheck0,.crosscheck1)){
    warning(.fit2call,': model altered by processing, might not give correct results');
  };
  zph <- cox.zph(fit2);
  # is 1 when perfect match, the greater the better the discrimination
  lmdiscrim <- lm(devpred~selfpred,dat2);
  # should not significantly differ from 1
  coxgof <- update(fit2,.~.+offset(devpred));
  ggcdf <- ggplot(dat1
                  ,aes_string('selfpred',group=groupcol,col=groupcol)) +
    stat_ecdf() + stat_ecdf(aes_string('selfpred'),data=dat2,lty=lty);
  return(list(zph=zph,lmdiscrim=lmdiscrim,coxgof=coxgof,ggcdf=ggcdf));
}


# text snippets
#
# # The 'table' argument must have the following columns: 'estimate','outcome',
# # and 'Outcome'
# resultsfold00 <- function(table){
#   data.table(table)[,.(paste(paste0(round(exp(unique(range(estimate))),1)
#                                     ,collapse=' to '),'fold for',Outcome[1]))
#                     ,by=outcome][[2]] %>%
#     submulti(cbind(c('icf','snf')
#                    ,c('ICF','SNF after having been admitted from home')))};

# resultsfold00 <- . %>% with(.,{
#   paste0(round(exp(estimate),1),'-fold for ',Outcome) %>% tolower %>%
#     submulti(cbind(c('icf','snf')
#                    ,c('ICF','SNF after having been admitted from home'))) %>%
#     setNames(outcome)}) %>% unname;


# Fits ----

fits <- f_makefits('dat03');
fitsval <- f_makefits('dat03v');
# correct the data location for all the models we just fit so that they look
# in fitsval rather than fits!
for(ii in names(fitsval)){
  fitsval[[ii]]$models <- lapply(fitsval[[ii]]$models,function(xx){
    xx$call$data<-parse(text=gsub('^fits','fitsval'
                                  ,deparse(xx$call$data)))[[1]];xx});};

# #' Validate against hold-out data
valresults <- list();
for(ii in intersect(names(fits),names(fitsval))){
  valresults[[ii]]<-list();
  for(jj in intersect(names(fits[[ii]]$models),names(fitsval[[ii]]$models))){
    dat1 <- eval((fit1<-fits[[ii]]$models[[jj]])$call$data);
    dat2 <- eval((fit2<-fitsval[[ii]]$models[[jj]])$call$data);
    valresults[[ii]][[jj]] <- try(f_validate(fit1,fit2,dat1,dat2));
  }
}

# length of stay ----
#'
# fits$a_los$cdata <- copy(dat03)[,c('age_at_visit_days','a_efi','a_los','patient_num'
#                             ,'a_agegrp','a_frailtf')][
#                               ,Frail:=a_frailtf][!is.na(a_los)][
#                                 ,.SD[1],by='patient_num'];
#
# #fits$a_los$data <- .losdat03;
# #fitsval$a_los$dispname <-
# fits$a_los$dispname <- 'Length of Stay';
# fits$a_los$plot <- with(fits$a_los
#                         ,plotsurv00(data,dispname,Surv(a_los)~Frail
#                                     ,xlab = 'Days since first admission'
#                                     ,ylab='% Patients still in hospital'
#                                     ,coords = NULL));
# fits$a_los$multidata <- with(fits$a_los,split(data,data$a_agegrp));
# fits$a_los$multiplot <-  with(fits$a_los
#                               ,lapply(multidata,plotsurv00,dispname
#                                       ,Surv(a_los)~Frail
#                                       ,xlab = 'Days since first admission'
#                                       ,ylab='% Patients still in hospital'
#                                       ,ylim=c(0,1),xlim=c(0,20)));
# fits$a_los$models$Frailty <- coxph(Surv(a_los)~I(10*a_efi),fits$a_los$data);
# fits$a_los$models$`Patient Age` <- update(fits$a_los$models$Frailty
#                                           ,.~age_at_visit_days);
# for(jj in names(fits$a_los$multidata)){
#   jjlabel <- paste0('Frailty, age:',jj);
#   fits$a_los$models[[jjlabel]] <-
#     update(fits$a_los$models$Frailty,data=fits$a_los$multidata[[jj]]);
#   fits$a_los$models[[jjlabel]]$call$data <-
#     substitute(fits$a_los$multidata[[jj]],list(jj=jj));
# }
# fits$a_los$modelsummary <- sapply(fits$a_los$models,summsurv00) %>%
#   apply(2,unlist) %>% t %>% data.frame(check.names=FALSE);
# fits$a_los$modelsummary[,'P adjusted'] <- p.adjust(fits$a_los$modelsummary[,'P']);

#+ survcurvesdev,message=FALSE,results='asis',fig.height=4,fig.width=12,fig.cap='development'
# survival curves and results ----
panderOptions('knitr.auto.asis', FALSE);
for(jj in fits) {with(jj,{
  message(dispname);
  cat('\n###',dispname,'\n\n');
  print(ggmatrix(multiplot,nrow=1,ncol=3,legend=grab_legend(plot) #,title=dispname
           ,xAxisLabels = names(multidata)
           ,xlab=plot$label$x
           ,ylab=plot$label$y));
  cat('\n\n\n');
  pander(modelsummary);
  cat("\n******\n");
  })};
#+ survcurvesval,message=FALSE,results='hide',fig.height=4,fig.width=12,fig.cap='validation'
panderOptions('knitr.auto.asis', FALSE);
for(jj in fitsval) {with(jj,{
  message(dispname);
  cat('\n###',dispname,'\n\n');
  print(ggmatrix(multiplot,nrow=1,ncol=3,legend=grab_legend(plot) #,title=dispname
                 ,xAxisLabels = names(multidata)
                 ,xlab=plot$label$x
                 ,ylab=plot$label$y));
  cat('\n\n\n');
  pander(modelsummary);
  cat("\n******\n");
})};

#'
# Table 1 ----
#'
#' ******
#'
#' ## Cohort table (validation)
#'
#' The obligatory 'table-1': key variables stratified by frailty status.
#'
#+ tb1, results='asis'
panderOptions('knitr.auto.asis', TRUE);
dat04v <- dat03v[,lapply(.SD,head,1),by=patient_num,.SDcols=v(c_patdata)[1:5]] %>%
  # the [,-1] in the following line and at the end are needed to avoid
  # duplicates of patient_num
  cbind(dat03[,lapply(.SD,any),by=patient_num
              ,.SDcol=c(v(c_response),'vi_diabetes')][,-1]
        ,dat03[,.(`Patient age (years)`=max(age_at_visit_days)/365.25
                  ,Frailty=tail(a_efi,1)
                  ,`Median Frailty`=median(a_efi,na.rm=T)
                  ,`Number of Visits`=.N
                  ,BLANK0='',BLANK1=''
                  ,a_los=as.numeric(median(a_los,na.rm=T))
                  ,`Frailty Stage`=cut(tail(a_efi,1),c(0,0.1,0.2,1)
                                       ,include.lowest = T
                                       ,labels=c('Nonfrail, < 0.1'
                                                 ,'Prefrail, 0.1 - 0.2'
                                                 ,'Frail, > 0.2')))
               ,by=patient_num][,-1]);

frailPlusAge <- lapply(fits,function(xx){
  anova(xx$models$Frailty,update(xx$models$Frailty,.~.+age_at_visit_days)) %>%
    tidy %>% `[`(2,)}) %>% bind_rows(.id='Outcome');
# %>% mutate(Outcome=rnameshort(Outcome),p.value=nb(p.value,3)) %>% rename(Wald=statistic,DF=df,P=p.value) %>% pander(caption='Age comparisons')
frailPlusAgeval <- lapply(fitsval,function(xx){
  anova(xx$models$Frailty,update(xx$models$Frailty,.~.+age_at_visit_days)) %>%
    tidy %>% `[`(2,)}) %>% bind_rows(.id='Outcome');


#' ## Cohort table (development)
panderOptions('knitr.auto.asis', TRUE);
dat04 <- dat03[,lapply(.SD,head,1),by=patient_num,.SDcols=v(c_patdata)[1:5]] %>%
  # the [,-1] in the following line and at the end are needed to avoid
  # duplicates of patient_num
  cbind(dat03[,lapply(.SD,any),by=patient_num
              ,.SDcol=c(v(c_response),'vi_diabetes')][,-1]
        ,dat03[,.(`Patient age (years)`=max(age_at_visit_days)/365.25
                  ,Frailty=tail(a_efi,1)
                  ,`Median Frailty`=median(a_efi,na.rm=T)
                  ,`Number of Visits`=.N
                  ,BLANK0='',BLANK1=''
                  ,a_los=as.numeric(median(a_los,na.rm=T))
                  ,`Frailty Stage`=cut(tail(a_efi,1),c(0,0.1,0.2,1)
                                       ,include.lowest = T
                                       ,labels=c('Nonfrail, < 0.1'
                                                 ,'Prefrail, 0.1 - 0.2'
                                                 ,'Frail, > 0.2')))
               ,by=patient_num][,-1]);



# .tb1formula <- setdiff(names(dat04),c('language_cd','Frailty Stage'
#                                       ,'Median Frailty','patient_num'
#                                       ,'age_at_death_days'
#                                       ,'age_at_visit_days')) %>%
.tb1formula <- c('sex_cd','race_cd','BLANK0'
                 ,setdiff(names(dat04),c(v(c_patdata),'BLANK0','BLANK1'
                                         ,'Median Frailty','patient_num'))) %>%
  paste0('`',.,'`',collapse='+') %>% paste('~',.,'|`Frailty Stage`') %>%
  formula;

tb1 <- table1(.tb1formula,data=dat04,render.categorical=table1cat00) %>%
  rnameshort(method='partial') %>% gsub('BLANK.','<br/>',.);
tb1v <- table1(.tb1formula,data=dat04v,render.categorical=table1cat00) %>%
  rnameshort(method='partial') %>% gsub('BLANK.','<br/>',.);
#submulti(na.omit(dct0[,c('colname','dispname')]),'partial') %>% gsub('BLANK.','<br/>',.);

# Development
tb1;

# Validation
tb1v;


# Table 2 ----
#'
#' *****
#'
#' ## Statistical results
#'
#+ tb2

# tb2 <- sapply(fits[c(v(c_mainresponse),'a_los')],summsurv01,simplify=F) %>%
#   bind_rows(.id='outcomevar') %>%
#   mutate(betahat=sprintf('%.2f (%.2f, %.2f)',estimate,conf.low,conf.high)
#          ,foldchange=sprintf('%.2f (%.2f, %.2f)',exp.estimate,exp.conf.low
#                              ,exp.conf.high),P=p.adjust(p.value)
#          ,Outcome=submulti(outcomevar,dct0[,c('colname','dispname')])) %>%
#   rename(SE=std.error,Z=statistic);
tb2 <- sapply(fits,function(xx) sapply(xx$models,summsurv01,simplify=F) %>%
                bind_rows(.id='predictor'),simplify=F) %>%
  bind_rows(.id='outcome') %>% mutate(Outcome = rnameshort(outcome)
                                      ,`P, adjusted`=p.adjust(p.value));
tb2v <- sapply(fitsval,function(xx) sapply(xx$models,summsurv01,simplify=F) %>%
                bind_rows(.id='predictor'),simplify=F) %>%
  bind_rows(.id='outcome') %>% mutate(Outcome = rnameshort(outcome)
                                      ,`P, adjusted`=p.adjust(p.value));
#' ### Development
tb2[,c('predictor','Outcome','β^ (95% CI)','fold-change (95% CI)','SE','Z'
       ,'P, adjusted')] %>%
  pander(digits=3);
#' ### Validation
tb2v[,c('predictor','Outcome','β^ (95% CI)','fold-change (95% CI)','SE','Z'
       ,'P, adjusted')] %>%
  pander(digits=3);


# Table 3, Model performance  ----
#' ## Model Performance
#'
#' Table 3.
#+ tb3
tb3 <- lapply(fits,function(xx) cbind(Predictor=rownames(xx$modelsummary)
                                      ,xx$modelsummary[,c('Concordance'
                                                          ,'SE Concordance'
                                                          ,'Log Likelihood'
                                                          ,'AIC'
                                                          ,'χ² (ZPH)'
                                                          ,'DF (ZPH)'
                                                          ,'P (ZPH)')])) %>%
  bind_rows(.id='Outcome');
tb3v <- lapply(fitsval,function(xx) cbind(Predictor=rownames(xx$modelsummary)
                                          ,xx$modelsummary[,c('Concordance'
                                                              ,'SE Concordance'
                                                              ,'Log Likelihood'
                                                              ,'AIC'
                                                              ,'χ² (ZPH)'
                                                              ,'DF (ZPH)'
                                                              ,'P (ZPH)')])) %>%
  bind_rows(.id='Outcome');
#' ### Development

pander(tb3, row.names=FALSE);

#' ### Validation

pander(tb3v, row.names=FALSE);


# Response vars ----
#' *****
#' ## Which variables are common enough to analyze?
#'
#' Which events are most common (by distinct patient) in this dataset?
resps <- dat04[,lapply(.SD,any),by=patient_num,.SDcols=v(c_response)] %>%
  select(-patient_num) %>% colSums() %>% sort() %>% rev() %>%
  cbind(Variable=names(.),`N Patients`=.
        ,`Fraction Patients`=(.)/length(unique(dat04$patient_num)));
rownames(resps) <- rname(rownames(resps));
pander(resps);

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
prepared_data_file <- .dat03new;
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
c()

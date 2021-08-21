#' ---
#' title: "Read in Data"
#' author:
#' - "Alex F. Bokov^[UT Health, Department of Epidemiology and Biostatistics]"
#' date: "09/14/2018"
#' ---
#'
#+ message=F,echo=F
# init ----
debug <- 0;
# note: the `icdcoder` library might be needed in the future, and it has to be
# manually downloaded and installed from https://github.com/wtcooper/icdcoder/
# (package hasn't been updated in 5 years, but only existing way to map ICD9 to
# ICD10
.projpackages <- c('dplyr','data.table','forcats','pander','icd');
.globalpath <- c(list.files(patt='^global.R$',full=T)
                 ,list.files(path='scripts',patt='^global.R$',full=T)
                 ,list.files(rec=T,pattern='^global.R$',full=T)
                 ,list.files(path='..',patt='^global.R$',full=T)
                 ,list.files(path='..',rec=T,pattern='^global.R$',full=T))[1];
if(debug>0) source(.globalpath,chdir = TRUE, local=TRUE) else {
  .junk<-capture.output(source(.globalpath,chdir=TRUE, echo=FALSE, local=TRUE))};
#.currentscript <- parent.frame(2)$ofile;
.currentscript <- current_scriptname('data.R');
#' Saving original file-list so we don't keep exporting functions and
#' environment variables to other scripts
.origfiles <- ls();
#+ echo=FALSE,message=FALSE
# groupings
#' Outcome groups that should be merged into one indicator variable each
tf_psi <- c('c_hospinfect','c_decubulcer','c_endometab','c_thromb','c_cardiac'
              ,'c_respiratory','c_hemorrhage','c_drugadverse','c_fluidmgmt'
              ,'c_surgcomp','c_hosptrauma','c_anaesthesia','c_delirium','c_cns'
              ,'c_gastro','c_severe','c_psi');
tf_other <- c('c_falls','c_vent','c_respillness'
              ,'c_corona');
tf_merge <- c(tf_psi,tf_other);

# read data dictionaries ----
#' Reading data dictionary
#'
#' If `varmap.csv` doesn't exist, run `dictionary.R`
.srcenv <- new.env();
if(!file.exists('varmap.csv')) source('dictionary.R',local=.srcenv);
#' Then load `varmap.csv`
if(debug) message('Importing varmap.csv');
dct0 <- import('varmap.csv');
# read data ----
#' # Read data
if(debug) message('About to read');
# filtering out patients with two or fewer visits with `if(.N>2)0` and
set.seed(project_seed);
dat01 <- fread(unzip(inputdata['dat01']))[-1,];
consort <- consortstep(dat01,'start',label='1% Sample of All Patients'
                       ,previous=NA);
dat01 <- dat01[age_at_visit_days >= 18*362.25,][,if(.N>2) .SD, by=patient_num];
consort <- rbind(consort
                 ,consortstep(dat01,'adultvis'
                              ,label='At least three visits after the age of 18'
                              ,previous=tail(consort$node,1)));
# . z_ixvis ----
dat01 <- dat01[
    # creating a randomly selected index visit for each patient
    ,z_ixvis:=sample(age_at_visit_days[-c(1,.N)],1),by=patient_num][
      ,a_t1:=age_at_visit_days-z_ixvis,by=patient_num][
        ,a_t0:=shift(a_t1),by=patient_num][
          # converting `start_date` to a proper date column for subsequent join
          ,start_date := as.Date(start_date)];
# transform data ----
#' # Transform data
#'
#' ## assign random subsets
if(debug) message('About to subsample');
set.seed(project_seed);
.sample <- dat01[,.(subsample=sample(c('devel','test'),1)),by=patient_num];
dat01[.sample,on = 'patient_num',z_subsample:=subsample];

# aggregate ----
#'
#' ## Aggregate the outcomes indicators
if(debug) message('Aggregating indicators');
for(ii in tf_merge) eval(substitute(dat01[,paste0('vi_',ii) :=
                                           do.call(pmax,.SD) %>% as.logical()
                                         ,.SDcols=v(ii,dictionary=dct0)]
                                    ,env=list(ii=ii)));
#' ## Diabetes
dat01$vi_diabetes <- apply(data.frame(dat01)[,v(c_diabetes)],1,any);
#' ## Missingness
#'
#' ### Any diagnoses at all during visit?
# dat01$a_anydiagtf <- apply(dat01[,.SD,.SDcols=setdiff(c(v(c_icd10),v(c_icd9))
#                                                       ,v(c_info))],1,any);
dat01$a_anydiagtf <- c(v(c_icd10),v(c_icd9)) %>% paste0('_tf') %>%
  intersect(names(dat01)) %>% `[`(dat01,,.SD,.SDcols=.) %>% apply(1,any);

#' ### Any procedures at all during visit?
# dat01$a_anyproctf <- apply(dat01[,.SD,.SDcols=setdiff(c(v(c_icd10pcs),v(c_cpt))
#                                                       ,v(c_info))],1,any);
dat01$a_anyproctf <- c(v(c_icd10pcs),v(c_cpt)) %>% paste0('_tf') %>%
  intersect(names(dat01)) %>% `[`(dat01,,.SD,.SDcols=.) %>% apply(1,any);
#' ### Any labs at all during visit?
# dat01$a_anylabstf <- apply(dat01[,.SD,.SDcols=setdiff(v(c_loinc),v(c_info))],1
#                            ,function(xx) any(!is.na(xx)));
dat01$a_anylabstf <- v(c_loinc) %>% paste0('_mn') %>%
  intersect(names(dat01)) %>% `[`(dat01,,.SD,.SDcols=.) %>%
  apply(1,function(xx) any(!is.na(xx)));
dat01$a_anythingtf <- with(dat01,a_anydiagtf|a_anyproctf|a_anylabstf);

#' ## Comorbidity scores
#'
#' ### Get all distinct ICD10 numbers
# daticd10 <- dat01[
#   ,.(patient_num,start_date,icd10=sapply(.SD,function(xx){
#     if(is.na(xx)) return() else {
#       sapply(jsonlite::fromJSON(gsub('""','"',xx)),function(yy){
#         as.list(yy)$cc})}}) %>%unlist %>% as.character %>% unique %>%
#       gsub('ICD10:','',.))
#   ,.SDcols=v(c_icd10),by=seq_len(nrow(dat01))][icd10!=''];
#
# datcharlson <- charlson(daticd10,return_df=TRUE);

# remove unused ----
#' ## Remove unused columns
#'
#' Blow away the info-only columns, labs, and PSI components except those named
#' in c_override_keep
if(debug) message('Removing columns');
dat01[,setdiff(c(v(c_info),v(c_loinc),v(c_psi)),v(c_override_keep)) := NULL];

#' ## Rename columns
#'
#' Rename columns to more easily recognizable names along with dct0 entries
names(dat01) <- dct0[,c('colname','rename')] %>% na.omit %>%
  submulti(names(dat01),.,method='exact');
#' Update the dictionary to match renamed columns
if(debug) message('Syncing dictionary');
dct0 <- sync_dictionary(dat01);

#' ## Recode or derive variables
#'
#' Recode the death-related columns
dat01$v_vitalstatnaaccr <- grepl('NAACCR|1760:0',dat01$v_vitalstatnaaccr);
dat01$vi_dischdsp_death <- grepl('DischDisp:E',dat01$v_dischdsp);
dat01$vi_dischst_death <- grepl('DischStat:EX',dat01$v_dischst);
dat01[,vi_c_death := do.call(pmax,.SD) %>% as.logical()
                          ,.SDcols=v(c_death)];
#' Recode visit-related columns
dat01$vi_icu <- grepl('VISITDETAIL\\|SPEC:80',dat01$v_department);
#' `vi_emergdept` is emergency department as per provider specialty
#' for now not using this, using the one below
dat01$vi_emergdept <- grepl('VISITDETAIL\\|SPEC:45',dat01$v_department);
dat01$vi_ip <- grepl('ENC\\|TYPE:IP',dat01$v_enctype);
#' `vi_ed` is emergency department as per encounter type.
dat01$vi_ed <- grepl('ENC\\|TYPE:ED',dat01$v_enctype);

#' Create hospital stay variables
#'
#' `z_ipv` : Which inpatient stay is it for this patient?
dat01[,z_ipv := cumsum(vi_ip),by=patient_num];
#' `z_inip` : Does this row of data represent a day that's part of
#'            an inpatient stay?
dat01[,z_inip := any(vi_ip) &
        seq_len(.N) <= rle(vi_ip|is.na(v_enctype) &
                             diff(c(NA,age_at_visit_days))==1)$length[1]
      ,by=list(patient_num,z_ipv)];
#' `a_los` : Length of stay
dat01[,a_los := -1][,a_los := ifelse(vi_ip,sum(as.numeric(z_inip)),NA)
      ,by=list(patient_num,z_ipv)];
#' `z_age_at_disch_days` : age at discharge (needed for readmission calc)
dat01[,z_age_at_disch_days := -1 ][
  ,z_age_at_disch_days := age_at_visit_days[1]+a_los[1]-1
  , by=list(patient_num,z_ipv)];
#' `a_t_since_disch` : at admission, days since previous discharge
dat01[,a_t_since_disch := -1][
  ,a_t_since_disch := ifelse(vi_ip & z_ipv > 1
                             ,age_at_visit_days - shift(z_age_at_disch_days
                                                        ,fill=-1E6),NA)
  ,by=patient_num];
#' `vi_readm30` : 30-day readmission
dat01$vi_readm30 <- !is.na(dat01$a_t_since_disch) & dat01$a_t_since_disch <=30;

#' Simplify `race_cd`
dat01$race_cd <- forcats::fct_collapse(dat01$race_cd,White='white',Black='black'
                                       ,Other=c('other','pac islander'
                                                ,'unknown/othe','more than on')
                                       ,Unknown=c('@','unknown','i choose not')
                                       ,Asian='asian') %>% forcats::fct_infreq();
#' Relabel `sex_cd`
dat01$sex_cd <- factor(dat01$sex_cd,levels = c('f','m','u')
                       ,labels=c('Female','Male','Unknown'));

#' Discharge to intermediate care or skilled nursin (for patients originally
#' admitted from home)
dat01$vi_snf <- grepl('DischStat:(SN|ICF)',dat01$v_dischst) &
  grepl('ADMIT|SOURCE:HO',dat01$v_admitsrc);

# debug/QC ----
#' ### QC
#' Number of visits seeming to occur prior to date of birth
.debug_birth <- subset(dat01,age_at_visit_days < 0);
nrow(.debug_birth);
#' Number of patients with such visits
length(unique(.debug_birth$patient_num)) %>%
  c(number=.,fraction=(.)/length(unique(dat01$patient_num)));
#' Distribution of number of visits per patient
if(nrow(.debug_birth)) .debug_birth[,.N,by=patient_num]$N %>% table %>%
  as.data.frame %>% setNames(c('number suspect visits','patients'));

#' Check the death dates. This is how many visits there are that seem to occur
#' after the patients' date of death:
.debug_death00 <- subset(dat01,age_at_visit_days>age_at_death_days);
nrow(.debug_death00);
#' Number of patients with such visits
length(unique(.debug_death00$patient_num)) %>%
  c(number=.,fraction=(.)/length(unique(dat01$patient_num)));
#' Distribution of number of visits per patient
.debug_death00[,.N,by=patient_num]$N %>% table %>% as.data.frame %>%
  setNames(c('number suspect visits','patients'));
#' Does discharge status ever disagree with discharge disposition for death?
with(dat01,table(vi_dischst_death,vi_dischdsp_death));
#' Does NAACCR vital status ever disagree with discharge disposition for death?
with(dat01,table(v_vitalstatnaaccr,vi_dischdsp_death));
#' How many patients have death records from additional sources?
#' TODO: Update c_death and use that
.debug_death01 <- subset(dat01,vi_dischdsp_death|vi_dischst_death|
                           v_vitalstatnaaccr|
                           age_at_visit_days>=age_at_death_days)[
  ,d_death:=age_at_visit_days >= age_at_death_days];
.debug_death01$patient_num %>% unique %>% length;
#' How many of those patients lack an `age_at_death_days`?
subset(.debug_death01,is.na(age_at_death_days))$patient_num %>%
  unique %>% length;
#' Deceased patients
.debug_decpt <- subset(dat01,!is.na(age_at_death_days))$patient_num %>% unique;
length(.debug_decpt);
#' Deceased patients with visits before DOD
.debug_predodpt <- subset(dat01,age_at_visit_days<=age_at_death_days
                           )$patient_num %>% unique;
length(.debug_predodpt);
#' Deceased patients with visits after DOD
.debug_postdodpt <- subset(dat01,age_at_visit_days>age_at_death_days
                           )$patient_num %>% unique;
length(.debug_postdodpt);
#' Patients with visits ONLY after DOD
.debug_postdodonlypt <- setdiff(.debug_decpt,.debug_predodpt);
length(.debug_postdodonlypt);

#' Anyway, long story short we know that date of death probably not ready to
#' be a first-priority response variable, but can still be a censoring variable
#'
#' FOR NOW: If we get to competing risks before resolving death dates, just
#' censor on earliest death date suggested by the minimum of
#' NAACCR, discharge status/disposition, and nominal date of death.
# dat01[,c('z_deathmin','z_deathmax') := list(
#   pmin(age_at_death_days,age_at_visit_days[match(1,do.call(pmax,.SD))]
#        ,na.rm = TRUE)-z_ixvis
#   ,pmax(age_at_death_days,age_at_visit_days[match(1,do.call(pmax,.SD))]
#         ,na.rm = TRUE)-z_ixvis)
#   ,by=patient_num,.SDcols=v(c_death)];
#'


#' Load the eFI values
# EFI values ----
dat02 <- fread(inputdata['dat02'])[,START_DATE := as.Date(START_DATE)] %>%
  setNames(.,tolower(names(.)));
#' Join the eFI values to patient-visits (eFIs go into a new column named
#' `a_efi`)
dat01[dat02,on = c('patient_num','start_date'),a_efi := i.nval_num];
#' Delete the rows prior to each patient's `z_ixvis` randomly chosen index visit
# . pre-z_ixvis data dropped here ----
dat01 <- dat01[a_t0>=0 & !is.na(a_efi),][,if(.N>1) .SD,by=patient_num];
consort <- rbind(consort,consortstep(dat01,'postidxefivisgt1'
                                     ,label='At least two visits with non-missing eFI after randomly selected index visit'
                                     ,previous=tail(consort$node,1)));
#' Mark all the trailing 0's ... these might be patients who have not even been
#' seen for their last few visits
# dat01[,z_trailing_old:=seq_len(.N) > Position(function(xx) xx>0
#                                               ,a_efi,right=T,nomatch=0)
#       ,by=patient_num];
#
# . trailing visits ----
dat01[,z_trailing:=seq_len(.N) > max(Position(function(xx) xx>0
                                              ,a_efi,right=T,nomatch=0)
                                     ,Position(function(xx) xx,a_anythingtf
                                               ,right=T,nomatch=0))
      ,by=patient_num];
efi_pats <- unique(subset(dat01,a_efi>0)$patient_num);
dat01 <- subset(dat01,patient_num %in% efi_pats & !z_trailing);
consort <- rbind(consort,consortstep(dat01,'haveefi'
                                     ,label='At least one visit where eFI was greater than 0'
                                     ,previous=tail(consort$node,1)));
# WHERE z_ixvis SHOULD BE SET AND THEN FILTERED UPON ----

# make binned variables ----
#' Maked binned versions of certain variables
#' age
dat01$a_agegrp <- cut(dat01$age_at_visit_days,365.25*c(-Inf,45,65,Inf)
                      ,labels=c(' 18-45 ',' 45-65 ',' 65+ '));
#' frailty
dat01$a_frailtf <- dat01$a_efi>0.19;

# subsamples ----

# document the branch-point
consort <- rbind(consort
                 ,data.frame(node='branchpoint0'
                             ,patients=NA,patdays=NA,earliest=NA,latest=NA
                             ,label='',previous=tail(consort$node,1)));
dat01devel <- dat01[z_subsample=='devel',];
consort <- rbind(consort,consortstep(dat01devel,'dev',label='Development subset'
                                     ,previous='branchpoint0'));
dat01test <- dat01[z_subsample=='test',];
consort <- rbind(consort,consortstep(dat01test,'test',label='Validation subset'
                                     ,previous='branchpoint0'));

#' # Diagnostic Summary
#'
#' ## Final encounter and patient counts in each dataset
sapply(ls(patt='dat01'),function(xx) {
  yy<-get(xx); c(encounters=nrow(yy)
                 ,patients=length(unique(yy$patient_num)))}) %>% pander;
#' ## Undocumented variables
setdiff(names(dat01),dct0$colname) %>% select_at(dat01,.) %>%
  sapply(class) %>% cbind %>% pander(col.names='class');

# save out ----
#' ## Save all the processed data to tsv files
#'
.outfile <- export(dat01,tempfile(),format='tsv');
file.rename(.outfile,paste0(format(Sys.time(),'%y%m%d%H%M'),'_'
                           ,substr(tools::md5sum(.outfile),1,5),'_'
                           ,submulti(basename(inputdata['dat01'])
                                     ,rbind(c('\\.[^.]*$','.tsv')
                                            ,c('^[0-9]{11,13}_','')))));
.outfile <- export(dat01devel,tempfile(),format='tsv');
file.rename(.outfile,paste0(format(Sys.time(),'%y%m%d%H%M'),'_'
                            ,substr(tools::md5sum(.outfile),1,5),'_'
                            ,submulti(basename(inputdata['dat01'])
                                      ,rbind(c('\\.[^.]*$','_dev.tsv')
                                             ,c('^[0-9]{11,13}_','')))));
.outfile <- export(dat01test,tempfile(),format='tsv');
file.rename(.outfile,paste0(format(Sys.time(),'%y%m%d%H%M'),'_'
                            ,substr(tools::md5sum(.outfile),1,5),'_'
                            ,submulti(basename(inputdata['dat01'])
                                      ,rbind(c('\\.[^.]*$','_test.tsv')
                                             ,c('^[0-9]{11,13}_','')))));
.outfile <- export(consort,tempfile(),format='tsv');
file.rename(.outfile,paste0(format(Sys.time(),'%y%m%d%H%M'),'_'
                            ,substr(tools::md5sum(.outfile),1,5)
                            ,'_consort.tsv'));

.savelist <- if(debug>0) setdiff(ls(),.origfiles) else c('consort');
suppressWarnings(save(file=file.path(.workdir
                                     ,paste0(basename(.currentscript)
                                             ,'.rdata'))
                      ,list=.savelist));

#+ echo=F,eval=F
c()

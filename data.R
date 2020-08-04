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
.projpackages <- c('dplyr','data.table','forcats');
.globalpath <- c(list.files(patt='^global.R$',full=T)
                 ,list.files(path='scripts',patt='^global.R$',full=T)
                 ,list.files(rec=T,pattern='^global.R$',full=T)
                 ,list.files(path='..',patt='^global.R$',full=T)
                 ,list.files(path='..',rec=T,pattern='^global.R$',full=T))[1];
if(debug>0) source(.globalpath,chdir = TRUE) else {
  .junk<-capture.output(source(.globalpath,chdir=TRUE, echo=FALSE))};
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
dct0 <- import('varmap.csv');
# read data ----
#' ### read data
message('About to read');
# filtering out patients with two or fewer visits with `if(.N>2)0` and
set.seed(project_seed);
dat01 <- fread(unzip(inputdata['dat01']))[-1,][age_at_visit_days >= 18*362.25,][
  ,if(.N>2) .SD, by=patient_num][
    # creating a randomly selected index visit for each patient
    ,z_ixvis:=sample(age_at_visit_days[-c(1,.N)],1),by=patient_num][
      ,a_t1:=age_at_visit_days-z_ixvis,by=patient_num][
        ,a_t0:=shift(a_t1),by=patient_num][
          # converting `start_date` to a proper date column for subsequent join
          ,start_date := as.Date(start_date)];
# transform data ----
#' assign random subsets
set.seed(project_seed);
.sample <- dat01[,.(subsample=sample(c('devel','test'),1)),by=patient_num];
dat01[.sample,on = 'patient_num',z_subsample:=subsample];

#' Aggregate the outcomes indicators
for(ii in tf_merge) eval(substitute(dat01[,paste0('vi_',ii) :=
                                           do.call(pmax,.SD) %>% as.logical()
                                         ,.SDcols=v(ii)],env=list(ii=ii)));
#' Blow away the info-only columns, labs, and PSI components
dat01[,unique(c(v(c_info),v(c_loinc),v(c_psi))) := NULL];

#' Rename columns to more easily recognizable names along with dct0 entries
names(dat01) <- dct0[,c('colname','rename')] %>% na.omit %>%
  submulti(names(dat01),.,method='exact');
#' Update the dictionary to match renamed columns
dct0 <- sync_dictionary(dat01);

#' Recode the death-related columns
dat01$v_vitalstatnaaccr <- grepl('NAACCR|1760:0',dat01$v_vitalstatnaaccr);
dat01$v_dischdsp <- grepl('DischDisp:E',dat01$v_dischdsp);
dat01$v_dischst <- grepl('DischStat:EX',dat01$v_dischst);
#' Recode visit-related columns
dat01$vi_icu <- grepl('VISITDETAIL\\|SPEC:80',dat01$v_department);
#' `vi_emergdept` is emergency department as per provider specialty
#' for now not using this, using the one below
dat01$vi_emergdept <- grepl('VISITDETAIL\\|SPEC:45',dat01$v_department);
dat01$vi_ip <- grepl('ENC\\|TYPE:IP',dat01$v_enctype);
#' `vi_ed` is emergency department as per encounter type.
dat01$vi_ed <- grepl('ENC\\|TYPE:ED',dat01$v_enctype);

#' Simplify `race_cd`
dat01$race_cd <- forcats::fct_collapse(dat01$race_cd,White='white',Black='black'
                                       ,Other=c('other','pac islander'
                                                ,'unknown/othe','more than on')
                                       ,Unknown=c('@','unknown','i choose not')
                                       ,Asian='asian') %>% forcats::fct_infreq();

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
with(dat01,table(v_dischst,v_dischdsp));
#' Does NAACCR vital status ever disagree with discharge disposition for death?
with(dat01,table(v_vitalstatnaaccr,v_dischdsp));
#' How many patients have death records from additional sources?
.debug_death01 <- subset(dat01,v_dischdsp|v_dischst|v_vitalstatnaaccr|
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
dat02 <- fread(inputdata['dat02'])[,START_DATE := as.Date(START_DATE)] %>%
  setNames(.,tolower(names(.)));
#' Join the eFI values to patient-visits (eFIs go into a new column named
#' `a_efi`)
dat01[dat02,on = c('patient_num','start_date'),a_efi := i.nval_num];

#' Delete the rows prior to each patient's `z_ixvis` randomly chosen index visit
dat01 <- dat01[a_t0>=0 & !is.na(a_efi),][,if(.N>1) .SD,by=patient_num];

dat01devel <- dat01[z_subsample=='devel',];
dat01test <- dat01[z_subsample=='test',];

#' ## Final encounter and patient counts in each dataset
sapply(ls(patt='dat01'),function(xx) {
  yy<-get(xx); c(encounters=nrow(yy),patients=length(unique(yy$patient_num)))});

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
#+ echo=F,eval=F
c()

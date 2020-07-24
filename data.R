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
.projpackages <- c('dplyr','data.table');
.globalpath <- c(list.files(patt='^global.R$',full=T)
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
if(!file.exists('varmap.csv')) source('dictionary.R');
#' Then load `varmap.csv`
dct0 <- import('varmap.csv');
# read data ----
message('About to read');
dat01 <- fread(unzip(inputdata['dat01']))[-1,][
  ,start_date := as.Date(start_date)];
# transform data ----
#' Aggregate the outcomes indicators
for(ii in tf_merge) eval(substitute(dat01[,paste0('vi_',ii) := 
                                           do.call(pmax,.SD) %>% as.logical()
                                         ,.SDcols=v(ii)],env=list(ii=ii)));
#' Blow away the info-only columns, labs, and PSI components
dat01[,unique(c(v(c_info),v(c_loinc),v(c_psi))) := NULL];

#' Rename columns to more easily recognizable names along with dct0 entries
names(dat01) <- dct0[,c('colname','rename')] %>% na.omit %>% 
  submulti(names(dat01),.,method='exact');

#' Load the eFI values
dat02 <- fread(inputdata['dat02'])[,START_DATE := as.Date(START_DATE)] %>%
  setNames(.,tolower(names(.)));
#' Join the eFI values to patient-visits (eFIs go into a new column named 
#' `a_efi`)
dat01[dat02,on = c('patient_num','start_date'),a_efi := i.nval_num];

# save out ----
#' ## Save all the processed data to a tsv file 
#' 
.outfile <- export(dat01,tempfile(),format='tsv');
file.rename(.outfile,paste0(format(Sys.time(),'%Y%m%d%H%M'),'_'
                           ,substr(tools::md5sum(.outfile),1,7),'_'
                           ,gsub('\\.[^.]*$','.tsv'
                                 ,basename(inputdata['dat01']))));
#+ echo=F,eval=F
c()

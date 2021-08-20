# Functions for the efi_prelim project

fs <- function(str,text=str,url=paste0('#',gsub('[^_a-z0-9]','-',tolower(str)))
               ,tooltip=alist(str),class='note2self'
               # %1 = text, %2 = url, %3 = class, %4 = tooltip
               # TODO: %5 = which position 'str' occupies in fs_reg if
               #       applicable and if not found, append 'str'
               #,template='[%1$s]: %2$s "%4$s"\n'
               ,template='[%1$s]{ .%3$s custom-style="%3$s" }'
               # Turns out that the below template will generate links, but they
               # only render properly for HTML output because pandoc doesn't
               # interpret them. However, if we use the markdown implicit link
               # format (https://pandoc.org/MANUAL.html#reference-links) we
               # don't have to wrap links in anything, but we _can_ use fs()
               # with the new template default above to generate a block of
               # link info all at once in the end. No longer a point in using
               # the fs_reg feature for this case, the missing links will be
               # easy to spot in the output hopefully
               #,template="<a href='%2$s' class='%3$s' title='%4$s'>%1$s</a>"
               ,dct=NA,col_tooltip='colname_long',col_class='',col_url=''
               ,col_text='',match_col=c('varname','colname'),fs_reg=NULL
               ,retfun=return # could also be cat
               #,fs_reg='fs_reg'
               ,...){
  # if a data dictionary is specified use that instead of the default values
  # for arguments where the user has not explicitly provided values (if there
  # is no data dictionary or if the data dictionary doesn't have those columns,
  # fall back on the default values)
  if(is.data.frame(dct) &&
     length(match_col<-intersect(match_col,names(dct)))>0){
    dctinfo <- dct[match(str,do.call(coalesce,dct[,match_col])),];
    #!all(is.na(dctinfo <- dct[which(dct[[match_col]]==str)[1],]))){
    if(missing(tooltip) #&&
       #length(dct_tooltip<-na.omit(dctinfo[[col_tooltip]]))==1) {
       #tooltip <- dct_tooltip;}
    ){tooltip <- do.call(coalesce,c(dctinfo[,col_tooltip],tooltip,''))};
    if(missing(text) &&
       length(dct_text<-na.omit(c(dctinfo[[col_text]],NA)))==1) {
      text <- dct_text;}
    if(missing(url) &&
       length(dct_url<-na.omit(c(dctinfo[[col_url]],NA)))==1) {
      url <- dct_url;}
    if(missing(class) &&
       length(dct_class<-na.omit(c(dctinfo[[col_class]],NA)))==1) {
      class <- dct_class;}
  } else dctinfo <- data.frame(NA);
  out <- sprintf(rep(template,nrow(dctinfo)),text,url,class,tooltip,...);
  # register each unique str called by fs in a global option specified by
  # fs_register
  if(!is.null(fs_reg)) {
    dbg<-try(do.call(options,setNames(list(union(getOption(fs_reg),str))
                                      ,fs_reg)));
    if(is(dbg,'try-error')) browser();
  }
  retfun(out);
}


#' Usew the fs() function to create .docx-compatible notes!
#'
#' This function is intended mainly for use dynamically in `r ...` calls
#' within markdown.
#'
#' @param target  The document text to which this note is attached
#' @param comment The content of the note
#' @param author  Author
#' @param id      Unique ID automatically generated for each note
#' @param date    Date
#' @param fmt     Can be one of:
#'                         'i' (short note and short comment, both inline),
#'                         'c' (long comment, treated as block)
#'                         't' (long target, treated as a block)
#'                         'b' (both treated as blocks)
#'
#' @param ...
#'
#' @return A character vector length 1-3 that together contains the markup for
#'         margin notes.
#' @export
#'
#' @examples
#'
#' # Ordinary comment:
#' note('Document text','Comment about the document text', author='Alex')
#'
#' # Long comment:
#' .nt <- note('Document text','PLACEHOLDER, UNUSED BUT SHOULD BE UNIQUE', author='Alex',fmt='c')
#' .nt[1]
#' #' Write your comment text here
#' .nt[2]
#'
#' # Long text:
#' .nt <- note('','A normal sized comment', author='Alex',fmt='t')
#' .nt[1]
#' #' This time, write the DOCUMENT text here
#' .nt[2]
#'
#' # Long comment and text
#' .nt <- note('','PLACEHOLDER2, UNUSED BUT SHOULD BE UNIQUE', author='Alex', fmt='b')
#' .nt[1]
#' #' Write your long comment here
#' .nt[2]
#' #' Write your long document text here
#' .nt[3]
#'
note <- function(target='TARGET',comment='COMMENT',author='Bokov, Alex F'
                 ,id=abs(digest::digest2int(comment)),date=Sys.Date()
                 ,fmt=c('inline','comment','target','both'),...){
  id <- as.character(id);
  class <- sprintf('.comment-start id="%s" author="%s" date="%s"'
                   ,id,author,date);
  tpl <- c('[','%4$s',']{%3$s}','%1$s',paste0('[]{.comment-end id="',id,'"}'));
  tpl <- switch(match.arg(fmt)
                ,inline = paste0(tpl,collapse='')
                ,comment = c(tpl[1],paste0(tpl[c(3:5)],collapse=''))
                ,target = c(paste0(tpl[1:3],collapse=''),tpl[5])
                ,both = tpl[c(1,3,5)]);
  fs(target,class=class,tooltip=comment,template=tpl,...);
}

#' Wrapper for note, specifically for highlighting text that needs to be
#' reviewed after the real validation data is run.
valnote <- function(text) note(text
                             ,comment='Review after running validation data'
                             ,id=runif(1)
                             ,author='VALIDATION');

trinote <- function(target,comment) note(target,comment=comment
                                         ,id=runif(1)
                                         ,author='TRIPOD');



# # The 'table' argument must have the following columns: 'estimate','outcome',
# # and 'Outcome'
resultsfold00 <- function(table){
  data.table(table)[,.(paste(paste0(round(exp(unique(range(estimate))),1)
                                    ,collapse=' to '),'fold for',Outcome[1]))
                    ,by=outcome][[2]] %>%
    submulti(cbind(c('icf','snf')
                   ,c('ICF','SNF after having been admitted from home')))};

#' This function creates an entry for a CONSORT diagram summarizing the current
#' state of the data
consortstep <- function(dat,node='',patient_num='patient_num'
                        ,start_date='start_date',...){
  patients <- length(unique(dat[[patient_num]]))
  patdays <- nrow(unique(
    if(is(dat,'data.table')){
      dat[,.SD,.SDcols=c(patient_num,start_date)]
    } else {dat[,c(patient_num,start_date)]}));
  earliest <- min(dat[[start_date]],na.rm = TRUE);
  latest <- max(dat[[start_date]],na.rm = TRUE);
  data.frame(node=node,patients=patients,patdays=patdays
             ,earliest=earliest,latest=latest,...)};

#' Clean up numbers.
nb <- function(xx,digits=4
                      ,rxp=gsub('#',digits,'^(-*[0-9]+\\.[0-9]{0,#})[0-9]+(.*)$')){
  #message('nb input: ',xx);
  #message('nb rxp: ',rxp);
  if(!is.numeric(xx)) return(xx);
  ifelse(between(log10(abs(xx)),-4,7),prettyNum(round(xx,digits),big.mark=",")
         ,gsub(rxp,'\\1\\2'
               ,knitr:::format_sci(xx,'md')))};

#' The standard Cox PH table format that will be used for this publication
cphunivar <- function(fit,nhyp=3,digitst=3)  tidy(fit,conf.int=T) %>%
  select(-term) %>% mutate(`Hazard Ratio`=exp(estimate),P=nhyp*p.value) %>%
  sapply(nb,digits=3) %>% rbind() %>% data.frame(check.names = F) %>%
  mutate(`β (95% CI)`=paste0(estimate,' (',conf.low,', ',conf.high,')')
         ,SE=std.error,Wald=statistic,) %>%
  select('Hazard Ratio','β (95% CI)','SE','Wald','P');

risktable00 <- function(surv,times=c(0,30,60,90,365,365*3),digits=2,...)
  summary(surv,times=times,censored=T,...) %>%
  with(data.frame(strata,`t (Days)`=time,`At Risk`=n.risk,Events=n.event
                  ,Censored=n.censor,`S(t)`=surv,SE=std.err
                  ,`Lower CI`=lower,`Upper CI`=upper,check.names = F) %>%
         mutate(Frail=gsub('^.*=',''
                           ,ifelse(`t (Days)`==min(`t (Days)`)
                                   ,as.character(strata),''))) %>%
         mutate_all(nb,digits=digits) ) %>%
  select('Frail','t (Days)','At Risk','Events','Censored','S(t)','SE'
         ,'Lower CI','Upper CI');

proj_render_cont <- function(xx,...){
  with(stats.apply.rounding(stats.default(xx,...),...)
       # ,c('',`Median [IQR]`=sprintf('%s [%s - %s]',nb(as.numeric(MEDIAN))
       #                              ,nb(as.numeric(Q2))
       #                              ,nb(as.numeric(Q3))))
       ,c(`Mean (SD)`=sprintf('%s (%s)',nb(as.numeric(MEAN)),nb(as.numeric(SD))))

       # ,c(`Median [IQR]`=sprintf('%s <br/>[%s - %s]',nb(as.numeric(MEDIAN))
       #                              ,nb(as.numeric(Q2))
       #                              ,nb(as.numeric(Q3))))
  )};

proj_render_tf <- function(xx,...,digits=3,na.is.category=F){
  sapply(stats.apply.rounding(stats.default(xx, ...), ...)
         , function(yy) {with(yy,sprintf("%s (%s%%)"
                                         , nb(as.numeric(FREQ)
                                              ,digits=digits)
                                         ,if (na.is.category) PCT else {
                                           PCTnoNA
                                         }))})['Yes']
};


proj_render_catblank <- function(xx,...){
  c("", sapply(stats.default(xx, ...),function(yy,...) ''))
};

proj_render_cat <- function(xx,...,digits=3,na.is.category=F){
  c("", sapply(stats.apply.rounding(stats.default(xx, ...), ...)
               , function(yy) {with(yy,sprintf("%s (%s%%)"
                                              , nb(as.numeric(FREQ)
                                                   ,digits=digits)
                                              ,if (na.is.category) PCT else {
                                                PCTnoNA
                                                }))})) %>%
    setNames(.,paste0('&nbsp;&nbsp;',names(.)))
  };

proj_render <- function(xx,name,...){
  if(grepl('^BLANK',name)) return(c(' ',sapply(stats.default(xx,...)
                                              ,function(yy) '&nbsp;')));
  #if(name=='vi_c_psi') browser();
  if(is.logical(xx)) return(proj_render_tf(xx,...));
  if(!is.numeric(xx)) return(proj_render_cat(xx,...));
  proj_render_cont(xx,...);
}

proj_render_strat <- function (label, n, transpose = F){
  sprintf(ifelse(is.na(n), "<span class='stratlabel'>%s</span>",
                 "<span class='stratlabel'>%s<br><span class='stratn'>(N=%s)</span></span>"),
          label, nb(as.numeric(n)))
}

# expr.from.lm <- function (fit) {
#   # the terms we're interested in
#   con <- names(coef(fit))
#   # current expression (built from the inside out)
#   expr <- quote(epsilon)
#   # prepend expressions, working from the last symbol backwards
#   for (i in length(con):1) {
#     if (con[[i]] == '(Intercept)')
#       expr <- bquote(beta[.(i-1)] + .(expr))
#     else
#       expr <- bquote(beta[.(i-1)] * .(as.symbol(con[[i]])) + .(expr))
#   }
#   # add in response
#   expr <- bquote(.(terms(fit)[[2]]) == .(expr))
#   # convert to expression (for easy plotting)
#   as.expression(expr)
# }
#
plotsurv01 <- function(srv1,srv2
                       ,labs=NA
                       ,conf.int=T
                       ,xlim=c(0,365.25*3)
                       # hard to distinguish training and validation when censor marks present, so turning off
                       ,censor.size=0
                       ,break.time.by=365.25/2 # 6 months
                       #,cumevents=F, risk.table = F
                       ,xscale='d_m' # days to months
                       ,surv.scale='percent'
                       ,ylab='% Patients event-free'
                       ,xlab='Months since randomly-selected index visit'
                       ,palette=c('#00BFC4','#F8766D','#00BFC5','#F8766E')
                       ,linetype=c(1,1,2,2)
                       ,linesize=c(1,1,0.5,0.5)
                       ,alpha=c(0.2,0.2,0,0)
                       ,fontsize=theme_get()$text$size
                       ,font.family=theme_get()$text$family
                       ,dat1=eval(srv1$call$data),dat2=eval(srv2$call$data)
                       ,legend='right'
                       ,ptheme = theme(legend.key.width = unit(1,'cm'))
                       ,...){
  compare <- !missing(srv2);
  nstrata <- length(stratanames<- if(compare) {
    names(c(srv1=srv1$strata,srv2=srv2$strata))
    } else names(srv1$strata));
  if(missing(labs)){
    labs <- stratanames;
    warning('No `labs` argument specified defaulting to the following labels:'
            ,paste0(stratanames,collapse=', '))
  } else if(length(labs) != nstrata){
    length(labs) <- nstrata; labs <- coalesce(labs,stratanames);
    warning('`labs` had wrong length. Changed to:'
            ,paste0(labs,collapse=', '))};
  if(length(palette) != nstrata){
    length(palette) <- nstrata; palette <- coalesce(palette,'#888888');
    warning('`palette` had wrong length. Changed to:'
            ,paste0(palette,collapse=', '))};
  # linetype, linesize, alpha
  if(length(linetype)!=nstrata){
    length(linetype) <- nstrata; linetype <- coalesce(linetype,1)
    warning('`linetype` had wrong length. Changed to:'
            ,paste0(linetype,collapse=', '))};
  if(length(linesize)!=nstrata){
    length(linesize) <- nstrata; linesize <- coalesce(linesize,1)
    warning('`linesize` had wrong length. Changed to:'
            ,paste0(linesize,collapse=', '))};
  if(length(alpha)!=nstrata){
    length(alpha) <- nstrata; alpha <- coalesce(alpha,1)
    warning('`alpha` had wrong length. Changed to:'
            ,paste0(alpha,collapse=', '))};
  if(!compare) return(ggsurvplot(srv1,data=dat1,conf.int=T,xlim=xlim
                                 ,censor.size=censor.size
                                 ,break.time.by=break.time.by
                                 ,xscale=xscale,ylab=ylab,xlab=xlab
                                 ,palette=palette,linetype=linetype
                                 ,fontsize=fontsize,font.family=font.family
                                 ,legend=legend,legend.labs=c(labs)
                                 ,surv.scale=surv.scale,...));

  out<-ggsurvplot_combine(list(srv1,srv2),data=list(dat1,dat2)
    # plot conf-ints separately, in order to suppress training ones to make the plots less busy
    ,conf.int=F,xlim=xlim,censor.size=censor.size,break.time.by=break.time.by
    ,xscale=xscale,ylab=ylab,xlab=xlab,palette=palette,linetype=linetype
    ,size='strata',fontsize=fontsize,font.family=font.family
    ,legend=legend,legend.labs=c(labs),surv.scale=surv.scale,...
    );
  # cannot add layers directly to ggsurvplot but luckily it has a valid ggplot
  # object inside it. So we extract that object, add customized confidence
  # intervals, and put it back where we found it.
  outplot <- out$plot + scale_size_manual(values=linesize);
  if(conf.int){
    outplot <- outplot + geom_ribbon(aes(ymin=lower,ymax=upper
                                         ,fill=strata,alpha=strata)) +
      scale_alpha_manual(values=alpha);
    };
  outplot <- outplot + labs(color="",linetype="",alpha="",fill="", size="") +
    ptheme;
  out$plot <- outplot;
  out;
};


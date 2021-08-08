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

# # The 'table' argument must have the following columns: 'estimate','outcome',
# # and 'Outcome'
resultsfold00 <- function(table){
  data.table(table)[,.(paste(paste0(round(exp(unique(range(estimate))),1)
                                    ,collapse=' to '),'fold for',Outcome[1]))
                    ,by=outcome][[2]] %>%
    submulti(cbind(c('icf','snf')
                   ,c('ICF','SNF after having been admitted from home')))};

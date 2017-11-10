
#' @export
summary.jms.database.table <- function (object, ...)
{
  summ=data.frame(character(),numeric(),numeric(),numeric(),numeric(),numeric(),numeric(),numeric(),stringsAsFactors=FALSE,row.names = NULL)
  names(summ)<-c('',"Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.","NA's")
  nn=colnames(object)
  a=0
  for(i in 1:ncol(object)) {
    if(nn[[i]]=='id') next()
    a=a+1
    summ[a,1]<-nn[[i]]
    o<-object[,i]
    if(!is.numeric(o)) next()
    nas <- is.na(o)
    o <- o[!nas]
    qq <- stats::quantile(o)
    qq <- signif(c(qq[1L:3L], mean(o), qq[4L:5L]), 3)
    if (any(nas))
      qq<-c(qq, `NA's` = sum(nas))
    else
      qq<-c(qq,NA)
    summ[a,2:8]<-qq
  }
  summ=as.matrix(summ)
  dimnames(summ)[[1L]]<-rep.int("", nrow(summ))
  summ[is.na(summ)]<-''
  print(summ,quote=FALSE)
}

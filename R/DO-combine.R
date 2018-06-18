#' @export
combine.jms.data.object <- function(objects,interpolate=FALSE) {
  return(objects)
}

#' @export
combine.list <- function(objects,interpolate=FALSE,maxPoints=Inf) {
  if(!length(objects)) stop('No objects to combine')
  if(length(objects)==1) return(objects[[1]])
  #Now we have to read each object as a subsequent column for a data frame
  len_f=length(objects)
  #Build the x axis
  x_all=NA
  for(f in 1:len_f) {
    if(is.null(objects[[f]])) next()
    x_column=xcol(objects[[f]])
    x=objects[[f]][,x_column]
    if(f==1)
      x_all=x
    else
      x_all=union(x_all,x)
  }
  x_all=sort(x_all)
  x_sampled=if(length(x_all)>maxPoints) sample(x_all,maxPoints) else x_all
  #Build the y columns
  columns=c()
  yNA=rep_len(NA,length(x_all))
  for(f in 1:len_f) {
    if(is.null(objects[[f]])) next()
    x_column=objects[[f]][,xcol(objects[[f]])]
    whichY=x_all%in%x_column
    for(i in ycol(objects[[f]])) {
      y_column=yNA
      y_column[whichY]=objects[[f]][!is.na(objects[[f]][,i]),i]
      if(interpolate) {
        y_columnApprox=approxfun(x_all,y_column,yleft=NA,yright=NA)
        y_column=y_columnApprox(x_sampled)
      }
      columns[[length(columns)+1]]=y_column
    }
  }
  df=jms.data.object(x_sampled,columns)
  #Add default attributes
  xlab(df)<-xlab(objects[[1]])
  ylab(df)<-ylab(objects[[1]])
  xcol(df)<-1
  ycol(df)<-c(2:ncol(df))
  attr(df,'file_type')<-attr(objects[[1]],'file_type')
  attr(df,'data_type')<-attr(objects[[1]],'data_type')
  #Rename columns
  names(df) <- c(xlab(df),paste0(ylab(df),'_',c(1:length(columns))))
  class(df)<-class(objects[[1]])
  #return data
  return(df)
}

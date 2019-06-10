#' @export
combine.jms.data.object <- function(objects,interpolate=FALSE, rescale=FALSE) {
  return(objects)
}

#' @export
combine.list <- function(objects,interpolate=FALSE,maxPoints=Inf, rescale=TRUE) {
  if(!length(objects)) stop('No objects to combine')
  if(length(objects)==1) return(objects[[1]])
  #Now we have to read each object as a subsequent column for a data frame
  len_f=length(objects)
  #Build the x axis
  x_all=NA
  y_locs=list()
  for(f in 1:len_f) {
    if(is.null(objects[[f]])) next()
    x_column=xcol(objects[[f]])

    xs <- ifelse(rescale, xscale(objects[[f]]), 1)

    x=objects[[f]][,x_column] * xs
    if(f==1) {
      x_all = x
      y_locs[[1]] = 1:length(x)
    }
    else {
      # Ideally we would use pmatch, but it doesn't seem to like long vectors??
      # So this is a workaround trying to do the same...
      matched_x = charmatch(x, x_all)
      # x found more than once in x_all
      d <- which(matched_x == 0)
      if(any(d)) {
        diffs <- c(1, diff(d))
        is_step <- diffs != 1
        range_list <- split(d, cumsum(is_step))
        matched_ranges <- lapply(range_list, function(a) {
          all_range = which(x_all == x[[a[[1]]]])
          l <- length(a)
          matched <- rep_len(NA, l)
          l2 <- length(all_range)
          l <- ifelse(l2 < l, l2, l)
          matched[1:l] <- all_range[1:l]
        })
        for(i in 1:length(range_list)) {
          matched_x[range_list[[i]]] <- matched_ranges[[i]]
        }
      }
      ### End "pmatch"
      x_end = length(x_all)
      not_matched = is.na(matched_x)
      if(any(not_matched)) {
        x_all=c(x_all, x[not_matched])
        matched_x[not_matched] <- (x_end + 1):length(x_all)
      }
      y_locs[[f]] <- matched_x
    }
  }

  # TODO: really we want to maintain the order of each object just interleave them w.r.t. each other...
  x_ord = order(x_all)
  x_all=x_all[x_ord]

  if(length(x_all)>maxPoints) {
    if(interpolate) {
      sampled_x = approx(x_all, n = maxPoints)
    } else {
      sampled_rows = sample(1:length(x_all), maxPoints)
      sampled_x = x_all[sampled_rows]
    }
  } else {
    sampled_rows = 1:length(x_all)
    sampled_x = x_all[sampled_rows]
  }

  #Build the y columns
  columns=c()
  column_atts=list()
  yNA=rep_len(NA,length(x_all))
  for(f in 1:len_f) {
    if(is.null(objects[[f]])) next()
    column_atts = append(column_atts, (attributes(objects[[f]])))

    xs <- ifelse(rescale, xscale(objects[[f]]), 1)

    whichY = y_locs[[f]]

    for(i in ycol(objects[[f]])) {
      y_column=yNA

      ys <- ifelse(rescale, yscale(objects[[f]]), 1)

      y_column[whichY] = objects[[f]][!is.na(objects[[f]][,i]),i] * ys
      y_column = y_column[x_ord]
      if(interpolate) {
        y_columnApprox=stats::approxfun(x_all,y_column,yleft=NA,yright=NA)
        y_column=y_columnApprox(sampled_x)
      } else {
        y_column=y_column[sampled_rows]
      }
      columns[[length(columns)+1]]=y_column
    }
  }
  df=jms.data.object(sampled_x,columns)
  #Add default attributes
  xlab(df)<-xlab(objects[[1]])
  ylab(df)<-ylab(objects[[1]])
  xcol(df)<-1
  ycol(df)<-c(2:ncol(df))

  if(rescale) {
    xscale(df) <- 1
    yscale(df) <- 1
  } else {
    xscale(df)<-xscale(objects[[1]])
    yscale(df)<-yscale(objects[[1]])
  }

  attr(df,'file_type')<-attr(objects[[1]],'file_type')
  attr(df,'data_type')<-attr(objects[[1]],'data_type')
  #Rename columns
  names(df) <- c(xlab(df),paste0(ylab(df),'_',c(1:length(columns))))
  class(df)<-class(objects[[1]])

  #Add attributes
  special_attrs=c('class', 'comment', 'dim', 'dimnames', 'names', 'row.names', 'tsp',
                  'xlab', 'ylab', 'y2lab', 'x_type', 'y_type', 'y2_type', 'x_column',
                  'y_column', 'y2_column', 'file_type', 'data_type', 'x_scale', 'y_scale', 'y2_scale')

  attrs_all = names(column_atts)
  attrs <- unique(attrs_all)
  attrs <- attrs[!attrs %in% special_attrs]
  grouped_attrs <- lapply(attrs, function(x) unlist(column_atts[x == attrs_all], use.names = F))
  attributes(df)[attrs]<-grouped_attrs

  #return data
  return(df)
}

#' Arrange multiple grobs on a page
#'
#' @param ... grobs to arrange, and optional layout parameters
#' @param grobs [optional] list of grobs
#' @param as.table logical, indicates whether the list of grobs should be placed row-wise or column-wise
#' @param top annotation (text or grob)
#' @param bottom annotation (text or grob)
#' @param left annotation (text or grob)
#' @param right annotation (text or grob)
#' @param draw logical, draw the table
#' @rdname arrange
#' @export
gtable_arrange <- function(..., grobs=list(), as.table=TRUE,
                           top = NULL, bottom = NULL, 
                           left = NULL, right = NULL, draw=TRUE){
  
  dots <- list(...)
  params <- c("nrow", "ncol", "widths", "heights",
              "respect", "just", "z") # TODO currently ignored
  
  layout.call <- intersect(names(dots), params)
  params.layout <- dots[layout.call]
  
  if(is.null(names(dots)))
    not.grobnames <- FALSE else
      not.grobnames <- names(dots) %in% layout.call
  
  if(!length(grobs))
    grobs <- dots[! not.grobnames ]
  
  ## figure out the layout
  n <- length(grobs)
  nm <- n2mfrow(n)
  
  if(is.null(params.layout$nrow) & is.null(params.layout$ncol)) 
  {
    params.layout$nrow = nm[1]
    params.layout$ncol = nm[2]
  }
  if(is.null(params.layout$nrow))
    params.layout$nrow = ceiling(n/params.layout$ncol)
  if(is.null(params.layout$ncol))
    params.layout$ncol = ceiling(n/params.layout$nrow)
  
  if(is.null(params.layout$widths))
    params.layout$widths <- unit(rep(1, params.layout$ncol), "null")
  if(is.null(params.layout$heights))
    params.layout$heights <- unit(rep(1,params.layout$nrow), "null")
  
  positions <- expand.grid(row = seq_len(params.layout$nrow), 
                           col = seq_len(params.layout$ncol))
  if(as.table) # fill table by rows
    positions <- positions[order(positions$row),]
  
  positions <- positions[seq_along(grobs), ] # n might be < ncol*nrow
  
  ## build the gtable, similar steps to gtable_matrix
  
  gt <- gtable(name="table")
  gt <- gtable_add_cols(gt, params.layout$widths)
  gt <- gtable_add_rows(gt, params.layout$heights)
  gt <- gtable_add_grobs(gt, grobs, t = positions$row, 
                         l = positions$col)
  
  ## titles given as strings are converted to text grobs
  if (is.character(top)) 
    top <- textGrob(top)
  if (is.character(bottom)) 
    bottom <- textGrob(bottom)
  if (is.character(right)) 
    right <- textGrob(right, rot = -90)
  if (is.character(left)) 
    left <- textGrob(left, rot = 90)
  
  if(!is.null(top)){
    gt <- gtable_add_rows(gt, heights=grobHeight(top), 0)
    gt <- gtable_add_grobs(gt, top, t=1, l=1, r=ncol(gt))
  }
  if(!is.null(bottom)){
    gt <- gtable_add_rows(gt, heights=grobHeight(bottom), -1)
    gt <- gtable_add_grobs(gt, bottom, t=nrow(gt), l=1, r=ncol(gt))
  }
  if(!is.null(left)){
    gt <- gtable_add_cols(gt, widths=grobWidth(left), 0)
    gt <- gtable_add_grobs(gt, left, t=1, b=nrow(gt), l=1, r=1)
  }
  if(!is.null(right)){
    gt <- gtable_add_cols(gt, widths=grobWidth(right), -1)
    gt <- gtable_add_grobs(gt, right, t=1, b=nrow(gt), l=ncol(gt), r=ncol(gt))
  }
  
  if(draw){
    grid.newpage()
    grid.draw(gt)
  }
  invisible(gt)
}

#' Arrange multiple grobs on a page following a layout matrix
#'
#' @param ... additional parameters passed to gtable
#' @param grobs list of grobs
#' @param widths 
#' @param heights 
#' @param mat integer matrix defining the layout
#' @rdname layout
#' @export
gtable_from_layout <- function(grobs, mat, 
                               widths = NULL, heights = NULL, ...){
  
  if(is.null(widths))
    widths <- unit(rep(1, ncol(mat)), "null")
  if(is.null(heights))
    heights <- unit(rep(1, nrow(mat)), "null")
  
  
  cells <- sort(unique(as.vector(mat)))
  
  ## left/right/top/bottom borders for given id
  range_cell <- function(ii){
    ind <- which(mat == ii, arr.ind=TRUE)
    c(l=min(ind[,"col"]),
      r=max(ind[,"col"]),
      t=min(ind[,"row"]),
      b=max(ind[,"row"]))
  }
  
  glayout <- data.frame(do.call(rbind, lapply(cells, range_cell)))
  gt <- gtable(widths = widths, heights = heights, ...)
  
  with(glayout, gtable_add_grobs(gt, grobs, t=t, l=l, b=b, r=r))
  
}

#' Graphical display of a textual table.
#'
#' @param d data.frame or matrix
#' @param colour label colours
#' @param fill cell background colours
#' @export
#' @examples
#' \donttest{
#' d <- head(iris, 3)
#' core <- gtable_table(d, 1:5)
#' colhead <- gtable_table(t(colnames(d)))
#' rowhead <- gtable_table(c("", rownames(d)))
#' g <- rbind(colhead, core)
#' g <- cbind(rowhead, g)
#' grid.newpage()
#' grid.draw(g)
#' }
gtable_table <- function(d, colour = "black", fill = NA){
  
  label_matrix <- as.matrix(d)
  
  nc <- ncol(label_matrix)
  nr <- nrow(label_matrix)
  n <- nc*nr
  
  colour <- rep(colour, length.out = n)
  fill <- rep(fill, length.out = n)
  
  ## text for each cell
  labels <- lapply(seq_len(n), function(ii) 
    textGrob(label_matrix[ii], gp=gpar(col=colour[ii])))
  label_grobs <- matrix(labels, ncol=nc)
  
  ## define the fill background of cells
  fill <- lapply(seq_len(n), function(ii) 
    rectGrob(gp=gpar(fill=fill[ii])))
  
  ## some calculations of cell sizes
  row_heights <- function(m){
    do.call(unit.c, apply(m, 1, function(l)
      max(do.call(unit.c, lapply(l, grobHeight)))))
  }
  
  col_widths <- function(m){
    do.call(unit.c, apply(m, 2, function(l)
      max(do.call(unit.c, lapply(l, grobWidth)))))
  }
  
  ## place labels in a gtable
  g <- gtable_matrix("table", grobs=label_grobs, 
                     widths=col_widths(label_grobs) + unit(4,"mm"), 
                     heights=row_heights(label_grobs) + unit(4,"mm"))
  
  ## add the background
  g <- gtable_add_grob(g, fill, t=rep(seq_len(nr), each=nc), 
                       l=rep(seq_len(nc), nr), z=0, name="fill")
  
  g
}




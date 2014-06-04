#' Graphical display of a textual table.
#'
#' @param d data.frame or matrix
#' @param widths optional vector to specify column widths
#' @param heights optional vector to specify row heights
#' @param fg.par control parameters for text grobs
#' @param bg.par control parameters for rect grobs
#' @param padding unit of length 2
#' @export
#' @examples
#' \donttest{
#' d <- head(iris, 3)
#' core <- gtable_table(d, 
#'                      fg.par = list(col=1:5, fontsize=c(10,12,15)),
#'                      bg.par = list(fill=1:2, alpha=0.5))
#' colhead <- gtable_table(t(colnames(d)))
#' rowhead <- gtable_table(c("", rownames(d)))
#' g <- rbind(colhead, core)
#' g <- cbind(rowhead, g)
#' grid.newpage()
#' grid.draw(g)
#' g2 <- gtable_table(t(colnames(d)), widths = unit(rep(1, ncol(d)), "null"))
#' grid.newpage()
#' grid.draw(g)
#' }
gtable_table <- function(d, widths, heights,
                         fg.par = list(col = "black"),
                         bg.par = list(fill = NA),
                         padding = unit(c(4, 4), "mm")){
  
  label_matrix <- as.matrix(d)
  
  nc <- ncol(label_matrix)
  nr <- nrow(label_matrix)
  n <- nc*nr
  
  fg.par <- lapply(fg.par, rep, length.out = n)
  bg.par <- lapply(bg.par, rep, length.out = n)
  
  fg.param <- data.frame(fg.par, label = as.vector(label_matrix), 
                         stringsAsFactors=FALSE)
  bg.param <- data.frame(bg.par, id = seq_len(n),
                         stringsAsFactors=FALSE)
  
  labels <- plyr::mlply(fg.param, cell_content)
  backgrounds <- plyr::mlply(bg.param, cell_background)
  
  label_grobs <- matrix(labels, ncol = nc)
  
  ## some calculations of cell sizes
  row_heights <- function(m){
    do.call(unit.c, apply(m, 1, function(l)
      max(do.call(unit.c, lapply(l, grobHeight)))))
  }
  
  col_widths <- function(m){
    do.call(unit.c, apply(m, 2, function(l)
      max(do.call(unit.c, lapply(l, grobWidth)))))
  }
  
  if(missing(widths))
    widths <- col_widths(label_grobs) + padding[1]
  if(missing(heights))
    heights <- row_heights(label_grobs) + padding[2]
  
  ## place labels in a gtable
  g <- gtable_matrix("table", grobs=label_grobs, 
                     widths = widths, 
                     heights = heights)
  
  ## add the background
  g <- gtable_add_grob(g, backgrounds, t=rep(seq_len(nr), each=nc), 
                       l=rep(seq_len(nc), nr), z=0, name="fill")
  
  g
}


cell_content <- function(...){
  dots <- list(...)
  gpar.names <- c("col", "cex", "fontsize", "lineheight", 
                 "font", "fontfamily", "alpha")
  other.names <- c("label", "hjust", "vjust", "rot")
  gpar.args <- dots[intersect(names(dots), gpar.names)]
  gp <- do.call(gpar, gpar.args)
  other.args <- dots[intersect(names(dots), other.names)]
  do.call(textGrob, c(other.args, list(gp = gp)))

}

cell_background <- function(...){
  
  dots <- list(...)
  gpar.names <- c("fill", "col", "lty", "lwd", "cex", "alpha",
                  "lineend", "linejoin", "linemitre", 
                  "lex")
  gpar.args <- dots[intersect(names(dots), gpar.names)]
  gp <- do.call(gpar, gpar.args)
  do.call(rectGrob, list(gp = gp))
  
}



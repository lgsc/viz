library(flextable)

#' Column coloring
#'
#' This function colors a column of a flextable blue.
#' @param col_index The index of the column to color
#' @param data The dataframe that created the flextable.
#' @param ft A flextable.
color_blue <- function(col_index, data, ft){
  row_num = nrow(data)

  for (r in 1:row_num){
    if (data[r, col_index] >= 0.20){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#e6e6ff", part = "body")}

    if (data[r, col_index] >= 0.36){
        ft <- bg(ft, i=c(r), j=c(col_index),
                 bg="#ccccff", part = "body")}

    if (data[r, col_index] >= 0.46){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#b3b3ff", part = "body")}

    if (data[r, col_index] >= 0.55){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#9999ff", part = "body")}

    if (data[r, col_index] >= 0.65){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#8080ff", part = "body")}

    if (data[r, col_index] >= 0.81){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#6666ff", part = "body")}
    }
  return(ft)
  }


color_red <- function(col_index, data, ft){
  row_num = nrow(data)

  for (r in 1:row_num){
    if (data[r, col_index] >= 0.20){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#ffe6e6", part = "body")}

    if (data[r, col_index] >= 0.36){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#ffcccc", part = "body")}

    if (data[r, col_index] >= 0.46){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#ffb3b3", part = "body")}

    if (data[r, col_index] >= 0.55){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#ff9999", part = "body")}

    if (data[r, col_index] >= 0.65){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#ff8080", part = "body")}

    if (data[r, col_index] >= 0.81){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#ff6666", part = "body")}
  }
  return(ft)
}

color_green <- function(col_index, data, ft){
  row_num = nrow(data)

  for (r in 1:row_num){
    if (data[r, col_index] >= 0.20){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#e6ffee", part = "body")}

    if (data[r, col_index] >= 0.36){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#ccffdd", part = "body")}

    if (data[r, col_index] >= 0.46){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#b3ffcc", part = "body")}

    if (data[r, col_index] >= 0.55){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#99ffbb", part = "body")}

    if (data[r, col_index] >= 0.65){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#80ffaa", part = "body")}

    if (data[r, col_index] >= 0.81){
      ft <- bg(ft, i=c(r), j=c(col_index),
               bg="#66ff99", part = "body")}
  }
  return(ft)
}

format_ft <- function(ft, tab){
  ft <- align(ft, align = "center", part = 'all')
  ft <- fontsize(ft, part = "all", size = 12)
  return(ft)
}

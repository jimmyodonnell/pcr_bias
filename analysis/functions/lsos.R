#' Improved list of objects
#'
#' @param pos (integer)
#'        I think this is the environment
#' @param pattern (string)
#'        Restrict listing of search to a pattern
#' @param order.by (string)['Type', 'Size', 'Rows', 'Columns'] 
#'        Ordering of output. 
#' @param decreasing (boolean)
#'        Order output in decreasing order? Defaults to TRUE
#' @param head (boolean)
#'        Restrict total number of rows in output?
#' @param n (int)
#'        Total number of rows in output. Used only if head==TRUE
#' @return Dataframe of object size in an easy to read format
#' @references source:  http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#' @export
#' @examples
#' lsos()

lsos <- function (pos = 1, pattern, order.by="Size",
                        decreasing=TRUE, head=TRUE, n=10) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

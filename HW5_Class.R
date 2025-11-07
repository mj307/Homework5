## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

setValidity("sparse_numeric", function(object){
  len <- length(object@value) == length(object@pos)
  pos <- all(object@pos > 0)
  within <- all(object@pos <= object@length)
  unique <- all(object@pos == sort(unique(object@pos)))
  validLen <- length(object@length) == 1 && object@length > 0
  
  all (len, pos, within, unique, validLen)
})

#x <- new("sparse_numeric", value = c(4, 3.2, 6.1), pos = c(2L, 3L, 5L), length = 5L)
#y <- new("sparse_numeric", value = c(1, 2), pos = c(1L, 10L), length = 5L)

setAs(from="numeric", to="sparse_numeric", def=function(from) {
  pos <- as.integer(which(from != 0))
  vals <- from[pos]
  len <- as.integer(length(from))
  new("sparse_numeric", value = vals, pos=pos, length = len)
})

setAs(from="sparse_numeric", to="numeric", def=function(from) {
  output <- numeric(from@length)
  output[from@pos] <- from@value
  output
})



setGeneric("sparse_add", function(x, y) standardGeneric("sparse_add"))
setMethod("sparse_add", signature(x="sparse_numeric", y="sparse_numeric"), function(x,y){
  if (x@length != y@length)
    stop()
  everything <- sort(unique(c(x@pos, y@pos)))
  x_vals <- ifelse(everything %in% x@pos, x@value[match(everything, x@pos)], 0)
  y_vals <- ifelse(everything %in% y@pos, y@value[match(everything, y@pos)], 0)
  total <- x_vals + y_vals
  final <- which(total != 0)
  new("sparse_numeric", 
      pos = everything[final],
      value = total[final],
      length = x@length)
})

setMethod("+", signature(e1="sparse_numeric", e2="sparse_numeric"), function(e1,e2) sparse_add(e1,e2))



setGeneric("sparse_sub", function(x, y) standardGeneric("sparse_sub"))
setMethod("sparse_sub", signature(x="sparse_numeric", y="sparse_numeric"), function(x,y){
  if (x@length != y@length)
    stop()
  everything <- sort(unique(c(x@pos, y@pos)))
  x_vals <- ifelse(everything %in% x@pos, x@value[match(everything, x@pos)], 0)
  y_vals <- ifelse(everything %in% y@pos, y@value[match(everything, y@pos)], 0)
  diff <- x_vals -  y_vals
  final <- which(diff != 0)
  new("sparse_numeric", 
      pos = everything[final],
      value = diff[final],
      length = x@length)
})

setMethod("-", signature(e1="sparse_numeric", e2="sparse_numeric"), function(e1,e2) sparse_sub(e1,e2))



setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))
setMethod("sparse_mult", signature(x="sparse_numeric", y="sparse_numeric"), function(x,y){
  if (x@length != y@length)
    stop()
  
  overall_pos <- intersect(x@pos, y@pos)
  if (length(overall_pos) == 0) {
    return (new("sparse_numeric", value = numeric(0), pos=integer(0), length=x@length))
  }
  
  x_vals <- x@value[match(overall_pos, x@pos)]
  y_vals <- y@value[match(overall_pos, y@pos)]
  product <- x_vals * y_vals
  new("sparse_numeric", 
      pos = overall_pos,
      value = product,
      length = x@length)
  })

setMethod("*", signature(e1="sparse_numeric", e2="sparse_numeric"), function(e1,e2) sparse_mult(e1,e2))



setGeneric("sparse_crossprod", function(x, y) standardGeneric("sparse_crossprod"))

setMethod("sparse_crossprod", signature(x="sparse_numeric", y="sparse_numeric"), function(x, y){
  if (x@length != y@length) stop()
  
  overall_pos <- intersect(x@pos, y@pos)
  if (length(overall_pos) == 0) return(0)
  
  x_vals <- x@value[match(overall_pos, x@pos)]
  y_vals <- y@value[match(overall_pos, y@pos)]
  
  sum(x_vals * y_vals)
})

isGeneric("sparse_crossprod")  # should return TRUE
x <- as(c(0, 4, 0, 3, 0, 0, 2), "sparse_numeric")
y <- as(c(1, 0, 0, 3, 5, 0, 0), "sparse_numeric")
sparse_crossprod(x, y)  # should return 9



setMethod("show", "sparse_numeric", function(object){
  cat("The vector has length", object@length, "\n")
  cat("The position are:", object@pos, "\n")
  cat("And the vals are", object@value, "\n")
})

setMethod("plot", signature(x="sparse_numeric", y="sparse_numeric"), function(x,y,...){
  plot(1:x@length, rep(0, x@length), type = "n", ylim = range(c(x@value, y@value)),xlab = "Position", ylab = "Value", main = "Sparse Vectors", ...)
  
  points(x@pos, x@value, col="green",pch=20)
  points(y@pos, y@value, col="purple",pch=17)
  legend("topleft", legend=c("x","y"), col=c("green", "purple"),pch=c(20,17))
})


x <- as(c(0, 4, 0, 3, 0, 0, 2), "sparse_numeric")
y <- as(c(1, 0, 0, 3, 5, 0, 0), "sparse_numeric")

plot(x, y)


setMethod("length", "sparse_numeric", function(x){
  x@length
})
length(x)







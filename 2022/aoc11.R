
setClass("Monkey", representation(name = 'character', items = 'vector', op = 'character', num = 'numeric',
                                  test = 'numeric', monkey_true = "numeric", monkey_false = "numeric", count = "numeric"))


m1 <- new('Monkey', name = "monk1", items = c(79,98), op = "*", num = 23, test = 10, monkey_true = 3, monkey_false =  4, count = 0)
m2 <- new('Monkey', name = "monk2", items = c(54,65,75,74), op = "+", num = 19, test = 1, monkey_true = 3, monkey_false = 1, count = 0)
m3 <- new('Monkey', name = "monk3", items = c(79,60,97), op = "*", num = 0, test = 13, monkey_true = 2, monkey_false = 3, count = 0)
m4 <- new('Monkey', name = "monk4", items = c(4), op = "+", num = 3, test = 17, monkey_true = 1, monkey_false = 2, count = 0)
mtest <- new('Monkey', name = "monktest", items = c(0), op = "+", num = 3, test = 17, monkey_true = 1, monkey_false = 2, count = 0)

op_reader <- function(old,op,num){
  if(num == 0){num <- old}
  
  x <- switch(op,
              "+" = old+num,
              "*" = old*num)
  return(x)
}

setGeneric("monkey_actions", function(object){
  standardGeneric("monkey_actions")
})


monkey_actions <- function(object){
  return_vals <- list()
  for(i in object@items){
    
    object@count <- object@count + 1
    
    new <- op_reader(i,object@op,object@num)
    if(new %% object@test == 0){
      print('True')
      return_val <- c(i, object@monkey_true)

    } else {
      print('False')
      return_val <- c(i, object@monkey_false)
    }
    return_vals <- append(return_vals, list(return_val))
    
  } 
  return(return_vals)
}

monkey_switch <- function(return_vals){

  for(i in seq_along(x)){
    item <- as.integer(x[[i]][1])
    monkey <- as.character(x[[i]][2])
    print(monkey)
    
    switch(monkey,
           "1" = (m1@items <- append(m1@items, item)),
           "2" = (m2@items <- append(m2@items, item)),
           "3" = (m3@items <- append(m3@items, item)),
           "4" = (m4@items <- append(m4@items, item))
    )
  }
}

#######

monkey_name <- function(x) x@name

setGeneric("monkey_inc", function(x, value) standardGeneric("monkey_inc"))

setMethod("monkey_inc", "Monkey", function(x) {
  y = x@count+1
  x@count <- y
  x
})

monkey_inc(m1)

m1@count


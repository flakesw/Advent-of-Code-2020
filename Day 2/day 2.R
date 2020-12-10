input <- read.csv("./Day 2/input.txt", sep = " ", 
                  header = FALSE, stringsAsFactors = FALSE)
input <- readLines("./Day 2/input.txt")

passwords <- do.call(rbind, strsplit(input, '[- ]|\\: '))

passwords <- setNames(as.data.frame(passwords, stringsAsFactors = FALSE),
                      c('min', 'max', 'letter', 'password'))
passwords <- transform(passwords,
                       min = as.integer(min),
                       max = as.integer(max))

foo <- function(min, max, letter, password){
  split_pass <- strsplit(password, character(0))
  between(sum(sapply(split_pass, FUN = function(x){x == letter})), 
          min, max) 
}

#answer to part 1
sum(mapply(foo, 
           passwords$min, 
           passwords$max, 
           passwords$letter, 
           passwords$password))

#part 2
input <- read.csv("./Day 2/input.txt", sep = " ", 
                  header = FALSE, stringsAsFactors = FALSE)
input <- readLines("./Day 2/input.txt")

passwords <- do.call(rbind, strsplit(input, '[- ]|\\: '))

passwords <- setNames(as.data.frame(passwords, stringsAsFactors = FALSE),
                      c('min', 'max', 'letter', 'password'))
passwords <- transform(passwords,
                       min = as.integer(min),
                       max = as.integer(max))

foo <- function(min, max, letter, password){
  split_pass <- unlist(strsplit(password, character(0)))
  xor(split_pass[min] == letter, split_pass[max] == letter)
}

#answer to part 2
sum(mapply(foo, 
           passwords$min, 
           passwords$max, 
           passwords$letter, 
           passwords$password))

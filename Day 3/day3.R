input <- readLines("./Day 3/input.txt")

input <- strsplit(input, character(0))

# there was a problem with building this array -- it was filling by columns 
# when I expected rows. Keep that in mind
map <- t(array(unlist(input), dim = c(length(unlist(input[[1]])),length(input))))

count_trees <- function(right, down){
  
  #y sequence
  y <- seq(from = 1, by = down, to = nrow(map))
  
  #x sequence
  x <- seq(from = 1, by = right, length.out = length(y)) %% ncol(map)
  x[x == 0] <- ncol(map)
  
  is.tree <- function(x,y){
    map[y, x] == "#"
  }
  
  sum(mapply(is.tree, x, y))
}

#answer to part 1
count_trees(3, 1)

#part 2
slopes <- list(right = c(1,3,5,7,1),
                     down = c(1,1,1,1,2))

prod(mapply(count_trees, right = slopes$right, down = slopes$down))

# part 1

input <- as.vector(read.csv("./Day 1/input.txt", header = FALSE)[, 1])

sums <- matrix(nrow = 200, ncol = 200)

for(i in 1:200){
  sums[i, ] <- input[i] + input
}

prod(input[which(sums == 2020, arr.ind = TRUE)[1, ]])


#part 2
#same approach but with 3-dimensional array

input <- as.vector(read.csv("./Day 1/input.txt", header = FALSE)[, 1])

sums <- array(dim = c(200,200,200))

for(i in 1:200){
    for(j in 1:200){
    sums[i, j, ] <- input[i] + input[j] + input
  }
}

prod(input[which(sums == 2020, arr.ind = TRUE)[1, ]])

input <- readLines("./Day 4/input.txt")

#there's probably a fancy regex solution
cuts <- which(input == "")
cuts <- c(0, cuts, length(input)+1) #add start and end points

passports <- character()

for(i in 1:(length(cuts) - 1)){
  #concatenate everything between the cuts
  passports[i] <- paste(input[(cuts[i] + 1):(cuts[i+1] - 1)], collapse = " ")
  
}

count_elements <- function(passport_keys){
  elements <- unlist(strsplit(passport_keys, split = " "))
  #select everything but cid
  elements <- elements[grep("cid", elements, invert = TRUE)]
  return(length(elements))
}

#answer to part 1
sum((sapply(passports, count_elements) == 7))

#part 2
valid <- rep(FALSE, length.out = length(passports))
valid[sapply(passports, count_elements) == 7] <- TRUE

patterns <- c(
  "byr:19[2-9].|200[0-2]( |$)",
  "iyr:201.|2020( |$)",
  "eyr:202.|2030( |$)",
  "hgt:(((1[5-8].|19[0-3])cm)|((59|6[0-9]|7[0-6])in))( |$)",
  "hcl:#[a-f0-9]{6}( |$)",
  "ecl:(amb|blu|brn|gry|grn|hzl|oth)( |$)",
  "pid:\\d{9}( |$)"
)

for(i in 1:length(patterns)){
  valid <- valid * grepl(patterns[i], passports)
}

sum(valid)
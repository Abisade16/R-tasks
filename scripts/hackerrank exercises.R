#Hello World!
print("Hello World!")

#If-else statement
n=22

if (n%%2 ==0) {
  if (n>= 2 & n<= 5) {
    print("Not Weird")
  } else if (n>=6 & n<= 20) {
    print("Weird")
  } else {
    print("Not Weird")
  }
} else {
  print("Weird")
}

#Arthimetic Operations
a <- readline() #gets user input
b <- readline()
a <- as.numeric(a) #converts to numerical datatype
b <- as.numeric(b)
a+b #adds numbers
a-b #subtracts numbers
a*b #multiplies numbers

#Division
c <- readline()
d <-  readline()
integer <- as.numeric(c) %/% as.numeric(d) #prints the result as a whole number
float <-  as.numeric(c) / as.numeric(d) #prints the result as a decimal number

#Loops
n <-5
for (i in 0:(n-1)) {
  square <- i^2 #square each value of i
  print(square)
  n <- n-1 #value of n decreases
}

#Function
leap_year <- function(year) {
  if (year %% 4 != 0) { #the year divided by 4 is not equal to 0
    return (FALSE )
    }
  else {
    if (year %% 400 == 0) { #the year divided by 400 is equal to 0
      return (TRUE)
      }
    else if (year %% 100 == 0) { #the year divided by 100 is equal to 0
      return (FALSE)
      }
    else {
      return (TRUE)
    }
    }
}
leap_year(1900)
#if a year is divisible by 4 but not by 100, it is not a leap year
#if a year is divisible by 4 and by 400, it is a leap year

#list comprehension
#using for loop
x <- as.numeric(readline(prompt="The first number? ")) #gets user input
y <- as.numeric(readline(prompt="The second number? "))
z <- as.numeric(readline(prompt="The third number? "))
number <- as.numeric(readline(prompt= "Summation number? ")) #x+y+z must not be equal to 'number'

answer <- list() #a list of x,y & z sequence combinations
for (i in 0:x) {
  for (j in 0:y) {
    for (k in 0:z) {
      if (i + j + k == number) {
        next #skips immediately it sums up to 'number'
      } else {
        answer <- append(answer, list(c(i, j, k)))
      }
    }
  }
}
print(answer)
#using list
l <- 1
m <- 1
p <- 1
summed_value <- 2

grid <- expand.grid(e = 0:l, f = 0:m, g = 0:p) #generates all possible combinations 
print(subset(grid, e + f + g != summed_value)) #skips combinations that are equal to 'summed_value'

#First Runner up score
number_of_scores <- as.numeric(readline("How many scores are there? ")) #gets the number of scores
arr <- numeric()
for (i in 1:number_of_scores) {
  numbers <-as.numeric(readline(paste("Enter score", i, ": "))) #each score is inputted
  arr <-  append(arr,numbers) #each score is appended
}

unique_sorted <- sort(unique(arr), decreasing = TRUE) #each unique scores are arranged in descending order

if (length(unique_sorted) >= 2) {
  runner_up <- unique_sorted[2] #second highest number
  print(runner_up)
} else {
  print("No runner-up score found.")
}

#Nested lists
students <- list()
scores <- numeric()
record <- list()
N <- as.numeric(readline("How many students are there? ")) #gets user input

for (i in 1:N) {
  names <- readline(paste("What's number", i, "'s name? ")) #name of each student
  students <- append(students, names) #added to the 'students' list
  grade <- as.numeric(readline(paste("What's", names, "'s score? "))) #grade of each student
  scores <-  append(scores, grade) #added to the 'scores' list as numerical data type
  record <- append(record, list(list(Student= names, Scores= grade))) #each student and corresponding score is added to 'records' list
}
scores_only <- unlist(lapply(record, function(x) x$Scores)) #separates each individual grade of each student
scores_only <- sort(unique(scores_only)) #each unique grade is sorted

second_lowest <- scores_only[2] #second lowest score
matching_names <- c()

for (student in record) {
  if (student$Scores == second_lowest) {
    matching_names <- c(matching_names, student$Student) #gets student(s) with the low grade
  }
}
sorted_names <-  sort(matching_names) #each student name is arranged in ascending order

#Finding the percentage
number_of_names <- as.numeric(readline("How many names are there? ")) #gets user input
Record <- c()
names_involved <- list()
marks <-list()
query_name <- readline("What's the query name? ")
for (i in 1:number_of_names) {
  input <- readline(paste("What are the names and marks? (separate with a space) ")) #names with their scores
  Record <- append(Record, input) #added to 'Record'
}

for (i in 1:length(Record)) {
  parts <- strsplit(Record[i], " ")[[1]] #separates each input
  first_part <- parts[1]
  rest_part <- as.numeric(parts[-1])
  names_involved <-  append(names_involved, first_part) #adds each separated name to 'names_involved'
  marks <- append(marks,list(rest_part)) #adds each separated score to 'marks'
}

if (query_name %in% names_involved) {
  index <- which(names_involved == query_name) #gets the index of the given name
  avg_score <- mean(marks[[index]]) #gets the average score
  paste("Average score for", query_name, "is:", sprintf("%.2f", avg_score))
} else {
  print("Name not found in records.")
}


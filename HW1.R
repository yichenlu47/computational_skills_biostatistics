## Biost 561  Assignment 1
## Name: Sirui Liu and Yichen Lu 


rm(list = ls())

## Problem 1
# Consider the following list
my_list <- list(
  a1 = list(b1 = list(c1="a1b1c1", c2="a1b1c2"),
            b2 = list(c1="a1b2c1", c2="a1b2c2")
  ),
  a2 = 1
)

# Problem 1.1

#Explain what each of the following lines is doing and indicate which ones are equivalent:
my_list[["a1"]]  
# It extracts the slice a1 in the defined list

my_list[["a1"]][["b2"]]    
# It extracts the slice b2 in a1 in the defined list. 

my_list[["a1"]][["b2"]][["c1"]]    
# It extracts the slice c1, which is in the sublist of b2 in a1 in the defined list.

my_list[[c(1,2)]]    
# It extracts contents of b2, which is c1 and c2. Their contents is "a1b2c1" and "a1b2c2"/

my_list[[c(1,2,1)]]      
# It extracts contents of c1.

my_list[[c("a1", "b2")]]  
# It extracts contents of b2, which is in the sublist of a1. 

my_list[[c("a1", "b2", "c1")]]   
# It extracts contents of c2, which is in the sublist of b2, and b2 is in the sublist of a1. 

## Among these command lines, line4 and line6, line 5 and line7 are considered equivalent. 

# Problem 1.2
# Explain the difference between the following two lines:
my_list[c("a1", "b2", "c1")]
my_list[[c("a1", "b2", "c1")]]

# The difference between these two lines is that the first one extracts the sublist of a1 because it
# uses the single square bracket operator. The second line uses the double square bracket operator, so 
# it references to the contents of c1 in the b2, which is in a1. 

my_list[c(1,2)]
my_list[[c(1,2)]]

# The difference between these two lines is that the first line uses the single square bracket operator, 
# so it returns all the contents that are under the components of first element. 
# The second line uses the double square bracket operator, so it directly references to the contents of the 
# second element which is under the first element of the defined list. 


## Problem 1.3
my_list[]
my_list[[]]
# By making use the analogy, my_list[[]] is like extracting the object in car NULL because there is no assigned 
# number of vector in the double square bracket operator, so it returns NULL value. 
# The first line uses the single square bracket operator, but there is nothing assigned in the bracket, so it returns 
# the whole 'train'. 

## Problem 2
summaries <- data.frame(patient = c(1:5,5),
                        treatment = c("a","b","a","c","a","c"),
                        visits = c(2,1,2,1,3,1))
patients <- data.frame(ID = 1:6, name = LETTERS[1:6], age = rpois(6,50))
treatments <- data.frame(code = c("a","b","c"), name = tail(letters,3),
                         cost = c(3,1000,100))

# Problem 2.1
# Create a data frame visits with columns patient and treatment and one row per visit.

dd<-summaries[c(rep(1,2),2,rep(3,2),4,rep(5,3),6),]
visits<- data.frame(patient = dd$patient,
                    treatment = dd$treatment)


# Problem 2.2
# Create a data frame visits1 replacing the codes for patients and treatments in visits with their names.
# Hint: the match function can come in handy here.

## didnt figure out match(), a naive way to replace all the code values 
visits1<-visits
visits1$patient[visits1$patient==1]<-"A"
visits1$patient[visits1$patient==2]<-"B"
visits1$patient[visits1$patient==3]<-"C"
visits1$patient[visits1$patient==4]<-"D"
visits1$patient[visits1$patient==5]<-"E"

levels(visits1$treatment)<-c("x","y","z")



#Problem 2.3
# Use the order function to create a data frame visits2 (from visits1) such that its rows are sorted by
# treatment.
visits2<-visits1[order(visits1$treatment),]


# Problem 2.4
# Use the function subset to extract from visits2 all the visits related to treatment x and store the result
# into query1.

query1<-subset(visits2,visits2$treatment=="x")






# Problem 3
#Provide a simple example of how to use each of these 16 functions: %in%, complete.cases, %%, %/%,
# floor, pmax, rle, xor, any, intersect, setequal, which, rev, choose, split, expand.grid

# %in% : to identify if the given element belongs to a prespecified vector 
vector1 <- c("A","BB","CCC","DDDD")
elementA<- "A"
elementB <-"B"
elementA %in% vector1 # TRUE because A belongs to vector 1
elementB %in% vector1 # FLASE becasue vector1 does not have element "B"

# complete.cases
colA <- c(1,2,3,4,5,NA)
colB <- c(2,3,4,5,NA,6)
colC<-c(34,NA,NA,9,9,0)
ddd<-data.frame(colA,colB,colC)
complete.cases(ddd)
#returns the row in given data that has missing values
ddd[complete.cases(ddd), ]
# only keep the row that does not have missing values


# %% modulus
9 %% 3 # = 0 becasue there is no remainder left after division
8 %% 3 # =2 ,remainder is 2

# %/% : integer division
9 %/% 3 # =3
9  %/% 2 # =4

# floor : to find the floor values 
floor(-10.285) # -11
floor(3.444)  #3

# pmax: returns the parallel maxima vector of multiple vectors or matrix.
pmax(colA,colB,colC)
pmax(colA,colB,colC,na.rm = TRUE)

# rle: run length encoding 
x <- c(2,4,2,5,6,3)
rle(x)
y<- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE)
rle(y)
# the outpus
#  lengths: int [1:5] 2 2 1 1 2
#   values : logi [1:5] TRUE FALSE TRUE FALSE TRUE
#  rle(y) means y has two true, two false, one true ...

# xor:  elementwise exclusive OR
a<-TRUE
b<-FALSE
xor(a,b)
xor(a,a)
xor(FALSE,FALSE)

# any: Given a set of logical vectors, is at least one of the values true?
c<- c(1,2,3,4,5)
any(c<0)
# it returns FALSE becasue there is no value in c that is smaller than zero


# intersect: return the values that both given vevtors/others have 
intersect(colA,colB)

# setequal: to justify if two sets have duplicated elements
set_a <- c('dog', 'cat', 'bird'  )
set_b <- c('whales', 'chicken', 'duck')
set_c <- c('dog', 'cat', 'bird' , 'whales', 'chicken', 'duck' )
setequal(set_a,set_b)
setequal(set_b,set_c)


# which:  return the position of the elements(i.e., row number/column number/array index) 
#         in a logical vector which are TRUE
which(x==2)
which(x==3)

# rev:reverse the order of elements 

rev(x)
rev(y)

# choose(n,k): the number of ways that choosing k from n
choose(7,1)
choose(7,2)
choose(1,1)
choose(10,3)


# split：divides the data in the vector x into the groups defined by f
split(x, x==2)

#expand.grid：creates a data frame from all combinations of the supplied vectors or factors.
species <- c("dog","cat","bird")
color <-c("white","black","grey")
height <-c("1cm","2cm")
expand.grid(species,color,height)

## Problem 4

#Problem 4.1
# Use sapply and [ to extract the entries 2 and 3 for each vector in the list below so that the result is a matrix.
x <- list(1:3, 4:9, 10:12)

#solution:
sapply(x,'[',c(2,3))


# Problem 4.2
# Consider a list of matrices
mat_list <- replicate(4, matrix(rpois(25, lambda=1), 5, 5), simplify=FALSE)

# And consider a list with row indices for each matrix in mat_list
row_inds <- replicate(3, sample(1:5,sample(1:5,1)), simplify=FALSE)
row_inds[[4]] <- 1

# Compute the sums of the rows of each mat_list[[i]][row_inds[[i]],] where i=1,2,3,4, without using a
# for loop, and using the mapply and [ functions.
mapply(function(i) sum(mat_list[[i]][row_inds[[i]],]),1:4)


## Problem 5
# Re-write the following as a typical for loop
y <- rep(NA,5)
`for`(i, 1:5, `{`(y[i] <- i, y[i] <- y[i]^2) )
# Answer:
y <- rep(NA,5)
for (i in (1:5)){
  y[i] <- i
  y[i] <- y[i]^2
}


## Problem 6
# Explain what is going on with this function:
f1 <- function(x = {y <- 1; 2}, y = 0) { 
  x+y
} 
f1()
# Answer:
# the function adds together x and y
# y is set to have default value of 0 but is not used in this function because its set to 1 in the x = {y <- 1; 2} part 
# x is set to have default value of 2. since no x and y values are passed into the function when calling f1(), 
# the default value 2 of x is used and 1 is used for y
# if the function is called with user-entered values of x and y, the given values will be added and returned

## Problem 7
# Now, let x be a vector of real numbers, e.g. x <- rnorm(10). 
# Create a replacement function that replaces the entries in x that are lower than or equal to value with value, and illustrate its use.
# Answer:
`modif<-` <- function(y, value) { 
  for (i in 1:length(y)){ if (y[i] <= value) y[i]= value} #loop through every element in the vector and repleace these <= the input value with the input value
  y #return the updated vector
}
x <- rnorm(10)
modif(x)  <- 0.1 #input x is the vector, input on the right hand side of <- (currently 0.1) will be the value compared to
x

## Problem 8
# The expected input of fn1, fn2 and fn3 below is either TRUE or FALSE. 
# Are these functions equivalent in terms of their output? Explain.
# Answer:
fn1 <- function(a) { 
  if ( a ) 1 else 0
}
fn2 <- function(a) { 
  if ( a ) return(1)
  0
}
fn3 <- function(a) {
  if ( a ) return(1) else return(0)
}
# all three functions returns 1 if input a is TRUE, returns 0 if a is FALSE
# In fn1, if a is TRUE, the function goes to 1 which will be the last line in the function and output 1
#         if a is FALSE, the function skips 1 and goes to 0 which becomes the last line in the function and output 0
# In fn2, if a is TRUE, return 1 and still proceed to next row 0 (but the function will not output 0 because something is already returned).
#         if a is FALSE, the function skips the return(1) part and output the last line in the function which is 0
# In fn3, if a is TRUE, return 1 and the function ends; if a is FALSE, return 0 and the function ends



## Problem 9
# Answer:
fn <- function(n) { 
  fnn <- function(x) x^n #assign so the result won't print
}


## Problem 10
# Modify the function save_plot so that it always closes the graphics device regardless of whether plot fails.
save_plot <- function(x, y, file) { 
  pdf(file)
  on.exit() #the function has to run plot and dev.off regardless
  plot(x,y)
  dev.off()
}
save_plot(1,2,"/Users/Yichen/Desktop/my_plot.pdf")


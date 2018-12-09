library(ggplot2)

# Now we define the dimensions of the arrangement we need:

lrows <- 3035
lcols <- 11

# in this case 3035 rows and 11 columns.

# Now we define the array first containing zeros on all entries:

syn_data <- array(data = 0, dim = c(lrows, lcols))

# Our data look like this:


# Now let’s name each field (column):

colnames(syn_data) <- c("ONE","NUMBER","R1","R2","R3", "R4", "R5", "R6","NINE","TEN", "ELEVEN")

# Now let’s assign values to some columns. 

syn_data[,2] <- c(seq(lrows, 1))
syn_data[,1] <- c(runif(lrows, 6, 7))
syn_data[,9] <- c(runif(lrows, 10, 11))
syn_data[,10] <-c(runif(lrows, 5.0, 6))
syn_data[,11] <-c(runif(lrows, 1.0, 2.0))

# You can see each line of the script and see what kind of value it assigned to each entry of which column:



# Now for columns R1 to R6 I want to assign a random integer value between 1 and 56 for which we use the following -for- and -while- cycle:

for(i in 1:lrows){
  j = 1
 while(j <= 6){
   syn_data[i,j+2] <- sample(1:56,1)
  j = j + 1
  }
 }

# Now our data looks like this:



# So far we have our synthetic data. Now let’s do some treatments.
  
# First we convert the array to a Data Frame type object:

syn_data <- as.data.frame(syn_data)

# Now let us calculate the mean of each row from R1 to R6 and accumulate these means in a vector:

smeans = vector()

for(i in 1:lrows){
  smeans[i] <- sum(syn_data[i , 3:8])/6
}


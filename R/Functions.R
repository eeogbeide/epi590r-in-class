#Practice exercise from slide


x <- c(1, 3, 5, 7, 9)
n <- length(x)
sum(x) / n

new_mean <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}

#You Try!

# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2

square <- function(x) {
	squared_val <- x*x
	return(squared_val)
}

# test it out
square(x)
square(53)
53^2 # does this match?


# 1. Write a function to raise to any power. You don’t just want to square
# numbers, you want to raise them to higher powers too. Make a function that
# uses two arguments, x for a number, and power for the power. Call it raise().
x <-2
power <- 4

x^power

raise <- function(x) {
	raised_val <- x^power
	return(raised_val)
}

raise(x)

# test with
raise(x = 2, power = 4)
# should give you
2^4

#2. Change your raise() function to default to squaring x when the user doesn’t
#enter a value for power.

# test
raise(x = 5)
# should give you
5^2

x <-5
power <- 4

x^power

raise <- function(x,power = 2) {
	raised_val <- x^power
	return(raised_val)
}
#same
raise(x=2, power=4)
raise(power=4,x=2)

#different
raise(2,4)
raise(4,2)

#2.6 Exercise 1. Create a function that takes a vector of numbers and returns
#the standard deviation manually (like we did the mean). Use if statements to
#check if the vector has only one (or fewer) elements and return NA if so.
#(Hint: the length() function will be helpful!) You don’t need any extra
#arguments besides the vector of numbers.sd (𝑥)=√∑𝑛𝑖=1(𝑥𝑖−𝑥)2 𝑛−1

prop <- function(x, percentage = FALSE) {
	n <- length(x)
	mean_val <- sum(x) / n
	if (percentage) {
		mean_val <- mean_val * 100
	} else {
		# don't actually need this else statement!
		mean_val <- mean_val
	}
	return(mean_val)
}

x <- c(234,634, 124, 5437, 723, 927)
demeaned_x <- x - mean(x)
squared_demeaned_x <-demeaned_x^2
sum_of_squares <- sum(squared_demeaned_x)
n_minus_1 <- length(x)-1
std_dev <- sqrt(sum_of_squares / n_minus_1)


new_sd<- function(x){
	n <- length(x)
	demeaned_x <- x - mean(x)
	squared_demeaned_x <-demeaned_x^2
	sum_of_squares <- sum(squared_demeaned_x)
	n_minus_1 <- length(x)-1
	std_dev <- sqrt(sum_of_squares / n_minus_1)
	if (n<=1) {
		std_dev <- NA
	}
	else {
		std_dev<-std_dev
	}
	return(std_dev)
}

new_sd(x)
sd(x)
new_sd(c(30584,245,2020))
sd(c(30584,245,2020))


#2. Modify your function to remove the NA values before calculating the standard
#deviation. (Hint: the na.omit() function will be helpful!) Add an argument
#na.rm = that defaults to TRUE (the opposite of the na.rm argument in the
#built-in R function sd(), which defaults to FALSE). If na.rm = FALSE, then the
#function should return NA if there are any NA values in the vector.

x <- c(234,634, 124, 5437, 723, NA)

new_sd<- function(x,na.rm = TRUE){
	if (na.rm){
		#remove NAs
		new_x <-na.omit(x)
	} else{
		#don't remove NAs
		new_x <- x
}		# will have a new value of x if NAs are removed, same value if not
		if (length(new_x) <=1 ) {
			return_val <- NA
		} else{
			#calculate the standard deviation using new x and save as return_val
			demeaned_x <- new_x - mean(new_x)
			squared_demeaned_x <-demeaned_x^2
			sum_of_squares <- sum(squared_demeaned_x)
			n_minus_1 <- length(new_x)-1
			return_val <- sqrt(sum_of_squares / n_minus_1)
		}
	return(return_val)
}


new_sd(x)
sd(x,na.rm=TRUE)
new_sd(c(1,245,2020))
sd(c(1,245,2020))

str(nlsy)
new_sd(nlsy$income)

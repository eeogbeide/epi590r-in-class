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

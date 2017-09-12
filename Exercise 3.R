# In-Class Exercise 3

## Loading Library
library(parallel)
library(stringr)
library(stringdist)
library(plyr)

# Exercise 2.3 Using readLines

## Using Read Lines to Get Data

(txt <-  readLines("./example.txt"))

## Using grepl to detect lines containing comments and Data

cvec <- txt[grepl("^//", txt)]
cvec

(dvec <- txt[!grepl("^//", txt)])
dvec

## Splitting the Data with the ;"

(split_1 <- strsplit(dvec, ";"))


## Creating the Function for the Fields and Standardization While Appending Rows Shorter with NAs

aFields <- function(x, n = 3){
  if(!is.vector(x) | is.null(x) | n <= 0){
    return("Either it's not a vector, or a NA element, or your
           pivot index is not at least 1")
  }
  out <- character(n)
  # get gender names
  i <- grepl("[[:alpha:]]",x)
  out[1] <- x[i]
  # get person's age
  # Since weight is a decimal, hoping that none of it is written as
  # an integer or 12.0 it wouldn't work
  i <- which(as.integer(x) == x)
  out[2] <- ifelse(length(i) > 0, x[i], NA)
  # get their weight
  if(!any(grepl(",", x))){
    i <- which(as.integer(x) != x)
  } else {
    i <- which(grepl(",", x))
  }
  out[3] <- ifelse(length(i) >0, x[i], NA)
  out
  }


## Max Number of Fields

max_fields <- lapply(split_1, aFields)
max_fields


## Unlisting and Turning it Into a Matrix

(matr <- matrix(
  unlist(max_fields)
  , nrow=length(max_fields)
  , byrow=TRUE))

## Extract Names of the Fields and Set as Column Names 
columnNames <- strsplit(cvec[2:4], ":")
(columnNames <- gsub("^ ", "", unlist(columnNames)[rep(c(FALSE,TRUE),2)]))

colnames(matr) <- columnNames

## Printing Out the Matrix
matr

# 2.4 Coercing mat to data.frame


## Coerce to Data Frame 

(dat_coef <- data.frame(matr, stringsAsFactors = F))
sapply(dat_coef, class)

## Using the String Distance Technique to Transform to Factor Variable

codes <- c("male", "female")
dist_tnqe <- adist(dat_coef$Gender, codes)
colnames(dist_tnqe) <- codes
rownames(dist_tnqe) <- dat_coef$Gender
dist_tnqe

indx <- apply(dist_tnqe, 1, which.min)
(dat2_coef <- data.frame(gender = dat_coef$Gender, coded = codes[indx]))

mptVec <- plyr::revalue(dat2_coef$coded, c(male = "man", female = "woman"))
(dat_coef$Gender <- mptVec)

dat_coef

sapply(dat_coef, class)

## Coerce Age into Integers

(dat_coef$Age..in.years. <- as.integer(dat_coef$Age))

sapply(dat_coef, class)


## Coerce Weight into Numeric

dat_coef$Weight..in.kg. <- gsub(",", ".", dat_coef$Weight)
dat_coef$Weight..in.kg. <- as.numeric(dat_coef$Weight..in.kg.)
dat_coef


sapply(dat_coef, class)
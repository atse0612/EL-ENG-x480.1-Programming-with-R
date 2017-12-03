# In-Class Assignment #1


## Running the mtcars dataset
data(mtcars)
names(mtcars)
?mtcars

## Plotting the mtcars dataset and visualizing the data

plot(mtcars$hp, mtcars$mpg,       # creating a plot
     xlab ='Horsepower',          # x-axis label
     ylab ='MPG (Miles Per Gallon)', #y-axis label
     main = 'Car Efficiency', # give a title
     sub = 'Motor Trend Car Tests in 1974', # give a subtitle
     col ='salmon',     # make salmon symbols
     pch = 19                     # set filled points and shapes
)

### START ###

# project: AREC596 - Fall 2024
# created by: alj
# created on: 25 September 2024
# edited by: alj
# edited on: 25 September 2024

# PACKAGES USED: 
### arrow
### dplyr 
### ggplot2
### tidyverse 

# preparing our workspace 
# we use Projects to organize our work 
### a Project is just R's name for a folder

# for getting started, let's use drop down menus
# File > New Project
### in the first dialog select: "New Directory" 

# an aside, an alternative way to create a directory
# define directory
setwd("")

### in the second dialog select: "New Project"
### give the directory a name
### ... let's talk about naming conventions! 
### (1) brief, (2) effective, (3) informative 

# we need to tell R where to put this 
# for now, let's put it on our desktop
### we'll talk more about workflow later 
### for now, this will make it easy to find 

# we'll talk more about file organization 
# for now, though, think about best practices
# there should be a one-way flow of information
# we take information from data 
# we write code to produce output
# we want to ensure we do not change our data
# so we create separate folders: (1) data, (2) output

dir.create("data")
dir.create("output")

### END ###
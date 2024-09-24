### START ###

# learning r
# created by: alj
# created on: 17 july
# edited by: alj
# edited on: 13 august 

# PACKAGES USED: 
### arrow
### dplyr 
### ggplot2
### tidyverse 

# LESSON 1: visualization and data summarizing 
### learning objectives 
### 1. understand the difference between files and objects
### 2. modify data with data hygiene
### 3. summarize information from raw data
### 4. visualize data to convey information 

# preparing our workspace 
# we use Projects to organize our work 
### a Project is just R's name for a folder

# for getting started, let's use drop down menus
# File > New Project
### in the first dialog select: "New Directory" 

# an aside, an alternative way to create a directory
# define directory
setwd("C:/Users/aljosephson/Dropbox/HSI/data")

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

# now we can talk about getting our data
# there are lots of different ways to get data
# for example, you can get data in R directly from the web!

# we are going to pause here and make sure that everyone has the file
# to use the data we're going to get a bit ahead of ourselves
# we need to import a package to be able to use this file type 
# packages are a set of R functions, compiled code, and sample data. 
# they are kept in a library 
# we'll talk more about them 

# import data
library(arrow)

# now we have our data in R
# so let's begin a scrip to start working with these data
# let's make it through the File menu
### File > New File > R Script 

# at the beginning of a script, you want to add some information 
# personally, I usually add:
### learning r (project name)
### created by: alj
### created on: date
### edited by: alj
### edited on: date 

# now we need to save our file
# you can see there are unsaved changes
### a little asterisk by the file name ("Untitled1" or something like that)
# give your file a short and descriptive name
### let's try data_plots_lesson1

# now let's bring in our data
# the most common file type is csv
### so i'll show you generalizable points 

# to read in the data, we use a read function 

merged_data <- read_parquet(file = "merged_data.parquet", stringsAsFactors = TRUE)

# this shows a typical set of R syntax
# generally:
### variable name <- function name (function arguments)

# the function (read_parquet) has two pieces of information (arguments):
### 1. the file, which tells R where to look for it
### 2. stringsAsFactors, which tells R how to treat text data
### in this case, we're treating text as categorical data

# data in the file are read in R and stored in a variable called merged_data

# quality assurance: let's look at the data 
# we don't necessarily need to record all the commands that we try here
# so this can give us practice using the console 

# let's start with "head" which shows the first six rows of data
head(merged_data)
# and similarly "tail" shows the last six rows 
tail(merged_data)

# something to look for: <NA> 
# in R: <NA> has a special meaning: it indicates a missing value 
# we'll talk more about this, but keep in mind:
### missing data might require special handling 

# another useful function "str" which is structure
str(merged_data)
# the output of str shows the size of the data

# cleaning up: we see it, now what? 
# data do not usually come to us "clean" (unlike this dataset!)
# so you should always plan to spend time "cleaning"

# to remove rows of missing data, you use the "na.omit" function
# but, before we do that, let's look at the "Environment" tab of our workspace 
# this will tell us about our merged_data set 
### it will tell us the size of our data set 
### 9622 observations (rows) and 8414 variables (columns)

# now lets see if any rows have missing data 
merged_data <- na.omit(merged_data)
### WHAT HAPPENED HERE!? 
# it dropped all our data
# why?
# let's talk about missing data 
# and let's talk about why missing data isn't a problem ...

# let's get our data back 
merged_data <- read_parquet(file = "merged_data.parquet", stringsAsFactors = TRUE)
# okay, we have our 9622 observations (rows) and 8414 variables (columns) again

# let's talk about all these ### 
# you've noticed as I'm going that there is this green text with #
# and then "regular" code
# good code isn't written for you, it is for someone else
# so you need to make sure you're telling a story 
# to do that, we add comments

# subsetting data
# this dataset has a lot of information 
# so to get familiar and build some skills, let's just look at a portion of it
# for now, let's just keep AZ
subset_data_az <- subset(merged_data, STATE_ABBR_2010SVI == "AZ") 
# within this, we can now see in "environment"
### 1526 obs and 8414 variables 

# above we have 
### 1. created a list of AZ, which is a state we're going to focus on
### 2. made a new variable called subset_az and stored the data for just AZ 

# something important to notice: we have not done anything to our original data
# remember: data are immutable: DO NOT EDIT YOUR ORIGINAL DATA 
# we will not have R write any data to our original files

# let's move on and do some basics
# you may be thinking, why are we doing commands, before we know what we're doing?
# what i am trying to do is to give you a basic foundation
# so that you have some grounding for when we talk about scientific computing

# summary 
# the summary function works on "data frames"
# data frames is how R stores spreadsheets
# this will give us a bit more information about each of the columns of data
# for columns with categorical data, this tells us how many rows are in each category 
# for columns with numerical data, it provides some statistics 

summary(subset_data_az)
# we have almost too many variables to be able to fully process this 
# what is something we could do, if we knew what we were interested in?

# let's look at summary statistics for groups of data
# consider upward mobility 
upward_mean_2010_az <- mean(subset_data_az$upward_mobility_rate_2010)
upward_sd_2010_az <- sd(subset_data_az$upward_mobility_rate_2010)
upward_mean_2020_az <- mean(subset_data_az$upward_mobility_rate_2020)
upward_sd_2020_az <- sd(subset_data_az$upward_mobility_rate_2020)
# remember, this is just looking at Arizona
# how do you think this would compare to the overall CA and AZ dataset? 
# let's pause and take a minute to code independently 
# i want you to
### 1. create a new subset with just CA, instead of just AZ
### 2. create the four variables we just made 

# 1. to get the CA subset
subset_data_ca <- subset(merged_data, STATE_ABBR_2010SVI == "CA") 
# 2. to get those summary statistics 
upward_mean_2010_ca <- mean(subset_data_ca$upward_mobility_rate_2010)
upward_sd_2010_ca <- sd(subset_data_ca$upward_mobility_rate_2010)
upward_mean_2020_ca <- mean(subset_data_ca$upward_mobility_rate_2020)
upward_sd_2020_ca <- sd(subset_data_ca$upward_mobility_rate_2020)

# pause and take a look at these
# what do you think? 

### END HERE ON DAY 1 ### 

# we can see these values alone by typing each variable name into the console
# say we wanted to delve into these statistics more deeply
# say for a county 
# we can get a data subset within the calls! 

pima_upward_mean_2010_az <- mean(subset_data_az$upward_mobility_rate_2010[subset_data_az$COUNTY_2010SVI == "Pima County"])
pima_upward_sd_2010_az <- sd(subset_data_az$upward_mobility_rate_2010[subset_data_az$COUNTY_2010SVI == "Pima County"])
# we could replicate this for multiple counties, states, etc.
# take a minute and practice for a county you're interested in!

delnorte_upward_mean_2010_ca <- mean(subset_data_ca$upward_mobility_rate_2010[subset_data_ca$COUNTY_2010SVI == "Del Norte County"])
delnorte_upward_sd_2010_ca <- sd(subset_data_ca$upward_mobility_rate_2010[subset_data_ca$COUNTY_2010SVI == "Del Norte County"])

# a few things to note here
### R is case-sensitive 
### this means, i would tend to put everything in lower-case
### but, be aware! 
### R autofills, which is helpful - did you notice that with the county variable?

# try and do something with incorrect cases 
pima_upward_mean_2010_az_trial <- mean(subset_data_az$upward_mobility_rate_2010[subset_data_az$county_2010svi == "Pima County"])
# now, it is tricky becuase the command WILL run!
# but, try and see the value
# you get NaN, which means "Not a Number"

# what if you wanted summary statistics for each county in all the states?
# you could copy and paste this code for all of them!
# there are 15 in AZ, so that feels doable!
# but there are then 58 in CA, which feels a lot more difficult! 
# thankfully, there are ways to make this easier! 

# so let's talk about packages! 
# packages are software in R that are designed to make operations, functions easier
# most of them are "third party"
# "third party" designates that someone else other than the authors of the original software wrote it
# R is especially amenable to third-party development - and you can decide if you like that or not! 
# when we use third-party R packages, we need to take two steps:
### 1. install the package
### 2. load the package's functions into memory 
# 1. only has to happen once, but 2. has to happen every time you use R

# we are going to use the dplyr package for summarizing 
# let's start with installation 
install.packages("dplyr")
# when we have installed it, we can load it into memory, with the library command
# when using third-party packages, we generally add library commands to the start of the script 
# this means that anyone who is going to run the code knows what they'll need to run it
# you'll remember that you saw this at the top already! you can add that to your code, too
library("dplyr")
# why is this code in RED?
# it makes me think of an error ... but here we are ... 

# we can now use dplyr functions to get summary statistics for each county
# here we use "group_by" function to essentially create a separate pile of data for each county
# and then for each pile, we calculate the mean and sd 

summary_stats_upward_2010 <- subset_data_az %>%
  group_by(COUNTY_2010SVI) %>%
  summarize(mean_mobility_2010_az = mean(upward_mobility_rate_2010),
            sd_mobility_2010_az = sd(upward_mobility_rate_2010))

# if we think about writing this in english 
# think about replacing %>% with the word "then"
# summary_stats_upward_2010 <- subset_data_az then
#   group_by(COUNTY_2010SVI) then
#   summarize(mean_mobility_2010_az = mean(upward_mobility_rate_2010),
#             sd_mobility_2010_az = sd(upward_mobility_rate_2010))
# or, if we just wanted to use words
# make a new variable called "summary_stats_upward_2010",
# take the subset_data from Arizona, then
# make a separate group of data for each county, then
# calculate the mean and standard deviation of the upward mobility figures 
# store them in columns called "mean_mobility_2010_az" and "sd_mobility_2010_az"
# best practices will have you explaining this also in comments 

# let's look at our summary stats 
# use the console 
# now we can see the mean and sd for every county in AZ!

head(summary_stats_upward_2010)
# remember head shows the first six rows of data 

# next let's try some visualization 
# when we use data to tell a story, we use visualization to make a point
# in this case, we're interested in looking at how upward mobility varies by county

# the first rule of plots
# when we visualize data, it is almost best to draw it out by hand
# this makes it easy to evaluate different graphical representations of data
# so, to start, let's consider the data we have (yes, it is a lot but consider...)
# how could we visualize this data? 
# draw it out!

# let's introduce another new package: ggplot2
# rembember: (1) install then (2) call
install.packages("ggplot2")
library("ggplot2")

# now le'ts make a boxplot 
# these are good for comparing values of different categories 
mobility_plot <- ggplot(data = summary_stats_upward_2010,
                        mapping = aes(x= COUNTY_2010SVI, y=mean_mobility_2010_az)) +
  geom_boxplot()
print(mobility_plot)
# even with just 15 counties, this looks a bit cluttered ... 
# now, let's think: this is a single value for each of thes
# is this the plot we want to make? 
# let's go back a bit
# can you make the same summary statistics we did above?
# BUT, looking at STATE AND COUNTY?
# take a few minutes and we'll give it a go together 

summary_stats_upward_2010_all <- merged_data %>%
  group_by(STATE_ABBR_2010SVI, COUNTY_2010SVI) %>%
  summarize(mean_mobility_2010 = mean(upward_mobility_rate_2010),
            sd_mobility_2010 = sd(upward_mobility_rate_2010))
# we've redo that summary statistics, looking across state and county 

# now, with that new summary statistics, let's replot it 
mobility_plot <- ggplot(data = summary_stats_upward_2010_all,
                        mapping = aes(x= STATE_ABBR_2010SVI, y=mean_mobility_2010)) +
  geom_boxplot()
print(mobility_plot)
# and replotted to demonstrate this

# sometimes, you might *think* that something is interesting and what you want to look at
# only to find that you need a different lens
# being flexible is more important than being "right" (whatever right is, anyway)

# let's break down the code for graphing
# ggplot is the main function for creating graphics in the ggplot2 package
# we give it two pieces of information:
### 1. data, tells R which data to use
### 2. mapping, which tells R which variable to put on which axis 
# we also have to tell R how to plot the data, which is a boxplot in our case
# then we tell R to print the graph, with the print command
# we also have it stored in a variable 

mobility_plot <- ggplot(data = summary_stats_upward_2010_all,
                        mapping = aes(x= STATE_ABBR_2010SVI, y=mean_mobility_2010,
                                      fill = STATE_ABBR_2010SVI)) +
  geom_boxplot()
print(mobility_plot)

# why is there an NA here?
# i'm not so certain about that - but let's omit it
summary_stats_upward_2010_all <- summary_stats_upward_2010_all[!(summary_stats_upward_2010_all$STATE_ABBR_2010SVI %in% c(NA)),]

mobility_plot <- ggplot(data = summary_stats_upward_2010_all,
                        mapping = aes(x= STATE_ABBR_2010SVI, y=mean_mobility_2010,
                                      fill = STATE_ABBR_2010SVI)) +
  geom_boxplot()
print(mobility_plot)

# finally we can save it 
# we use a ggsave function for this

ggsave(plot = mobility_plot, filename = "output/mobility-plot-2010.pdf")
# this saves the file in the "output" folder with the name mobility plot 2010

# YOU DID IT! 
# let's review our Lesson 1 learning objectives 
### 1. understand the difference between files and objects
### 2. modify data with data hygiene
### 3. summarize information from raw data
### 4. visualize data to convey information 

# in our next lesson, we'll take these skills and move into the scientific computing space
# but, based on today's learning objectives, where do you want more time, energy, information?

### END ###
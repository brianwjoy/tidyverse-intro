## ---------------------------
##
## File Name: tidyverse-demo-script.r
## Author: Brian Joy
## Original Publish Date: December 4, 2025
##
## ---------------------------


# RStudio Orientation -----------------------------------------------------

# This file is a "script" it contains R commands that can be executed line by
# line using keyboard shortcut of 'CTRL' + 'Enter'. Script files generally will
# have a file extension on '.R'

# You can open an existing R script or create a new R script from within
# RStudio. By default, RStudio will display scripts in the top left panel. You
# can also type or paste commands directly into the RStudio Console. The console
# is displayed in the bottom left pane of RStudio.

# Generally, most work should be done within a script -- this is key to
# reproducibility and automation. However, the console can be useful for initial
# testing or debugging. You can recall previous commands in the console by
# pressing the up arrow.


# RStudio Keyboard Shortcuts ----------------------------------------------

# Insert Section: 'CTRL'+'Shift'+'R'

# Insert assignment operator ( <- ): 'ALT'+'-'

# Insert Pipe Operator ( %>% ):'CTRL'+'Shift'+'M'

# Comment/Uncomment Line (#): 'CTRL'+'Shift'+'C' 
### Beware of conflict with Zoom keyboard shortcut ###

# Display all keyboard shortcuts: 'ALT'+'Shift'+'K'



# Install and Load the Tidyverse ------------------------------------------


# If needed, install the tidyverse
# Hint: Remove the "#" in the line below if you need to install
# install.packages("tidyverse")

# Load the tidyverse during each session
library(tidyverse)



# Read in .csv ------------------------------------------------------------



# Load data from file path
df <- read_csv("C:/Users/YourUsernameHere/Downloads/Demo/fictitious_student_enrollment_panel.csv")


# Preview first 6 six rows of data
df %>% head()


# Explore the Data --------------------------------------------------------

## Which Enrollment Terms are included?

###### Without the Pipe Operator
distinct(df, EnrollmentTerm)


###### With the Pipe Operator
df %>% 
  distinct(EnrollmentTerm)



## How many students are enrolled in each term?

###### Use the filter() function
df %>% 
  filter(EnrollmentTerm == "Fall 2023") %>% 
  summarise(student_cnt = n_distinct(StudentID))

###### Use group_by() and summarise()
df %>% 
  group_by(EnrollmentTerm) %>% 
  summarise(student_cnt = n_distinct(StudentID))


# Create a New Variable ---------------------------------------------------

## Binary Fall Indicator
df <- df %>% 
  mutate(isFall = if_else(TermSeason == "Fall", "Y", "N"))

## Check results
df %>% 
  count(isFall)


# Data Visualization ------------------------------------------------------

# GPA and Retention Viz Version 1

df %>% 
  filter(isFall == "Y") %>% 
  ggplot(aes(x = EnrollmentTerm, y = GPA)) +
  geom_boxplot()

# GPA and Retention Viz Version 2
df %>% 
  filter(isFall == "Y") %>% 
  ggplot(aes(x = EnrollmentTerm, y = GPA, fill = RetainedNextFall)) +
  geom_boxplot()

# GPA and Retention Viz Version 3
df %>% 
  filter(isFall == "Y") %>% 
  ggplot(aes(x = EnrollmentTerm, y = GPA, fill = RetainedNextFall)) +
  geom_boxplot() +
  scale_fill_manual( values = c("gray50", "#3E31E4")) +
  lims(y = c(0,4)) +
  labs(
    title = "Average GPA by Retention Status",
    caption = "Source: Fictitious Data"
  )+
  theme_minimal() +
  theme(legend.position = "bottom")

# GPA and Retention Viz Version 4
df %>% 
  filter(isFall == "Y") %>% 
  ggplot(aes(x = EnrollmentTerm, y = GPA, fill = RetainedNextFall)) +
  geom_boxplot() +
  scale_fill_manual( values = c("gray50", "#3E31E4")) +
  lims(y = c(0,4)) +
  facet_wrap(vars(AcademicCollege)) +
  labs(
    title = "Average GPA by Retention Status",
    caption = "Source: Fictitious Data"
  )+
  theme_minimal() +
  theme(legend.position = "bottom")
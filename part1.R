#loading and preprocessing the data


# Load the plotting package `ggplot2` by Hadley Wickham.
# Try to install the package if it is not found.

if(!suppressMessages(require(ggplot2))){
  print('trying to install ggplot2')
  install.packages('ggplot2')
  if(suppressMessagesrequire(ggplot2)){
    print("ggplot2 installed and loaded")
  } else {
    stop("could not install ggplot2")
  }
}

# Define some options for knitr
knitr::opts_chunk$set(tidy=FALSE, fig.path='figures/')


activity = read.csv("activity.csv", stringsAsFactors = FALSE)

# Change class for the date variable
activity$date <- as.Date(activity$date)




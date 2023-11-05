library(tictoc)
library(tweedie)
library(ggplot2)
library(tidyverse)
library(doParallel)

printTicTocLog <- # Function from lecture to print results more nice
  function() {
    tic.log() %>%
      unlist %>%
      tibble(logvals = .) %>%
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      mutate(log = str_trim(log)) %>%
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }



tic.clearlog() # To clear log of so we have a clean running

# Task 1 - Run current script ---------------------------------------------


tic("Original")
source("scripts/task_1.R") 
toc(log = TRUE)
printTicTocLog() %>%
  knitr::kable()


# Task 2 - With parallel computing ----------------------------------------


tic("Parallel")
source("scripts/task_2.R")
toc(log = TRUE)
printTicTocLog() %>%
  knitr::kable()


# Task 3 - Change in MTweedietests ----------------------------------------


tic("Parallel inside MTweedieTests")
source("scripts/task_3.R")
toc(log = TRUE)
printTicTocLog() %>%
  knitr::kable()



# Results and reflections -------------------------------------------------

#  |Function type                 |Seconds |
#  |:-----------------------------|:-------|
#  |Original                      |37.42   |
#  |Parallel                      |37.62   |
#  |Parallel inside MTweedieTests |35.25   |

# The results shows that Parallel inside MTweedieTests is the quickest, but not
# with much. This might be because of other background processes that are running,
# which interferes with the results. it might also be because it is actual quicker.
# I only have a two core CPU, so the difference would not be that dramatic, since
# the time the algorithm saves by using two CPUs is almost neglected by the time
# it takes to set up and organize the multiple CPU use. I think this is also why
# Parallel is slower than the original.

tryCatch(expr = library(topicmodels), error = function(e) {} )
library(textmineR)
library(lsa)
library(philentropy)
library(matrixcalc)
library(doParallel)
library(scales)

library(gtools)
library(ggplot2)
library(gplots)
library(plotly) 

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

library(tidyr)
library(tidyverse)
library(tidytext)
library(dplyr)
library(data.table)
library(data.tree)

library(quanteda)
if (packageVersion("quanteda") != "1.4.3") {
  stop("\nThis project requires package `quanteda` version 1.4.3\nPlease install it using:\n\trequire(devtools)\n\tinstall_version(\"quanteda\", version = \"1.4.3\", repos = \"http://cran.us.r-project.org\")")
}
## TODO: use packageVersion("quanteda") to automatically check for the package
#version of quanteda, and stop on the wrong version with a message on how to
#install the correct version. 
# MAR: note that you need quanteda v.1.4.3 --
#install using the following: 
#   require(devtools) 
#   install_version("quanteda", version = "1.4.3", repos = "http://cran.us.r-project.org")

library(Rtsne)
library(cluster)
library(dbscan)


library(gridExtra)
library(stringr)
library(topicmodels)
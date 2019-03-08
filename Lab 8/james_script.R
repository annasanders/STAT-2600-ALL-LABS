library(tidyverse)
library(readxl)
suicides <- read.table(file= "master.xls", sep= "\t", header= TRUE)
view(suicides)

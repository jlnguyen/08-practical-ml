# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 3 | Course Project - Submission file generator
# Joe Nguyen | 21 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
# par(mfrow = c(1,2))

# Answers from Rmd file
predRfSub <- c("B", "A", "B", "A", "A", "E", "D", "B", "A", "A", "B", "C", "B", "A", "E", "E", "A", "B", "B", "B")

#######################################
## Store answers in individual files ## 
#######################################
answers <- predRfSub

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename <- paste0("problem_id_",i,".txt")
        write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
    }
}

if (!dir.exists("./cp_answers")) { dir.create("./cp_answers") }
setwd(file.path("./cp_answers"))
pml_write_files(answers)

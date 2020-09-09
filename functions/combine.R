


#################################################################
#######This script used to combine all .R script in temp folder## 
###########into one .R which could be later######################
#editted and genarated by render("Template.R") ##################
#################################################################
files<-list.files("temp/",pattern = ".R",full.names = T)

sink("results/Template.R")
for(i in 1:length(files)){
  current_file=readLines(files[i])
  cat(current_file,sep="\n")
}
sink()

do.call(unlink,list(list.files("temp/",pattern = ".R",full.names=TRUE)[-1]))

#athena_folder is expected to contain files parsed athena CSV files 
athena_folder='n:/athena'

library(tidyverse)
library(magrittr)


  
  concept     <-read.delim(file.path(athena_folder,'concept.csv'),as.is=T,quote = "")
  
  #save(concept,file = file.path(athena_folder,'concept.rda'))
       
  vocabulary  <-read.delim(file.path(athena_folder,'vocabulary.csv'),as.is=T,quote = "")
  

  relationship<-read.delim(file.path(athena_folder,'relationship.csv')     ,as.is=T,quote = "")
  
  #large files
  concept_relationship        <-read.delim(file.path(athena_folder,'concept_relationship.csv'),as.is=T,quote = "")
  concept_ancestor   <-read.delim(file.path(athena_folder,'concept_ancestor.csv') ,as.is=T,quote = "")
  
  
  
  
  rm(athena_folder)
  Sys.time()
  save.image(file = file.path(athena_folder,'athena.rda'))
  Sys.time()
  
  
  library(devtools)
  
  
  
  
  #
  # crel        <-read.delim('inst/extdata/concept_relationship.csv',as.is=T,quote = "")
  # relationship<-read.delim('inst/extdata/relationship.csv',as.is=T,quote = "")
  # cancestor   <-read.delim('inst/extdata/concept_ancestor.csv',as.is=T,quote = "")
  #vocabulary$VOCABULARY_ID
#   library(dplyr)
# 
#   #version of vocab
#   print(dplyr::filter(vocabulary,VOCABULARY_ID=='None'))
# 
# rm(athena_folder)
# rm(crel)
# rm(stats2)
# rm(relationship)
# rm(vocabulary)
# rm(cancestor)
# rm(tta)
# rm(ttb)
# rm(ttd)
# rm(cid)
# 
# 

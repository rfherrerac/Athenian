
#athena_folder is expected to contain files parsed athena CSV files 
athena_folder='o:/athena'

library(tidyverse)
library(magrittr)


  
  concept     <-read.delim(file.path(athena_folder,'concept.csv'),as.is=T,quote = "")
  
  
  #save(concept,file = file.path(athena_folder,'concept.rda'))
       
  vocabulary  <-read.delim(file.path(athena_folder,'vocabulary.csv'),as.is=T,quote = "")
  

  relationship<-read.delim(file.path(athena_folder,'relationship.csv')     ,as.is=T,quote = "")
  
  #large files
  concept_relationship        <-read.delim(file.path(athena_folder,'concept_relationship.csv'),as.is=T,quote = "")
  concept_ancestor   <-read.delim(file.path(athena_folder,'concept_ancestor.csv') ,as.is=T,quote = "")
  
  
  
  
  
  a<-Sys.time()
  save.image(file = file.path(athena_folder,'athena-big.rda'))
  print(Sys.time()-a)
  
#ignore code below------------
#make a smaller file without some vocab tables  

  #rm(concept_ancestor)
  
  # a<-Sys.time()
  # save.image(file = file.path(athena_folder,'athena-small.rda'))
  # save.image(file = file.path(athena_folder,'athena-medium.rda'))
  # print(Sys.time()-a)
  
  
  #rm(athena_folder)  
  
  
  #   #version of vocab
  #   print(dplyr::filter(vocabulary,VOCABULARY_ID=='None'))
  
  
  
  
  
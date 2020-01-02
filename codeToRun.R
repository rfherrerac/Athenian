#main script to load Athenian data and function 
#R package would be too large, so script resembling package is provided instead
#also script allows to start with localy available athena csv files

#data must be first prepared using the oneTimeDataPreparation.R script

library(tidyverse)
library(magrittr)

#folder is expected to contain files parsed athena CSV files 
athena_folder='n:/athena'

#using rda (like an R package)
start<-Sys.time()
writeLines('Reading prepared vocab data (may take 2-8 minutes)')
 load(file.path(athena_folder,'athena.rda'))
writeLines('Vocab data all loaded.')
print(Sys.time()-start)



#' @export
get_version <- function() {
  dplyr::filter(vocabulary,vocabulary_id =='None')$vocabulary_version
}




#' Search by source code
#'
#' Search concept table for a code
#'
#' @param search_string
#' @return data frame with the matches
#' @export
search_code <- function(search_string,vocabulary=NULL) {
  
  out<-dplyr::filter(concept, concept_code ==search_string)
  if (!is.null(vocabulary)) out <-out %>% filter(vocabulary_id %in% vocabulary)
  return(out)
}




#' @export
search_concept_id <- function(concept_id,vocabulary=NULL,full_output=FALSE) {
  out<-dplyr::filter(concept, concept_id %in% concept_id)
  out <-out %>% mutate(concept_name=stringr::str_sub(concept_name,1,35))
  if (!is.null(vocabulary)) out <-out %>% filter(vocabulary_id %in% vocabulary)
  return(select(out,concept_id,concept_name,vocabulary_id))
}


#' Search by string
#'
#' Search concept table
#'

#' @return data frame with the matches
#' @export
search_text <- function(search_string) {
  dplyr::filter(concept, stringr::str_detect(concept_name, search_string))
}


#' @export
search_relationships <- function(concept_id) {
  dplyr::left_join(dplyr::filter(concept_relationship, concept_id_1==concept_id),concept, by=c('concept_id_2'='concept_id'))
}






# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#' Search by source code
#'
#' Search concept table for a code
#'
#' @param search_string
#' @return data frame with the matches
#' @export
search_code <- function(search_string) {
  dplyr::filter(concept, concept_code ==search_string)
}

#' @export
get_version <- function() {
  dplyr::filter(vocabulary,vocabulary_id =='None')
}


#' @export
search_concept_id <- function(cid,vocabulary=NULL,full_output=FALSE) {
  out<-dplyr::filter(concept, concept_id %in% cid)
  out <-out %>% mutate(concept_name=stringr::str_sub(concept_name,1,35))
  if (!is.null(vocabulary)) out <-out %>% filter(vocabulary_id %in% vocabulary)
  if (full_output) return(out) else return(select(out,concept_id,concept_name,vocabulary_id))
}


#vocabulary=NULL,full_output=FALSE
# out <-out %>% mutate(concept_name=stringr::str_sub(concept_name,1,35))
# if (!is.null(vocabulary)) out <-out %>% filter(vocabulary_id %in% vocabulary)
# if (full_output) return(out) else return(select(out,concept_id,concept_name,vocabulary_id))


#' Search by string
#'
#' Search concept table
#'

#' @return data frame with the matches
#' @export
search_text <- function(search_string,vocabulary=NULL) {
  out<-dplyr::filter(concept, stringr::str_detect(concept_name, search_string))
  if (!is.null(vocabulary)) out <-out %>% filter(vocabulary_id %in% vocabulary)
  return(out)
}


#' @export
search_relationships <- function(concept_id,vocabulary=NULL,full_output=FALSE) {
  out<-dplyr::left_join(dplyr::filter(concept_relationship, concept_id_1 %in% concept_id),concept, by=c('concept_id_2'='concept_id'))
  out <-out %>% mutate(concept_name=stringr::str_sub(concept_name,1,35))
  #if (!is.null(vocabulary)) out <-out %>% filter(vocabulary_id %in% vocabulary)
  if (full_output) return(out) else return(select(out,concept_id_1,concept_id_2,relationship_id,concept_name,vocabulary_id))
}


#' list all concepts in a given class
#' @export
search_other <- function(vocabulary=NULL,class=NULL,full_output=FALSE) {
  out<-dplyr::filter(concept,concept_class_id %in% class)
  out <-out %>% mutate(concept_name=stringr::str_sub(concept_name,1,35))
  #if (!is.null(vocabulary)) out <-out %>% filter(vocabulary_id %in% vocabulary)
  if (full_output) return(out) else return(select(out,concept_id,concept_name,vocabulary_id))
}

#' search by invalid reason field
#' @export
search_by_invalid_reason <- function(invalid_reason_value,vocabulary=NULL,class=NULL,full_output=FALSE) {
  out<-dplyr::filter(concept,invalid_reason  %in% invalid_reason_value)
  out <-out %>% mutate(concept_name=stringr::str_sub(concept_name,1,35))
  if (!is.null(vocabulary)) out <-out %>% filter(vocabulary_id %in% vocabulary)
  if (full_output) return(out) else return(select(out,concept_id,concept_name,vocabulary_id))
}


search_atc4 <- function(atc4='C07A') {
  concept %<>% mutate(concept_name=tolower(concept_name))
  #all atc4 level
  atc4temp<-concept %>% filter(domain_id=='Drug') %>% filter(concept_class_id=='ATC 4th') %>% select(concept_id)
  
  ing<-concept %>% filter(domain_id=='Drug') %>% filter(concept_class_id=='Ingredient') %>% 
    filter(vocabulary_id=='RxNorm') #remove this condition to do non US ingredients as well
  
  
  ia4map<-concept_ancestor %>% 
    inner_join(atc4temp,by=c('ancestor_concept_id'='concept_id')) %>% 
    inner_join(select(ing,concept_id), by=c('descendant_concept_id'='concept_id'))
  ia4map %<>% left_join(sconcept,by=c('ancestor_concept_id'='concept_id')) %>% 
    left_join(sconcept,by=c('descendant_concept_id'='concept_id'))
  ia4map %<>% arrange(concept_code.x)
  
  out<-ia4map %>% filter(grepl(atc4,concept_code.x)) %>% arrange(concept_code.x)
  out
  
}


#' See overview of vocabularies (only valid concepts are counted!)
#'
#'
#' @return data frame 
#' @export
overview_vocab <- function() {
  
  out<-dplyr::count(concept %>% filter(invalid_reason==''),vocabulary_id)
  out<-dplyr::arrange(out,desc(n))
  out
}


#' See overview of vocabularies (only valid concepts are counted!)
#'
#'
#' @return data frame 
#' @export
overview_vocab_standard <- function() {
  
  out<-dplyr::count(concept %>% filter(invalid_reason==''),vocabulary_id,standard_concept) %>% arrange(desc(standard_concept))
  out %<>% spread(standard_concept,n)
  out<-dplyr::arrange(out,desc(S))
  out
}


#' See overview of classes (only valid concepts)
#' @export
overview_class <- function() {
  out<-dplyr::count(concept %>% filter(invalid_reason==''),concept_class_id)
  out<-dplyr::arrange(out,desc(n))
  out
} 

#' overview by domain 
#' @export
#' 
overview_domain <- function() {
  out<-dplyr::count(concept %>% filter(invalid_reason==''),domain_id)
  out<-dplyr::arrange(out,desc(n))
  out
} 


#' See overview of domains and vocabularies  (only valid concepts)
#' @export
overview_domain_and_vocab <- function() {
  out<-dplyr::count(concept %>% filter(invalid_reason==''),domain_id,vocabulary_id)
  out<-dplyr::arrange(out,domain_id,desc(n))
  out
} 



#' get ancestors
#' @param cid concept id (or a vector of them)
#' @return data frame with the ancestors
#' @export
get_ancestors <- function(cid,vocabulary=NULL,full_output=FALSE) {
  out<-dplyr::filter(concept_ancestor, descendant_concept_id %in% cid) %>% dplyr::filter(min_levels_of_separation >0)
  out %<>% left_join(concept,by=c('ancestor_concept_id'='concept_id')) %>% arrange(descendant_concept_id,vocabulary_id,min_levels_of_separation,max_levels_of_separation)
  #out <-out %>% mutate(concept_name=stringr::str_sub(concept_name,1,35))
  if (!is.null(vocabulary)) out <-out %>% filter(vocabulary_id %in% vocabulary)
  #names(out)
  if (full_output) return(out) else return(out %>% select(ancestor_concept_id,concept_name,vocabulary_id,min_levels_of_separation,max_levels_of_separation)%>% mutate(concept_name=stringr::str_sub(concept_name,1,35)))
}


#demostration of Athenian functions


source('codeToRun.R')
get_version()

library(tidyverse)
#print all rows for next two commands
options(tibble.print_max = Inf)

overview_class()
overview_vocab()

search_code('250.00')

search_text('itag')

search_text('itag',vocabulary='RxNorm') %>% View()
search_by_invalid_reason(invalid_reason_value = 'D')

#deprecated with no successor in LOINC and RxNorm
search_by_invalid_reason(invalid_reason_value = 'D',vocabulary = c('LOINC','RxNorm')) %>% View()

#using full output
search_by_invalid_reason(invalid_reason_value = 'D',vocabulary = c('LOINC','RxNorm'),full_output = TRUE) %>% View()

search_by_invalid_reason(invalid_reason_value = 'D',vocabulary = c('NDC'),full_output = TRUE) %>% View()

#deprecated with sucessor (upgraded)
search_by_invalid_reason(invalid_reason_value = 'U',vocabulary = 'LOINC',full_output = TRUE) %>% View()



#looking into deprecation by vocabulary_id by year
# first prepare a new column with end year
concept$valid_end_date %<>%  as.character
concept$end_year=str_sub(concept$valid_end_date,1,4)
aa<-search_by_invalid_reason(invalid_reason_value = 'U',full_output = TRUE) %>% count(vocabulary_id,end_year) 
aa %>% write_csv('local/upgraded.csv')

ab<-search_by_invalid_reason(invalid_reason_value = 'D',full_output = TRUE) %>% count(vocabulary_id,end_year) 
aa %>% write_csv('local/deprecated.csv')

#reused codes
search_by_invalid_reason(invalid_reason_value = 'R') %>% View()
#concept %>% group_by(invalid_reason) %>% count()


  
concept %>% as_tibble() %>% mutate(year=valid_end_date)
mtcars %>% mutate(a=1)

#extract which LOINC codes appear in mappings

#we need to find all MapsTorelationship that have target terms LOINC codes

#all realtionships
names(concept_relationship)
options(min.print = 100)
options(tibble.print_max = 500)
concept_relationship %>% count(relationship_id) 
aa<-concept_relationship %>% 
  filter(concept_id_1!=concept_id_2) %>% 
  filter(relationship_id=='Maps to') %>%
  left_join(concept,by=c('concept_id_2'='concept_id')) %>% 
  left_join(concept,by=c('concept_id_1'='concept_id')) 
  
names(aa)
ab<-aa %>% filter(vocabulary_id.y=='LOINC') 
#lipid panel, let's see the relationships
cid=2212095
search_relationships(cid)
#we see CPT4 - LOINC eq

ac<-concept_relationship %>% 
  #ignore replationships to self
  filter(concept_id_1!=concept_id_2) %>% 
  filter(relationship_id=='CPT4 - LOINC eq') %>%
  #get explenations of codes
  left_join(concept,by=c('concept_id_1'='concept_id')) %>% 
  left_join(concept,by=c('concept_id_2'='concept_id')) %>% 
  filter(standard_concept.y=='S' )
ac %>% write_csv('CPT4-LOINCeq.csv')


    



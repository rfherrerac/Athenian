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


    


# map NDCs to clinical drug (not branded drug)



#filter all relationships of NDCs  
cid=45077293
search_relationships(cid)
names(concept_relationship)
rxnorm <- concept %>% filter(vocabulary_id=='RxNorm')
mt<-concept_relationship %>% filter(relationship_id=='Maps to')
mt %<>% left_join(concept, by=c('concept_id_1'='concept_id'))
mt %<>% left_join(concept, by=c('concept_id_2'='concept_id'))

#map<-mt %>% inner_join(rxnorm %>% select(concept_id), by=c('concept_id_2'='concept_id'))
#map %<>% left_join(concept, by=c('concept_id_1'='concept_id'))
map <- mt %>% filter(vocabulary_id.x=='NDC' & concept_class_id.x=='11-digit NDC')
map %>% write_rds('o:/athena/ndc-map-a.rds')

#if branded drug, I must go further
cid=40171779
cid=1710316

search_relationships(cid,vocabulary = 'RxNorm',full_output = TRUE) %>% View()

#brand drug    #Tradename of   #clinical drug
bcmap <- concept_relationship %>% filter(relationship_id=='Tradename of') %>% rename(cdcid=concept_id_2)

names(mapp)
mapp<-map %>% left_join(bcmap,by=c('concept_id_2'='concept_id_1')) %>% left_join(concept %>% select(concept_id,concept_name),by=c('cdcid'='concept_id'))
names(mapp)
mappp<-mapp %>% mutate(target_cid=if_else(is.na(cdcid),concept_id_2,cdcid))
names(mappp)

mappp %>% write_csv('o:/athena/ndc-map-b-ignore.csv')
#mtcars %>%   mutate(new_col = if_else(mpg*cyl == 126.0, 2, 0)) %>%    head()
mappp %>% select(concept_code=concept_code.x,target_cid) %>% write_rds('o:/athena/ndc-map-b.rds')



#hieararchy for a concept


cid=45077293

cid<-search_code('02HV33Z')$concept_id #%>% unlist()
cid

#atc example
cid<-search_code('J05AF09')$concept_id #%>% unlist()



aa<-search_relationships(cid,full_output = TRUE) 
names(aa)
View(aa)
View(aa %>% filter(standard_concept=='C'))

#for icd10 pcs hieararchy the concepts are not marked C

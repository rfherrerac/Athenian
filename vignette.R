#demostration of Athenian functions

source('codeToRun.R')
get_version()


search_code('250.00')

search_text('itag')

search_text('itag',vocabulary='RxNorm') %>% View()

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


    



# Title: Creating clean and ready datasets for PhD Theses and Courses
# Purpose: clean datasets and create variables necessary for classification
# Authors: Livio Silva Muller
# Date: January 2023

#Setup

library(tidyverse) 
library(cld3)
library(readxl)

setwd("~/Documents/GitHub/IHEIData/data/")
phd_theses <- readRDS("phd_theses.rds") #dataset comes from iheid_scrapping.r
cc_2021 <- read_excel("Course Catalog 2021-22.xlsx")
cc_2022 <- read_excel("Course Catalog 2022-23.xlsx")

skimr::skim(phd_theses)

#Data cleaning

phd_theses$language<- detect_language(phd_theses$abstract) #detect language from abstract
phd_theses$language <- ifelse(phd_theses$language=="ig", detect_language(phd_theses$title), phd_theses$language) # package codes NAs as igbo...
phd_theses$language <- gsub("sr", "en", phd_theses$language) #all "sr" are english (manual check)

phd_theses <- phd_theses %>%
                mutate(year=replace_na(year,"2022"), # all missing years are actually 2022
                      language=replace_na(language,"en"), # all NAs are actually english
                      department= case_when( #harmonizing departments
                          program == "PhD in International History and Politics (2021-)" ~  "IPH",
                          program == "PhD in International History (2012-2020)" ~  "IPH",
                          program == "PhD in Anthropology and Sociology (2013-)" ~  "ANSO",
                          program == "PhD in International Relations/Political Science (2012-)" ~  "IRPS",
                          program == "PhD in International Relations (1996-2017)" ~  "IRPS",
                          program == "PhD in International Studies (2010-2016)" ~  "MINT",
                          program == "PhD in Development Studies (1999-2017)" ~  "MINT",
                          program == "PhD in International Economics (2013-)" ~  "IE",
                          program == "Economies and Institutions" ~  "IE",
                          program == "PhD in Development Economics (2014-)" ~  "IE",
                          program == "PhD in International Law (2012-)" ~  "IL",
                          program == "No specialisation (1928-2001)" ~  "IL"))%>%
              select(-year2)%>% #getting rid of useless variables
              drop_na(abstract)

phd_theses$abstract <- tolower(phd_theses$abstract)

cc_2022 <- cc_2022 %>% mutate(
              course_department=case_when(
                course_department=="Anthropology and Sociology" ~ "ANSO",
                course_department=="International Relations/Political Science" ~"IRPS",
                course_department=="Interdisciplinary"  ~"MINT",
                course_department =="International History and Politics" ~"IHP",
                course_department=="International Economics"~"IE",
                course_department=="International Law" ~"IL"),
              course_year=2022,
              course_term=case_when(
                course_term=="Autumn S1" ~ "Autumn",
                course_term=="Autumn S2" ~"Autumn",
                course_term=="Spring S1" ~"Spring",
                course_term=="Spring S2" ~"Spring"))%>%
            select(-...1, -course_faculty)

cc_2021 <- cc_2021 %>% mutate(
  course_department=case_when(
    course_department =="International History" ~"IHP",
    course_department=="International Economics"~"IE",
    course_department=="International Law" ~"IL",
    course_department=="ANSO" ~ "ANSO",
    course_department=="MINT"  ~"MINT",
    course_department=="IRPS" ~"IRPS"),
  course_year=2021)%>%
  drop_na(course_description)

course_catalog <- rbind(cc_2021, cc_2022)

course_catalog$course_language<- detect_language(course_catalog$course_description)
course_catalog <- course_catalog %>% mutate( course_language=replace_na(course_language, "en"))

#creating and running dictionaries

sus_na_en <- ("sustain| enviro| agric |rural development |clean technologies|
                   climate change| carbon emissions| greenhouse gases| natural disasters|
                   conservation| biodiversity| corporate responsibility| energy|
                   natural resources| water| human security| equity| social inequality|
                   social justice|  poverty| peace| urban| land| health ")

sus_br_en <- ("poverty| income distribution| wealth| distribution| socioeconomic|
                   agriculture| food| nutrition| hunger| health| well-being| mortality|
                   disease| pandemic| educat | inclusive| equitable| school| water| sanitation|
                   wastewater| drought| rivers| aquifers| wetlands| oceans| marine| hydro|
                   groundwater| energy| renewable| wind| solar| geothermal| hydroelectricity|
                   energy efficiency| electricity| battery| electric vehicles| inclusive growth|
                   green growth| sustainable development| labor| worker| wage| infrastructure|
                   innovation| industr | buildings| clean technolog | inequality| tax |
                   social justice| equity| cities| urban| resilien | rural| waste| recycl |
                   circular| climate change| greenhouse gas| environment | global warming| weather|
                   water| natural resource| carbon emissions| CO2 emissions| low carbon|
                   ocean| marine| water| pollut | conserv | fish| sea| forest| biodiversity|
                   ecology| pollut| conserv| land use| land| institution| justice| governance| 
                   peace| human rights| human security| development")

sus_na_fr <- ("durabl| enviro| agric | rural |
              développement | technologies propres| changement climatique| émissions de carbone|
              gaz à effet de serre| catastrophes naturelles| conservation|
              biodiversité| responsabilité des entreprises| énergie| ressources naturelles|
              eau| sécurité humaine| équité| inégalités sociales|
              justice sociale| pauvreté| paix| urbain| terre| santé")

sus_br_fr <- ("durabl|pauvreté| répartition des revenus| richesse| distribution|
                     socio-économique| agriculture| alimentation| nutrition|
                     faim| santé| bien-être être| mortalité| maladie| pandémie|
                     éducation| inclusive| équitable| école| eau| assainissement|
                     eaux usées| sécheresse| rivières| aquifères| zones humides|
                     océans| marine| hydroélectricité| eaux souterraines| énergie|
                     renouvelable| éolienne| solaire| géothermique| hydroélectricité|
                     efficacité énergétique| électricité| batterie| véhicules électriques|
                     croissance inclusive| croissance verte| développement durable| travail|
                     travailleur| salaire| infrastructure| innovation| industrie | bâtiments|
                     technologie propre | inégalité| fiscalité| justice sociale| équité| villes|
                     urbain| résilien | rural| déchets| recyclable | circulaire| changement climatique|
                     gaz à effet de serre| environnement| réchauffement climatique| météo| eau|
                     ressource naturelle| émissions de carbone| émissions de CO2| bas carbone| océan|
                     marin| eau| polluant | conservateur | poisson| mer| forêt| biodiversité| écologie|
                     polluant| conservateur| utilisation du sol| terre| institution| justice| gouvernance|
                     paix| droits de l'homme| sécurité humaine| développement")

gen_en <- ("gender|women|equality|girl|queer|female|femin|sex|race|colonial|ethnic|
           white|ability|diversity|class|inequality|intersectionality|inclusion|lgbt")
  
gen_fr <- ("genre|femme|égalité|fille|queer|femelle|femme|sexe|race|colonial|ethnique|.
           blanc|capacité|diversité|classe|inégalité|intersectionnalité|inclusion|lgbt")

phd_theses <- phd_theses %>%
                mutate(
                  sust_na =ifelse(phd_theses$language=="fr",
                                  stringr::str_count(phd_theses$abstract,sus_na_fr),
                                  stringr::str_count(phd_theses$abstract,sus_na_en)),
                  sust_br =ifelse(phd_theses$language=="fr",
                                  stringr::str_count(phd_theses$abstract,sus_br_fr),
                                  stringr::str_count(phd_theses$abstract,sus_br_en)),
                  gen_na=ifelse(phd_theses$language=="fr",
                                stringr::str_count(phd_theses$abstract,gen_fr),
                                stringr::str_count(phd_theses$abstract,gen_en)))

course_catalog <- course_catalog %>%
  mutate(
    sust_na =ifelse(course_catalog$course_language=="fr",
                    stringr::str_count(course_catalog$course_description,sus_na_fr),
                    stringr::str_count(course_catalog$course_description,sus_na_en)),
    sust_br =ifelse(course_catalog$course_language=="fr",
                    stringr::str_count(course_catalog$course_description,sus_br_fr),
                    stringr::str_count(course_catalog$course_description,sus_br_en)),
    gen_na=ifelse(course_catalog$course_language=="fr",
                  stringr::str_count(course_catalog$course_description,gen_fr),
                  stringr::str_count(course_catalog$course_description,gen_en)))

saveRDS(phd_theses, file = "phd_theses_clean.rds")
saveRDS(course_catalog, file = "course_catalog_clean.rds")

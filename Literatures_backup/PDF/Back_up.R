library(gsubf)
library(proto)
library(RSQLite)
library(readr)
library(tm)
library(SnowballC)
library(RWeka)
library(sqldf)
library(dplyr)
library(data.table)
library(ggmap)
library(RODBC)
library(XML)
library(rvest)
library(xml2)
library(openxlsx)
library(RPostgreSQL)
library(stringr)
library(data.table)
library(stringr)

rm(list=ls())

##Parameter_1 : Location - Country

##Set the working directory with the ubication of the files
setwd("C:/Users/Mauricio/Desktop/WETLANDS_EXTRATION/Documents")

## The documents should be prepared before previous analysis:
## Step 1: Convertion of the document from PDF to .txt by opening the PDF file in Microsoft WORD an then save it as .txt
## Step 2: Elimination of all whitespaces and tap-spaces by using the program Note++ and the following regular expression [\t\r\n\s]+

##Read the documents you want for analysis (Documents located in a specific folder in the computer)
files_names<-list.files(getwd()) ##The name of the documents should be in the form doc[#].txt e.g. doc1.txt
doc.list<-lapply(files_names,read_file)
N.docs<-length(doc.list)
##Construction of Corpus of documents
my.docs<-VectorSource(c(doc.list))
my.docs$names<-sub(".txt","",files_names)
mydocs<-Corpus(my.docs)
removeSpecialChars<-function(x) gsub("[^-a-zA-z0-9. ]","",x)
##Cleaning 1= Removing special characters
mydocs<-tm_map(mydocs,removeSpecialChars)
##Cleaning 2= Removing stopwords()
mydocs<-tm_map(mydocs,removeWords,stopwords('en'))
##Cleaning 3= Removing doble whitespaces between words
mydocs<-tm_map(mydocs,stripWhitespace)
##Remove information before the word "Abstract"
removeAbstract<-function(x) gsub("(.*?)A(?i)bstract(?-i)","",x)
mydocs<-tm_map(mydocs,removeAbstract)
##Remove information after the word "Conclusions"
removeConclusions<-function(x) sub("C(?i)onclusion(?-i)(.*)","",x)
mydocs<-tm_map(mydocs,removeConclusions)
##Structure number 1: Abstract - Introduction
removeIntroduction_down<-function(x) sub("I(?i)ntroduction(?-i)(.*)","",x)
str1<-tm_map(mydocs,removeIntroduction_down)
##Structure number 2: Introduction - Methods
removeIntroduction_up<-function(x) sub("(.*?)I(?i)ntroduction(?-i)","",x)
str2_fs1.1<-tm_map(mydocs,removeIntroduction_up)
removeMethods_down<-function(x) sub("M(?i)ethods(?-i)(.*)","",x)
str2_fs1.2<-tm_map(str2_fs1.1,removeMethods_down)
removeMethods_down<-function(x) sub("M(?i)aterial(?-i)(.*)","",x)  
str2<-tm_map(str2_fs1.2,removeMethods_down)
##Structure number 3: Method - Results
removeMethods_up<-function(x) sub("(.*?)M(?i)aterial(?-i)","",x)
str3_fs1.1<-tm_map(mydocs,removeMethods_up)
removeMethods_up<-function(x) sub("(.*?)M(?i)ethods(?-i)","",x)
str3_fs1.2<-tm_map(str3_fs1.1,removeMethods_up)
removeResults_down<-function(x) sub("R(?i)esults(?-i)(.*)","",x)
str3<-tm_map(str3_fs1.2,removeResults_down)
##Structure number 4: Results-Conclusions
removeResults_up<-function(x) sub("(.*?)R(?i)esults(?-i)","",x)
str4<-tm_map(mydocs,removeResults_up)
dataframe_docs<-data.frame(text=sapply(mydocs, as.character),stringsAsFactors = FALSE)
extract_str1<-data.frame(text=sapply(str1, as.character),stringsAsFactors = FALSE)
extract_str2<-data.frame(text=sapply(str2, as.character),stringsAsFactors = FALSE)
extract_str3<-data.frame(text=sapply(str3, as.character),stringsAsFactors = FALSE)
extract_str4<-data.frame(text=sapply(str4, as.character),stringsAsFactors = FALSE)
#Creation of VCorpus for N-gram analysis
df<-data.frame(doc_id=seq(1:N.docs),text=dataframe_docs$text,stringsAsFactors = F)
VCorpus_docs<-VCorpus(DataframeSource(df))
df_str1<-data.frame(doc_id=seq(1:N.docs),text=extract_str1$text,stringsAsFactors = F)
VCorpus_docs1<-VCorpus(DataframeSource(df_str1))
df_str2<-data.frame(doc_id=seq(1:N.docs),text=extract_str2$text,stringsAsFactors = F)
VCorpus_docs2<-VCorpus(DataframeSource(df_str2))
df_str3<-data.frame(doc_id=seq(1:N.docs),text=extract_str3$text,stringsAsFactors = F)
VCorpus_docs3<-VCorpus(DataframeSource(df_str3))
df_str4<-data.frame(doc_id=seq(1:N.docs),text=extract_str4$text,stringsAsFactors = F)
VCorpus_docs4<-VCorpus(DataframeSource(df_str4))

##Reading the dataset of countries in the world
setwd("C:/Users/Mauricio/Desktop//WETLANDS_EXTRATION/Documents_backup")
countries<-read.delim("GEODATASOURCE-COUNTRY.txt", header = TRUE, sep = "\t")
countries_names<-as.vector(countries[,4])
countries_codes<-as.vector(countries[,1])
##Extracting the number of countries in the dataset
l_countries<-length(countries_names)
country_code<- c(rep(NA,N.docs))
country<-c(rep(NA,N.docs))
##Coding for comparing match in the dataframe of the Term-Document-Matrix and the dataframe of countries names
  for (m in 1:N.docs){
      #Defining Tokanizer for Term-Document-Matrix = # of words in each row of the matrix
         Tokenizer<-function(x) NGramTokenizer(x,Weka_control(min=1,max=2))
         head(NGramTokenizer(VCorpus_docs3[m],Weka_control(min=1,max=2)))
         dtm3<-TermDocumentMatrix(VCorpus_docs3[m],control=list(tokenize=Tokenizer,tolower=FALSE))
         bio3<-as.data.frame(t(as.matrix(dtm3)))
         keywords3<-colnames(bio3)
         kw_country3<-as.data.frame(keywords3)
         colnames(kw_country3)[1]<-"keywords_country"
         
         Tokenizer<-function(x) NGramTokenizer(x,Weka_control(min=1,max=2))
         head(NGramTokenizer(VCorpus_docs1[m],Weka_control(min=1,max=2)))
         dtm1<-TermDocumentMatrix(VCorpus_docs1[m],control=list(tokenize=Tokenizer,tolower=FALSE))
         bio1<-as.data.frame(t(as.matrix(dtm1)))
         keywords1<-colnames(bio1)
         kw_country1<-as.data.frame(keywords1)
         colnames(kw_country1)[1]<-"keywords_country"
         
         kw_country<-rbind(kw_country3,kw_country1)
         
         l_keywords<-length(keywords1)+length(keywords3)
         extract_country<-as.data.frame(lapply(kw_country, function(x) lapply(countries_names,function(y) grepl(y,x))))
         for (i in 1:l_keywords){
             for (j in 1:l_countries){
                 if (extract_country[i,j]=="TRUE"){
                     country[m]<-countries_names[j]
                     country_code[m]<-countries_codes[j]
                   }
               }
          }
      }
rm(dtm,bio,keywords,kw_country,l_keywords,extract_country)
##Parameter_2 : Location - City
cities<-fread("GEODATASOURCE-CITIES-FREE.txt", header = TRUE, sep = "\t")
cities_data<-as.data.frame(cities)
MUNICIPALITY<-c(rep(NA,N.docs))
for (m in 1:N.docs){
  if (is.na(country_code[m])==FALSE){
    Tokenizer_city<-function(x) NGramTokenizer(x,Weka_control(min=1,max=4))
    head(NGramTokenizer(VCorpus_docs3[m],Weka_control(min=1,max=4)))
    dtm_city<-TermDocumentMatrix(VCorpus_docs3[m],control=list(tokenize=Tokenizer_city,tolower=FALSE))
    bio_city<-as.data.frame(t(as.matrix(dtm_city)))
    keywords_city<-colnames(bio_city)
    kw_city<-as.vector(keywords_city)
    l_keywords_city<-length(keywords_city)
    filtration_cities<-subset(cities_data,CC_FIPS==country_code[m])
    filtration_cities<-as.vector(filtration_cities[,2])
    l_cities<-length(filtration_cities)
    filtration_cities<-paste("^",filtration_cities,"$",sep="")
    extract_cities<-lapply(filtration_cities,function(x)grep(x,kw_city,value=TRUE))
    extract_cities<-extract_cities[lapply(extract_cities,length)>0]
    if ((NROW(extract_cities)==1)&(length(extract_cities[[1]])==1)){
      MUNICIPALITY[m]<-extract_cities[[1]][1]
    }
  }
rm(dtm_city,bio_city,keywords_city,kw_city,l_keywords_city,extract_cities,filtration_cities)  
}
##Parameter_3: Location - Coordinates
## API from Gogle maps for extracting coordinates based on the name of the city (MUNICIPALITY)
LONGITUDE<-c(rep(NA,N.docs))
LATITUDE<-c(rep(NA,N.docs))

for (m in 1:N.docs){
  
  if (is.na(MUNICIPALITY[m])=="FALSE"){    
    coordinates<-geocode(MUNICIPALITY[m])
    LONGITUDE[m]<-coordinates[1,1]
    LATITUDE[m]<-coordinates[1,2]
  }
  
  if (is.na(MUNICIPALITY[m])=="TRUE"){
    Tokenizer<-function(x) NGramTokenizer(x,Weka_control(min=1,max=1))
    head(NGramTokenizer(VCorpus_docs3[m],Weka_control(min=1,max=1)))
    dtm_coordinates<-TermDocumentMatrix(VCorpus_docs3[m],control=list(tokenize=Tokenizer,tolower=FALSE))
    bio_coordinates<-as.data.frame(t(as.matrix(dtm_coordinates)))
    keywords_coordinates<-colnames(bio_coordinates)
    kw_coordinates<-as.data.frame(keywords_coordinates)  
    
    longitud_filter_E<-as.vector(filter(kw_coordinates, grepl(pattern="^[0-9]{5}E",kw_coordinates$keywords_coordinates))[1,])
    longitud_filter_W<-as.vector(filter(kw_coordinates, grepl(pattern="^[0-9]{5}W",kw_coordinates$keywords_coordinates))[1,])
    latitud_filter_N<-as.vector(filter(kw_coordinates, grepl(pattern="^[0-9]{5}N",kw_coordinates$keywords_coordinates))[1,])
    latitud_filter_S<-as.vector(filter(kw_coordinates, grepl(pattern="^[0-9]{5}S",kw_coordinates$keywords_coordinates))[1,])
    
    if (is.na(longitud_filter_E[1])=="FALSE"){
    
    lon_number<-sub("(^[^0-9]*)(\\d+)([^0-9].*)", "\\2", longitud_filter_E[1])
    grade_longitud<-as.numeric(sub('([0-9]{2}).*', '\\1', lon_number))
    minutes_longitud<-as.numeric(sub(".*(\\d+{2}).*$", "\\1", lon_number))
    LONGITUDE[m]<-grade_longitud+minutes_longitud/60
    }
    
    if (is.na(latitud_filter_N[1])=="FALSE"){
     lat_number<-sub("(^[^0-9]*)(\\d+)([^0-9].*)", "\\2", latitud_filter_N[1])
     grade_latitud<-as.numeric(sub('([0-9]{2}).*', '\\1', lat_number))
     minutes_latitud<-as.numeric(sub(".*(\\d+{2}).*$", "\\1", lat_number))
     LATITUDE[m]<-grade_latitud+minutes_latitud/60
    }
   
    if (is.na(longitud_filter_W[1])=="FALSE"){
      
      lon_number<-sub("(^[^0-9]*)(\\d+)([^0-9].*)", "\\2", longitud_filter_W[1])
      grade_longitud<-as.numeric(sub('([0-9]{2}).*', '\\1', lon_number))
      minutes_longitud<-as.numeric(sub(".*(\\d+{2}).*$", "\\1", lon_number))
      LONGITUDE[m]<-(grade_longitud+minutes_longitud/60)*(-1)
    } 
    
    if (is.na(latitud_filter_S[1])=="FALSE"){
      lat_number<-sub("(^[^0-9]*)(\\d+)([^0-9].*)", "\\2", latitud_filter_S[1])
      grade_latitud<-as.numeric(sub('([0-9]{2}).*', '\\1', lat_number))
      minutes_latitud<-as.numeric(sub(".*(\\d+{2}).*$", "\\1", lat_number))
      LATITUDE[m]<-(grade_latitud+minutes_latitud/60)*-1
    }
  }
}

##Parameter_4: Type of wetland

matrix_keyword<-c("^VSSF$","^HSSF$","^VFCW$","^HFCW$","^(?i)vertical(?-i)$","^(?i)surface flow(?-i)$","^(?i)subsurface flow(?-i)$")
matrix_keyword_nregex<-c("VSSF","HSSF","VFCW","HFCW","vertical","surface flow","subsurface flow")
cluster<-c("VF","HSSF","VF","HF","VF","FWS","HSSF")

n.words<-length(matrix_keyword)
TYPE_WETLANDS<-vector(mode="character", length=N.docs)
variable_cluster<-vector(mode="character", length=10)
for (m in 1:N.docs){
  Tokanizer_typewetlands3<-function(x) NGramTokenizer(x,Weka_control(min=1,max=2))
  head(NGramTokenizer(VCorpus_docs3[m],Weka_control(min=1,max=2)))
  dtm_typewetlands3<-TermDocumentMatrix(VCorpus_docs3[m],control = list(tokenize=Tokanizer_typewetlands3,tolower=FALSE))
  bio_typewetlands3<-as.data.frame(t(as.matrix(dtm_typewetlands3)))
  keywords_typewetlands3<-colnames(bio_typewetlands3)
  kw_typewetlands3<-as.data.frame(keywords_typewetlands3)
  colnames(kw_typewetlands3)[1]<-"keywords_typewetlands"
  
  Tokanizer_typewetlands4<-function(x) NGramTokenizer(x,Weka_control(min=1,max=2))
  head(NGramTokenizer(VCorpus_docs4[m],Weka_control(min=1,max=2)))
  dtm_typewetlands4<-TermDocumentMatrix(VCorpus_docs4[m],control = list(tokenize=Tokanizer_typewetlands4,tolower=FALSE))
  bio_typewetlands4<-as.data.frame(t(as.matrix(dtm_typewetlands4)))
  keywords_typewetlands4<-colnames(bio_typewetlands4)
  kw_typewetlands4<-as.data.frame(keywords_typewetlands4)
  colnames(kw_typewetlands4)[1]<-"keywords_typewetlands"
  
  Tokanizer_typewetlands1<-function(x) NGramTokenizer(x,Weka_control(min=1,max=2))
  head(NGramTokenizer(VCorpus_docs1[m],Weka_control(min=1,max=2)))
  dtm_typewetlands1<-TermDocumentMatrix(VCorpus_docs1[m],control = list(tokenize=Tokanizer_typewetlands4,tolower=FALSE))
  bio_typewetlands1<-as.data.frame(t(as.matrix(dtm_typewetlands1)))
  keywords_typewetlands1<-colnames(bio_typewetlands1)
  kw_typewetlands1<-as.data.frame(keywords_typewetlands1)
  colnames(kw_typewetlands1)[1]<-"keywords_typewetlands"
  
  kw_typewetlands<-rbind(kw_typewetlands3,kw_typewetlands4,kw_typewetlands1)
  c<-0
  
    for(i in 1:n.words){
    length_filtered_terms_type<-nrow(filter(kw_typewetlands, grepl(pattern=matrix_keyword[i],kw_typewetlands$keywords_typewetlands)))
    if((length_filtered_terms_type>=1)&(TYPE_WETLANDS[m]!=""))
    {
      con<-0
      for (l in 1:c){
      if (variable_cluster[l]==cluster[i]){
      con<-1
      }
      }
      if (con==0){
        c<-c+1
        variable_cluster[c]<-cluster[i]
        TYPE_WETLANDS[m]<-paste0(TYPE_WETLANDS[m],"-",cluster[i])
      }

    }
    if((length_filtered_terms_type>=1)&(TYPE_WETLANDS[m]==""))
    {
      c<-c+1
      variable_cluster[c]<-cluster[i]
      TYPE_WETLANDS[m]<-cluster[i]
    }
  }
}

##Parameter_5: Area
##Matrix of words from the text based in the Tokanizer (minimum and maximum number of words)
AREA<-vector(mode="character", length=N.docs)
NLPBigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

for (m in 1:N.docs){
  dtm_area<-TermDocumentMatrix(VCorpus_docs1[m],control = list(tokenize=NLPBigramTokenizer))
  bio_area<-as.data.frame(t(as.matrix(dtm_area)))
  keywords_area<-colnames(bio_area)
  kw_area<-as.data.frame(keywords_area)
  filtered_terms_area1 <- filter(kw_area, grepl(pattern="[0-9]m2.$",kw_area$keywords_area))
  filtered_terms_area2 <- filter(kw_area, grepl(pattern="[0-9] m2.$",kw_area$keywords_area))
  filtered_terms_area3 <- filter(kw_area, grepl(pattern="[[0-9]m2$",kw_area$keywords_area))
  filtered_terms_area4 <- filter(kw_area, grepl(pattern="[0-9] m2$",kw_area$keywords_area))
  filtered_terms_area<-rbind(filtered_terms_area1,filtered_terms_area2,filtered_terms_area3,filtered_terms_area4)
  filtered_terms_area<-as.vector(filtered_terms_area$keywords_area)
  #Another filtering process for just extracting the numbers
  if (length(filtered_terms_area)>0){
    for(i in 1:length(filtered_terms_area)){
      if(AREA[m]!="")
      {
        filtered_terms_area[i]<-sub("m(.*)","",gsub(gsub("[0-9].*","",filtered_terms_area[i]),"",filtered_terms_area[i]))
        filtered_terms_area[i]<-gsub("[[:space:]]+","",filtered_terms_area[i])
        AREA[m]<-paste0(AREA[m],"-",filtered_terms_area[i])
      }
      if(AREA[m]=="")
      {
        filtered_terms_area[i]<-sub("m(.*)","",gsub(gsub("[0-9].*","",filtered_terms_area[i]),"",filtered_terms_area[i]))
        filtered_terms_area[i]<-gsub("[[:space:]]+","",filtered_terms_area[i])
        AREA[m]<-filtered_terms_area[i]
      }
    }  
  }
  
  if (length(filtered_terms_area)==0){

    dtm_area<-TermDocumentMatrix(VCorpus_docs3[m],control = list(tokenize=NLPBigramTokenizer))
    bio_area<-as.data.frame(t(as.matrix(dtm_area)))
    keywords_area<-colnames(bio_area)
    kw_area<-as.data.frame(keywords_area)
    filtered_terms_area1 <- filter(kw_area, grepl(pattern="[0-9]m2.$",kw_area$keywords_area))
    filtered_terms_area2 <- filter(kw_area, grepl(pattern="[0-9] m2.$",kw_area$keywords_area))
    filtered_terms_area3 <- filter(kw_area, grepl(pattern="[[0-9]m2$",kw_area$keywords_area))
    filtered_terms_area4 <- filter(kw_area, grepl(pattern="[0-9] m2$",kw_area$keywords_area))
    filtered_terms_area<-rbind(filtered_terms_area1,filtered_terms_area2,filtered_terms_area3,filtered_terms_area4)
    filtered_terms_area<-as.vector(filtered_terms_area$keywords_area)
    
    if (length(filtered_terms_area)!=0){

      for(i in 1:length(filtered_terms_area)){
        if(AREA[m]!="")
        {
          filtered_terms_area[i]<-sub("m(.*)","",gsub(gsub("[0-9].*","",filtered_terms_area[i]),"",filtered_terms_area[i]))
          filtered_terms_area[i]<-gsub("[[:space:]]+","",filtered_terms_area[i])
          AREA[m]<-paste0(AREA[m],"-",filtered_terms_area[i])
        }
        if(AREA[m]=="")
        {
          filtered_terms_area[i]<-sub("m(.*)","",gsub(gsub("[0-9].*","",filtered_terms_area[i]),"",filtered_terms_area[i]))
          filtered_terms_area[i]<-gsub("[[:space:]]+","",filtered_terms_area[i])
          AREA[m]<-filtered_terms_area[i]
        }
      }     
    }
  }
}

##Parameter_6: Type of wastewaster
TYPE_WASTEWATER<-c(rep(NA,N.docs))
matrix_keyword<-c("^(?i)domestic(?-i)$","^(?i)industrial(?-i)$","^(?i)urban runoff(?-i)$","^(?i)food processing industry(?-i)$","^(?i)agriculture(?-i)$","^(?i)agricultural(?-i)$","^(?i)eutrophic lake water(?-i)$","^(?i)Dariy milking parlor(?-i)$","^(?i)Dariy milking parlor(?-i)$","^(?i)Potato starch processing(?-i)$","^(?i)swine urine(?-i)$")
matrix_keyword_nregex<-c("domestic","industrial","urban runoff","food processing industry","agriculture","agricultural")
cluster<-c("MUNICIPAL","INDUSTRIAL","STORMWATER","INDUSTRIAL","AGRICULTURAL","AGRICULTURAL","INDUSTRIAL","INDUSTRIAL","INDUSTRIAL")
variable_cluster<-vector(mode="character", length=10)
n.words<-length(matrix_keyword)
for (m in 1:N.docs){
  Tokanizer_typewastewater<-function(x) NGramTokenizer(x,Weka_control(min=1,max=3))
  head(NGramTokenizer(VCorpus_docs3[m],Weka_control(min=1,max=3)))
  dtm_typewastewater<-TermDocumentMatrix(VCorpus_docs3[m],control = list(tokenize=Tokanizer_typewastewater,tolower=FALSE))
  bio_typewastewater<-as.data.frame(t(as.matrix(dtm_typewastewater)))
  keywords_typewastewater<-colnames(bio_typewastewater)
  kw_typewastewater<-as.data.frame(keywords_typewastewater)
  c<-0
  
  for(i in 1:n.words){
    length_filtered_terms_type<-nrow(filter(kw_typewastewater, grepl(pattern=paste0("(?i)",matrix_keyword[i],"(?-i)"),kw_typewastewater$keywords_typewastewater)))
    if((length_filtered_terms_type>=1)&(is.na(TYPE_WASTEWATER[m])=="FALSE"))
    {
      con<-0
        for (l in 1:c){
          if (variable_cluster[l]==cluster[i]){
            con<-1
          }
        }
   
      if (con==0){
        c<-c+1
        variable_cluster[c]<-cluster[i]
        TYPE_WASTEWATER[m]<-paste0(TYPE_WASTEWATER[m],"-",cluster[i])
      }
      
    }
    if((length_filtered_terms_type>=1)&(is.na(TYPE_WASTEWATER[m])=="TRUE"))
    {
      c<-c+1
      variable_cluster[c]<-cluster[i]
      TYPE_WASTEWATER[m]<-cluster[i]
    }
  }
}

##Parameter_7: Type of plants
setwd("C:/Users/Mauricio/Desktop/WETLANDS_EXTRATION/Documents_backup")
TYPE_PLANT<-vector(mode="character", length=N.docs)
matrix<-as.data.frame(read.xlsx("Type_plants.xlsx"))
matrix_keywords<-as.vector(matrix[,1])
variable_cluster<-vector(mode="character", length=10)
cluster<-as.vector(matrix[,2])
n.words<-length(matrix_keywords)

for (m in 1:N.docs){
  Tokanizer_typeplant<-function(x) NGramTokenizer(x,Weka_control(min=1,max=4))
  head(NGramTokenizer(VCorpus_docs3[m],Weka_control(min=1,max=4)))
  dtm_typeplant<-TermDocumentMatrix(VCorpus_docs3[m],control = list(tokenize=Tokanizer_typeplant))
  bio_typeplant<-as.data.frame(t(as.matrix(dtm_typeplant)))
  keywords_typeplant<-colnames(bio_typeplant)
  kw_typeplant<-as.data.frame(keywords_typeplant)
  c<-0
  
  for(i in 1:n.words){
    length_filtered_terms_type<-nrow(filter(kw_typeplant, grepl(pattern=paste0("(?i)",matrix_keywords[i],"(?-i)"),kw_typeplant$keywords_typeplant)))
    if((length_filtered_terms_type>=1)&(TYPE_PLANT[m]!=""))
    {
      con<-0
      for (l in 1:c){
        if (variable_cluster[l]==cluster[i]){
          con<-1
        }
      }
      if (con==0){
        c<-c+1
        variable_cluster[c]<-cluster[i]
        TYPE_PLANT[m]<-paste0(TYPE_PLANT[m],";",cluster[i])
      }
    }
    if((length_filtered_terms_type>=1)&(TYPE_PLANT[m]==""))
    {
      c<-c+1
      variable_cluster[c]<-cluster[i]
      TYPE_PLANT[m]<-cluster[i]
    }
  }
}


##Parameter_8: BOD5 influent and BOD5 efluent
##The following approach works for pre-reviwed articles, which information
##is available in HTML version e.g. ELSEVIER

BOD_inlist<-vector(mode="list",length=N.docs)
BOD_outlist<-vector(mode="list",length=N.docs)
BOD_removallist<-vector(mode="list",length=N.docs)
CITATION<-c(rep(NA,N.docs))
URL<-c(rep(NA,N.docs))
JOURN_NAME<-c(rep(NA,N.docs))
PUBLISHER<-c(rep(NA,N.docs))
LIT_TYPE<-c(rep(NA,N.docs))
YEAR<-c(rep(NA,N.docs))
TITLE<-vector(mode="character", length=N.docs)
setwd("C:/Users/Mauricio/Desktop/WETLANDS_EXTRATION/HTML_LINKS")
HTML_links<-read.xlsx("HTML_links.xlsx")
setwd("C:/Users/Mauricio/Desktop/WETLANDS_EXTRATION/Phantom/phantomjs-2.1.1-windows/bin")
HTML_names<-as.vector(HTML_links[,1])
HTML_url<-as.vector(HTML_links[,2])

for (m in 1:N.docs){
  
  LIT_TYPE[m]<-"PRE-REVIEW ARTICLE"
  
  for (j in 1:nrow(HTML_links)){
    if (my.docs$names[m]==HTML_names[j]){
      url<-HTML_url[j]
      URL[m]<-url
      name<-paste0("doc",m,".js")
      writeLines(sprintf("var page = require('webpage').create();
                         page.open('%s', function () {
                         console.log(page.content); //page source
                         phantom.exit();
                         });", url), con=name)
      name_phantom<-paste0("phantomjs doc",m,".js")
      name_html<-paste0("doc",m,".html")
      write(readLines(pipe(name_phantom, "r")), name_html)
      
      journal_elsevier<-grepl("sciencedirect",url,ignore.case=FALSE)
      journal_IWA<-grepl("iwaponline",url,ignore.case=FALSE)
      journal_researchgate<-grepl("researchgate",url,ignore.case=FALSE)
      journal_ncbi<-grepl("ncbi",url,ignore.case=FALSE)
      
      page_html<- read_html(name_html)
      table<-page_html %>% html_nodes("table") %>% html_table(fill=TRUE)
      head_lines<-page_html %>% html_nodes("span")%>%html_text()
      
      if (journal_elsevier=="TRUE"){
        title_article<-page_html %>% html_nodes("span.title-text")%>%html_text()
        TITLE[m]<-title_article
        surname<-page_html %>% html_nodes("span.text.surname")%>%html_text()
        name<-page_html %>% html_nodes("span.text.given-name")%>%html_text()
        doi<-page_html %>% html_nodes("a.doi")%>%html_text()
        doi<-sub("(.*?)doi.org","doi:",doi)
        journal_name<-page_html %>% html_nodes("h2#publication-title.publication-title")%>%html_text()
        publication_year<-as.data.frame(page_html %>% html_nodes("div.text-xs")%>%html_text())
        colnames(publication_year)[1]<-"YEAR"
        publication_year<-as.vector(filter(publication_year,grepl(pattern="(?i)volume(?-i)",publication_year$YEAR))[,1])
        name<-sub("Ã¤","ä",name)
        name<-sub("Ãµ","õ",name)
        name<-sub("Ã\u009c","Ü",name)
        surname<-sub("Ã\u0096Ã¶","Öö",surname)
        
        for (z in 1:length(surname)){
          
          if (z==1){
            authors<-paste0(surname[1]," ",name[1],",")
          }
          
          if ((z!=1)&(z!=length(surname))){
            authors<-paste0(authors,name[z]," ",surname[z],",")
          }
          
          if(z==length(surname)){
            
            authors<-paste0(authors,"and ",name[z]," ",surname[z],".")
            
          }
        }
        PUBLISHER[m]<-authors
        publication_year[1]<-sub("â\u0080\u0093","-",publication_year[1])
        part1<-sub("(.*?)[,]","",publication_year[1])
        part2<-sub("(.*?)[,] ","",part1)
        part3<-sub("[,](.*)","",part2)
        YEAR[m]<-str_sub(part3, start= -4)
        JOURN_NAME[m]<-journal_name
        CITATION[m]<-paste0(authors,'"',title_article,'"',".",journal_name," ",publication_year[1],".",doi) 
      }
      
      if (journal_IWA=="TRUE"){
        
        title_article<-page_html %>% html_nodes("h1#page-title.highwire-cite-title")%>%html_text()
        TITLE[m]<-title_article
        journal_name<-page_html %>% html_nodes("div.region-inner.region-branding-inner")%>%html_text()
        JOURN_NAME[m]<-journal_name
        name<-page_html %>% html_nodes("span.highwire-citation-authors")%>%html_text()
        PUBLISHER[m]<-name[1]
        doi_publicationyear<-page_html %>% html_nodes("div.highwire-cite-metadata")%>%html_text()
        CITATION[m]<-paste0(name[1],".",'"',title_article,'"',".Water Sci Technol.",doi_publicationyear[1])
        
      }
      
      if (journal_ncbi=="TRUE"){
        
        journal_name<-page_html %>% html_nodes("h1")%>%html_text()
        TITLE[m]<-journal_name[2]
        name<-page_html %>% html_nodes("div.auths")%>%html_text()
        PUBLISHER[m]<-name
        publication_year<-page_html %>% html_nodes("div.cit")%>%html_text()
        JOURN_NAME[m]<-sub("[.](.*)","",publication_year[1])
        part1<-sub("(.*?)[.] ","",publication_year[1])
        YEAR[m]<-sub("[;](.*)","",part1)
        PMID<-page_html %>% html_nodes("dd")%>%html_text()
        CITATION[m]<-paste0(name,'"',journal_name[2],'"',publication_year,"PMID: ",PMID[2])
        
      }
      
      head_tables<-vector(mode="character", length=20)
      a<-0
      
      for (i in 1:length(head_lines)){
        
        regex_head<-grepl("Table",head_lines[i],ignore.case=FALSE)
        
        if (regex_head=="TRUE"){
          a<-a+1
          head_tables[a]<-head_lines[i]
        }
      }
      
      head_tables<-head_tables[head_tables!=""]
      head_tables_clean<-vector(mode="character", length=20)
      c<-0
      for (i in 1:length(head_tables)){
        number<-length(unlist(str_extract_all(head_tables[i], "\\w+")))
        if (number>2){
          c<-c+1
          head_tables_clean[c]<-head_tables[i]
        }
      }
      head_tables_clean<-head_tables_clean[head_tables_clean!=""]
      
      BOD_IN<-vector(mode="integer", length=20)
      BOD_OUT<-vector(mode="integer", length=20)
      BOD_REMOVAL<-vector(mode="integer", length=20)
      sec_in<-0
      sec_out<-0
      sec_removal<-0
      
      if(length(table)>0){
        for(b in 1:length(table)){
          
          con5<-0
          con6<-0
          con2<-0
          con3<-0
          con4<-0
          
          regex_influent<-grepl("(?i)influent(?-i)",head_tables_clean[b],ignore.case=FALSE)|grepl("(?i)inflow(?-i)",head_tables_clean[b],ignore.case=FALSE)
          if (regex_influent==TRUE){
            con2<-1
          }
          
          regex_efluent<-grepl("(?i)efluent(?-i)",head_tables_clean[b],ignore.case=FALSE)|grepl("(?i)outflow(?-i)",head_tables_clean[b],ignore.case=FALSE)
          if (regex_efluent==TRUE){
            con3<-1
          }
          
          regex_removal<-grepl("(?i)removal(?i)",head_tables_clean[b],ignore.case=FALSE)
          if (regex_removal==TRUE){
            con4<-1
          }
          
          con<-0
          row<-nrow(table[[b]])
          col<-ncol(table[[b]])
          
          for(i in 1:row){
            for (j in 1:col){
              table[[b]][i,j]<-sub("(?i)Â(.*)(?-i)","",table[[b]][i,j])
              regex<-grepl("BOD",table[[b]][i,j],ignore.case=FALSE)|grepl("BOD",colnames(table[[b]])[j],ignore.case=FALSE)
              if (regex=="TRUE"){
                con<-con+1
              }
            }
          }
          
          table_numeric<-data.frame(table[[b]],stringsAsFactors=FALSE)
          table_numeric<- as.data.frame(sapply(table_numeric, as.numeric))
          
          elimination_colum<-vector(mode="integer", length=10)
          elimination_row<-vector(mode="integer", length=10)
          
          if (con>=1){
            
            for (j in 1:col){
              
              regex_deviation<-grepl("(?i)deviation(?-i)",table[[b]][1,j],ignore.case=FALSE)
              regex_deviation_names<-grepl("(?i)deviation(?-i)",colnames(table[[b]][j]),ignore.case=FALSE)
              
              if((regex_deviation==TRUE)|(regex_deviation_names==TRUE)){
                con5<-con5+1
                elimination_colum[con5]<-j
              }
            }
            
            for (i in 1:row){
              
              regex_deviation<-grepl("(?i)deviation(?-i)",table[[b]][i,1],ignore.case=FALSE)
              
              if(regex_deviation==TRUE){
                con6<-con6+1
                elimination_row[con6]<-i
              }
            }
            
            elimination_colum<-elimination_colum[elimination_colum!=0]  
            elimination_row<-elimination_row[elimination_row!=0]
            
            if ((length(elimination_colum)!=0)&(length(elimination_row)!=0)){
              table[[b]]<-table[[b]][-elimination_row,-elimination_colum]
            }
            if ((length(elimination_colum)==0)&(length(elimination_row)!=0)){
              table[[b]]<-table[[b]][-elimination_row,]
            }
            if ((length(elimination_colum)!=0)&(length(elimination_row)==0)){
              table[[b]]<-table[[b]][,-elimination_colum]
            }
          }
          
          row<-nrow(table[[b]])
          col<-ncol(table[[b]])
          
          if ((con2>=1)&(con>=1)){
            
            for (i in 1:2){
              for (j in 1:col){
                
                regex_in<-grepl("BOD",table[[b]][i,j],ignore.case=FALSE)
                
                k<-i+1
                if(k<=row){
                  if ((regex_in=="TRUE")&(is.numeric(table_numeric[k,j])==TRUE)&(is.na(table_numeric[k,j])==FALSE)){
                    p<-i+1
                    for (u in p:row){
                      sec_in<-sec_in+1
                      BOD_IN[sec_in]<-table[[b]][u,j]
                    } 
                  }
                }
              }
            }
            
            if (BOD_IN[1]==0){
              
              for (j in 1:col){
                
                regex_in<-grepl("BOD",colnames(table[[b]])[j],ignore.case=FALSE)&grepl("(?i)in(?-i)",table[[b]][1,j],ignore.case=FALSE)
                
                if (regex_in=="TRUE"){
                  
                  for (i in 2:row){
                    
                    sec_in<-sec_in+1
                    BOD_IN[sec_in]<-table[[b]][i,j]
                    
                  }
                }
              }
            }
            
            for (i in 1:row) {
              
              regex_in<-grepl("BOD",table[[b]][i,1],ignore.case=FALSE)
              
              if ((regex_in=="TRUE")&(is.numeric(table_numeric[i,2])==TRUE)&(is.na(table_numeric[k,j])==FALSE)){
                for (u in 2:col){
                  sec_in<-sec_in+1
                  BOD_IN[sec_in]<-table[[b]][i,u]
                } 
              }  
            }
            
            BOD_IN<-BOD_IN[BOD_IN!=0]
            BOD_inlist[[m]]<-BOD_IN
            
          } 
          
          
          if ((con3>=1)&(con>=1)){
            
            for (i in 1:2){
              for (j in 1:col){
                
                regex_out<-grepl("BOD",table[[b]][i,j],ignore.case=FALSE)
                k<-i+1
                if(k<=row){
                  if ((regex_out=="TRUE")&(is.numeric(table_numeric[k,j])==TRUE)&(is.na(table_numeric[k,j])==FALSE)){
                    p<-i+1
                    for (u in p:row){
                      sec_out<-sec_out+1
                      BOD_OUT[sec_out]<-table[[b]][u,j]
                    } 
                  }
                }
              }
            }
            
            if (BOD_OUT[1]==0){
              
              for (j in 1:col){
                
                regex_out<-grepl("BOD",colnames(table[[b]])[j],ignore.case=FALSE)&grepl("(?i)out(?-i)",table[[b]][1,j],ignore.case=FALSE)
                
                if (regex_out=="TRUE"){
                  
                  for (i in 2:row){
                    
                    sec_out<-sec_out+1
                    BOD_OUT[sec_out]<-table[[b]][i,j]
                    
                  }
                }
              }
            }
            
            for (i in 1:row) {
              
              regex_out<-grepl("BOD",table[[b]][i,1],ignore.case=FALSE)
              
              if ((regex_out=="TRUE")&(is.numeric(table_numeric[i,2])==TRUE)&(is.na(table_numeric[k,j])==FALSE)){
                for (u in 2:col){
                  sec_out<-sec_out+1
                  BOD_OUT[sec_out]<-table[[b]][i,u]
                } 
              }  
            }
            BOD_OUT<-BOD_OUT[BOD_OUT!=0]
            BOD_outlist[[m]]<-BOD_OUT
          }   
          
          if ((con4>=1)&(con>=1)){
            
            
            sec<-0
            for (i in 1:2){
              for (j in 1:col){
                
                regex_removal<-grepl("BOD",table[[b]][i,j],ignore.case=FALSE)
                k<-i+1
                if(k<=row){
                  if ((regex_removal=="TRUE")&(is.numeric(table_numeric[k,j])==TRUE)&(is.na(table_numeric[k,j])==FALSE)){
                    p<-i+1
                    for (u in p:row){
                      sec_removal<-sec_removal+1
                      BOD_REMOVAL[sec_removal]<-table[[b]][u,j]
                    } 
                  }
                }
              }
            }
            
            for (i in 1:row) {
              
              regex_removal<-grepl("BOD",table[[b]][i,1],ignore.case=FALSE)
              
              if ((regex_removal=="TRUE")&(is.numeric(table_numeric[i,2])==TRUE)&(is.na(table_numeric[k,j])==FALSE)){
                for (u in 2:col){
                  sec_removal<-sec_removal+1
                  BOD_REMOVAL[sec_removal]<-table[[b]][i,u]
                } 
              }  
            }
            BOD_REMOVAL<-BOD_REMOVAL[BOD_REMOVAL!=0]
            BOD_removallist[[m]]<-BOD_REMOVAL
          }
        }
      }
    }
}
  }



for (i in 1:N.docs){

  if (length(BOD_inlist[[i]])==0){
  BOD_inlist[[i]]<-c(NA)
  }
  
  if (length(BOD_outlist[[i]])==0){
    BOD_outlist[[i]]<-c(NA)
  }
  
  if (length(BOD_removallist[[i]])==0){
    BOD_removallist[[i]]<-c(NA)
  }
  
  BOD_removallist[[i]]<-as.numeric(BOD_removallist[[i]])
  BOD_outlist[[i]]<-as.numeric(BOD_outlist[[i]])
  BOD_inlist[[i]]<-as.numeric(BOD_inlist[[i]])
}

##Extracting BOD values from other sources than tables
for (m in 1:N.docs){
 x<-c(NA)
 if (is.na(x)=="TRUE"){
   Tokanizer_BOD<-function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
   head(NGramTokenizer(VCorpus_docs1[m],Weka_control(min=3,max=3)))
   dtm_BOD<-TermDocumentMatrix(VCorpus_docs1[m],control = list(tokenize=Tokanizer_BOD,tolower=FALSE))
   bio_BOD<-as.data.frame(t(as.matrix(dtm_BOD)))
   keywords_BOD<-colnames(bio_BOD)
   kw_BOD<-as.data.frame(keywords_BOD)
   filter_BOD<-filter(kw_BOD, grepl(pattern="BOD$",kw_BOD$keywords_BOD))
 }
}

pw <- {
  "mauricio"
}

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = "postgres",
                 host = "localhost", port = 5432,
                 user = "openpg", password = pw)

con2=dbConnect(PostgreSQL(),user = "postgres",password="wetlands",dbname = "postgres")
SITES <- as.data.frame(dbGetQuery(con2, 'SELECT * from "SITES"'))
SITES$CTRY_CODE[is.na(SITES$CTRY_CODE)=="TRUE"]<-"NULL"
SITES$MUNI[is.na(SITES$MUNI)=="TRUE"]<-"NULL"
SITES$LONG[is.na(SITES$LONG)=="TRUE"]<-0
SYSTEMS<-as.data.frame(dbGetQuery(con2, 'SELECT * from "SYSTEMS"'))
C_PLANTS<-as.data.frame(dbGetQuery(con2, 'SELECT * from "C_PLANTS"'))
C_PLANTS$PLANT_SPEC[is.na(C_PLANTS$PLANT_SPEC)=="TRUE"]<-"NULL"
LITERATURE<-as.data.frame(dbGetQuery(con2, 'SELECT * from "LITERATURE"'))
LITERATURE$TITLE[is.na(LITERATURE$TITLE)=="TRUE"]<-"NULL"
C_ORG<-as.data.frame(dbGetQuery(con2, 'SELECT * from "C_ORG"'))
CELLS<-as.data.frame(dbGetQuery(con2, 'SELECT * from "CELLS"'))
COUNTRY<-as.data.frame(dbGetQuery(con2, 'SELECT * from "COUNTRY"'))
LITERATURE_comparison<-data.frame(LIT_ID=LITERATURE$LIT_ID,TITLE=LITERATURE$TITLE)
C_PLANTS_comparison<-data.frame(PLANT_ID=C_PLANTS$PLANT_ID,PLANT_SPEC=C_PLANTS$PLANT_SPEC)
SITES_comparison<-data.frame(SITE_ID=SITES$SITE_ID,CTRY_CODE=SITES$CTRY_CODE,MUNI=SITES$MUNI,LONG=SITES$LONG)

row_C_ORG<-nrow(C_ORG)
row_LITERATURE<-nrow(LITERATURE)
row_PLANT<-nrow(C_PLANTS)
row_SYSTEM<-nrow(SYSTEMS)
row_SYSTEM_seq<-row_SYSTEM+1
row_C_ORG_seq<-row_C_ORG+1
row_SITES<-nrow(SITES)

MUNICIPALITY[is.na(MUNICIPALITY)=="TRUE"]<-"NULL"
country_code[is.na(country_code)=="TRUE"]<-"NULL"
country[is.na(country)=="TRUE"]<-"NULL"
LONGITUDE[is.na(LONGITUDE)=="TRUE"]<-0
LATITUDE[is.na(LATITUDE)=="TRUE"]<-0
CITATION[is.na(CITATION)=="TRUE"]<-"NULL"
URL[is.na(URL)=="TRUE"]<-"NULL"
JOURN_NAME[is.na(JOURN_NAME)=="TRUE"]<-"NULL"
PUBLISHER[is.na(PUBLISHER)=="TRUE"]<-"NULL"
LIT_TYPE[is.na(LIT_TYPE)=="TRUE"]<-"NULL"
YEAR<-as.numeric(YEAR)
YEAR[is.na(YEAR)=="TRUE"]<-0
TITLE[is.na(TITLE)=="TRUE"]<-"NULL"
TYPE_PLANT[TYPE_PLANT==""]<-"NULL"
TYPE_WASTEWATER[is.na(TYPE_WASTEWATER)=="TRUE"]<-"NULL"
TYPE_WETLANDS[TYPE_WETLANDS==""]<-"NULL"
AREA[AREA==""]<-"NULL"



##Combine all lists in one dataframe for exporting to PostGreSQL

database <- data.table( DOCUMENT=character(),
                        CTRY_CODE=character(),
                        SYSTEM_ID=character(),
                        SITE_ID=character(),
                        LIT_ID=character(),
                        PLANT_ID=character(),
                        CELL_ID=character(),
                        CTRY_NAME=character(),
                        C_ORG_ID=character(),
                        MUNI=character(), 
                        LONG=numeric(),
                        LAT=numeric(),
                        LIT_TYPE=character(),
                        TITLE=character(),
                        YEAR=numeric(),
                        JOURN_NAME=character(),
                        PUBLISHER=character(),
                        URL=character(),
                        CELL_AREA=numeric(),
                        PLANT_SPEC=character(),
                        WW_TYPE=character(),
                        C_BOD_IN=numeric(),
                        C_BOD_OUT=numeric(),
                        CW_TYPE=character(),
                        stringsAsFactors=FALSE)

for (v in 1:N.docs){
  
  l1<-length(BOD_inlist[[v]])
  l2<-length(BOD_outlist[[v]])
  location<-c(l1,l2)
  max_location<-which(location == max(location), arr.ind = TRUE)
  max<-max(location)
  control<-0
  
  if(length(max_location)==2){
    
    LIT_ID<-vector(mode="character",length=max)
    lit_type<-rep(LIT_TYPE[v],max)
    url<-rep(URL[v],max)
    year<-rep(YEAR[v],max)
    journ_name<-rep(JOURN_NAME[v],max)
    publisher<-rep(PUBLISHER[v],max)
    Country<-rep(country[v],max)
    ctr_code<-rep(country_code[v],max)
    longitude<-rep(LONGITUDE[v],max)
    latitude<-rep(LATITUDE[v],max)
    typewetlands<-rep(TYPE_WETLANDS[v],max)
    typewaster<-rep(TYPE_WASTEWATER[v],max)
    typeplant<-rep(TYPE_PLANT[v],max)
    municipality<-rep(MUNICIPALITY[v],max)
    area<-rep(AREA[v],max)
    bod_outlist<-rep(BOD_outlist[[v]],max)
    bod_inlist<-rep(BOD_inlist[[v]],max)
    document<-rep(my.docs$names[v],max)
    SITE_ID<-vector(mode="character",length=max)
    PLANT_ID<-vector(mode="character",length=max)
    SYSTEM_ID<-vector(mode="character",length=max)
    C_ORG_ID<-vector(mode="character",length=max)
    CELL_ID<-vector(mode="character",length=max)
    row_SYSTEM_end<-row_SYSTEM+max
    cont<-0
    for (r in row_SYSTEM_seq:row_SYSTEM_end){
    cont<-cont+1
    SYSTEM_ID[cont]<-paste0("SYSTEM_",r)
    C_ORG_ID[cont]<-paste0("C_ORG_ID_",r)
    CELL_ID[cont]<-paste0("CELL_",r)
    }
    
    row_SYSTEM<-row_SYSTEM_end
    row_SYSTEM_seq<-row_SYSTEM+1
    title<-rep(TITLE[v],max)
 
    for (i in 1:max){
      
      row_SITES_comparison<-nrow(SITES_comparison)
      
      for (m in 1:row_SITES_comparison){
          
          if ((SITES_comparison$CTRY_CODE[m]==ctr_code[i])&(SITES_comparison$MUNI[m]==municipality[i])&(SITES_comparison$LONG[m]==longitude[i])){
            temporal_sites<-as.vector(SITES_comparison$SITE_ID)
            SITE_ID[i]<-temporal_sites[m]
        }
      }
        
        if (SITE_ID[i]==""){
          row_SITES_seq<-row_SITES_comparison+1
          SITE_ID[i]<-paste0("SITE_",row_SITES_seq)
          add_SITES<-data.frame(SITE_ID=SITE_ID[i],CTRY_CODE=ctr_code[i],MUNI=municipality[i],LONG=longitude[i])
          SITES_comparison<-rbind(SITES_comparison,add_SITES)
        }
      }
    
    
    for (i in 1:max){
      
      row_C_PLANTS_comparison<-nrow(C_PLANTS_comparison)
      
      for (m in 1:row_C_PLANTS_comparison){
        
          if (C_PLANTS_comparison$PLANT_SPEC[m]==typeplant[i]){
            temporal_plant<-as.vector(C_PLANTS_comparison$PLANT_ID)
            PLANT_ID[i]<-temporal_plant[m]
          }
        }
        
        if (PLANT_ID[i]==""){
          row_PLANT_seq<-row_C_PLANTS_comparison+1
          PLANT_ID[i]<-paste0("PLANT_",row_PLANT_seq)
          add_PLANT<-data.frame(PLANT_ID=PLANT_ID[i],PLANT_SPEC=typeplant[i])
          C_PLANTS_comparison<-rbind(C_PLANTS_comparison,add_PLANT)
        }
      }
    
    
    for (i in 1:max){
      
      row_LITERATURE_comparison<-nrow(LITERATURE_comparison)
      
      for (m in 1:row_LITERATURE_comparison){
        
        if (LITERATURE_comparison$TITLE[m]==title[i]){
          temporal_lit<-as.vector(LITERATURE_comparison$LIT_ID)
          LIT_ID[i]<-temporal_lit[m]
        }
      }
      
      if (LIT_ID[i]==""){
        row_LITERATURE_seq<-row_LITERATURE_comparison+1
        LIT_ID[i]<-paste0("LIT_",row_LITERATURE_seq)
        add_LITERATURE<-data.frame(LIT_ID=LIT_ID[i],TITLE=title[i])
        LITERATURE_comparison<-rbind(LITERATURE_comparison,add_LITERATURE)
      }
    }
    
    database_add<-data.table(DOCUMENT=document,
                             CTRY_NAME=Country,
                             MUNI=municipality,
                             LONG=longitude,
                             LAT=latitude,
                             CELL_AREA=area,
                             PLANT_SPEC=typeplant,
                             WW_TYPE=typewaster,
                             CTRY_CODE=ctr_code,
                             SYSTEM_ID=SYSTEM_ID,
                             SITE_ID=SITE_ID,
                             LIT_ID=LIT_ID,
                             PLANT_ID=PLANT_ID,
                             CELL_ID=CELL_ID,
                             LIT_TYPE=lit_type,
                             TITLE=title,
                             YEAR=year,
                             JOURN_NAME=journ_name,
                             PUBLISHER=publisher,
                             URL=url,
                             CW_TYPE=typewetlands,
                             C_ORG_ID=C_ORG_ID,
                             C_BOD_IN=BOD_inlist[[v]],
                             C_BOD_OUT=BOD_outlist[[v]])
    
    database<-rbind(database,database_add)
    control<-1
  }
  
  if ((max_location==1)&(control!=1)){
  year<-rep(YEAR[v],max)
  lit_type<-rep(LIT_TYPE[v],max)
  url<-rep(URL[v],max)
  journ_name<-rep(JOURN_NAME[v],max)
  publisher<-rep(PUBLISHER[v],max)
  Country<-rep(country[v],max)
  ctr_code<-rep(country_code[v],max)
  title<-rep(TITLE[v],max)
  longitude<-rep(LONGITUDE[v],max)
  latitude<-rep(LATITUDE[v],max)
  typewetlands<-rep(TYPE_WETLANDS[v],max)
  typewaster<-rep(TYPE_WASTEWATER[v],max)
  typeplant<-rep(TYPE_PLANT[v],max)
  municipality<-rep(MUNICIPALITY[v],max)
  area<-rep(AREA[v],max)
  document<-rep(my.docs$names[v],max)
  SITE_ID<-vector(mode="character",length=max)
  PLANT_ID<-vector(mode="character",length=max)
  LIT_ID<-vector(mode="character",length=max)
  bod_outlist<-rep(BOD_outlist[[v]],length.out=max)
  SYSTEM_ID<-vector(mode="character",length=max)
  C_ORG_ID<-vector(mode="character",length=max)
  CELL_ID<-vector(mode="character",length=max)
  row_SYSTEM_end<-row_SYSTEM+max
  cont<-0
  for (r in row_SYSTEM_seq:row_SYSTEM_end){
    cont<-cont+1
    SYSTEM_ID[cont]<-paste0("SYSTEM_",r)
    C_ORG_ID[cont]<-paste0("C_ORG_ID_",r)
    CELL_ID[cont]<-paste0("CELL_",r)
  }
  
  row_SYSTEM<-row_SYSTEM_end
  row_SYSTEM_seq<-row_SYSTEM+1
  
  for (i in 1:max){
    
    row_SITES_comparison<-nrow(SITES_comparison)
    
    for (m in 1:row_SITES_comparison){
      
      if ((SITES_comparison$CTRY_CODE[m]==ctr_code[i])&(SITES_comparison$MUNI[m]==municipality[i])&(SITES_comparison$LONG[m]==longitude[i])){
        temporal_sites<-as.vector(SITES_comparison$SITE_ID)
        SITE_ID[i]<-temporal_sites[m]
      }
    }
    
    if (SITE_ID[i]==""){
      row_SITES_seq<-row_SITES_comparison+1
      SITE_ID[i]<-paste0("SITE_",row_SITES_seq)
      add_SITES<-data.frame(SITE_ID=SITE_ID[i],CTRY_CODE=ctr_code[i],MUNI=municipality[i],LONG=longitude[i])
      SITES_comparison<-rbind(SITES_comparison,add_SITES)
    }
  }
  
  
  for (i in 1:max){
    
    row_C_PLANTS_comparison<-nrow(C_PLANTS_comparison)
    
    for (m in 1:row_C_PLANTS_comparison){
      
      if (C_PLANTS_comparison$PLANT_SPEC[m]==typeplant[i]){
        temporal_plant<-as.vector(C_PLANTS_comparison$PLANT_ID)
        PLANT_ID[i]<-temporal_plant[m]
      }
    }
    
    if (PLANT_ID[i]==""){
      row_PLANT_seq<-row_C_PLANTS_comparison+1
      PLANT_ID[i]<-paste0("PLANT_",row_PLANT_seq)
      add_PLANT<-data.frame(PLANT_ID=PLANT_ID[i],PLANT_SPEC=typeplant[i])
      C_PLANTS_comparison<-rbind(C_PLANTS_comparison,add_PLANT)
    }
  }
  
  
  for (i in 1:max){
    
    row_LITERATURE_comparison<-nrow(LITERATURE_comparison)
    
    for (m in 1:row_LITERATURE_comparison){
      
      if (LITERATURE_comparison$TITLE[m]==title[i]){
        temporal_lit<-as.vector(LITERATURE_comparison$LIT_ID)
        LIT_ID[i]<-temporal_lit[m]
      }
    }
    
    if (LIT_ID[i]==""){
      row_LITERATURE_seq<-row_LITERATURE_comparison+1
      LIT_ID[i]<-paste0("LIT_",row_LITERATURE_seq)
      add_LITERATURE<-data.frame(LIT_ID=LIT_ID[i],TITLE=title[i])
      LITERATURE_comparison<-rbind(LITERATURE_comparison,add_LITERATURE)
    }
  }
  
  database_add<-data.table(DOCUMENT=document,
                           CTRY_NAME=Country,
                           MUNI=municipality,
                           LONG=longitude,
                           LAT=latitude,
                           CELL_AREA=area,
                           PLANT_SPEC=typeplant,
                           WW_TYPE=typewaster,
                           CTRY_CODE=ctr_code,
                           SYSTEM_ID=SYSTEM_ID,
                           SITE_ID=SITE_ID,
                           LIT_ID=LIT_ID,
                           PLANT_ID=PLANT_ID,
                           CELL_ID=CELL_ID,
                           LIT_TYPE=lit_type,
                           TITLE=title,
                           YEAR=year,
                           JOURN_NAME=journ_name,
                           PUBLISHER=publisher,
                           URL=url,
                           CW_TYPE=typewetlands,
                           C_ORG_ID=C_ORG_ID,
                           C_BOD_IN=BOD_inlist[[v]],
                           C_BOD_OUT=bod_outlist)
  
  database<-rbind(database,database_add)
  }
  
  if ((max_location==2)&(control!=1)){
    year<-rep(YEAR[v],max)
    lit_type<-rep(LIT_TYPE[v],max)
    url<-rep(URL[v],max)
    journ_name<-rep(JOURN_NAME[v],max)
    publisher<-rep(PUBLISHER[v],max)
    Country<-rep(country[v],max)
    title<-rep(TITLE[v],max)
    ctr_code<-rep(country_code[v],max)
    longitude<-rep(LONGITUDE[v],max)
    latitude<-rep(LATITUDE[v],max)
    typewetlands<-rep(TYPE_WETLANDS[v],max)
    typewaster<-rep(TYPE_WASTEWATER[v],max)
    typeplant<-rep(TYPE_PLANT[v],max)
    municipality<-rep(MUNICIPALITY[v],max)
    area<-rep(AREA[v],max)
    document<-rep(my.docs$names[v],max)
    SITE_ID<-vector(mode="character",length=max)
    PLANT_ID<-vector(mode="character",length=max)
    LIT_ID<-vector(mode="character",length=max)
    bod_inlist<-rep(BOD_inlist[[v]],length.out=max)
    SYSTEM_ID<-vector(mode="character",length=max)
    C_ORG_ID<-vector(mode="character",length=max)
    CELL_ID<-vector(mode="character",length=max)
    row_SYSTEM_end<-row_SYSTEM+max
    cont<-0
    for (r in row_SYSTEM_seq:row_SYSTEM_end){
      cont<-cont+1
      SYSTEM_ID[cont]<-paste0("SYSTEM_",r)
      C_ORG_ID[cont]<-paste0("C_ORG_ID_",r)
      CELL_ID[cont]<-paste0("CELL_",r)
    }
    
    row_SYSTEM<-row_SYSTEM_end
    row_SYSTEM_seq<-row_SYSTEM+1
    
    for (i in 1:max){
      
      row_SITES_comparison<-nrow(SITES_comparison)
      
      for (m in 1:row_SITES_comparison){
        
        if ((SITES_comparison$CTRY_CODE[m]==ctr_code[i])&(SITES_comparison$MUNI[m]==municipality[i])&(SITES_comparison$LONG[m]==longitude[i])){
          temporal_sites<-as.vector(SITES_comparison$SITE_ID)
          SITE_ID[i]<-temporal_sites[m]
        }
      }
      
      if (SITE_ID[i]==""){
        row_SITES_seq<-row_SITES_comparison+1
        SITE_ID[i]<-paste0("SITE_",row_SITES_seq)
        add_SITES<-data.frame(SITE_ID=SITE_ID[i],CTRY_CODE=ctr_code[i],MUNI=municipality[i],LONG=longitude[i])
        SITES_comparison<-rbind(SITES_comparison,add_SITES)
      }
    }
    
    
    for (i in 1:max){
      
      row_C_PLANTS_comparison<-nrow(C_PLANTS_comparison)
      
      for (m in 1:row_C_PLANTS_comparison){
        
        if (C_PLANTS_comparison$PLANT_SPEC[m]==typeplant[i]){
          temporal_plant<-as.vector(C_PLANTS_comparison$PLANT_ID)
          PLANT_ID[i]<-temporal_plant[m]
        }
      }
      
      if (PLANT_ID[i]==""){
        row_PLANT_seq<-row_C_PLANTS_comparison+1
        PLANT_ID[i]<-paste0("PLANT_",row_PLANT_seq)
        add_PLANT<-data.frame(PLANT_ID=PLANT_ID[i],PLANT_SPEC=typeplant[i])
        C_PLANTS_comparison<-rbind(C_PLANTS_comparison,add_PLANT)
      }
    }
    
    
    for (i in 1:max){
      
      row_LITERATURE_comparison<-nrow(LITERATURE_comparison)
      
      for (m in 1:row_LITERATURE_comparison){
        
        if (LITERATURE_comparison$TITLE[m]==title[i]){
          temporal_lit<-as.vector(LITERATURE_comparison$LIT_ID)
          LIT_ID[i]<-temporal_lit[m]
        }
      }
      
      if (LIT_ID[i]==""){
        row_LITERATURE_seq<-row_LITERATURE_comparison+1
        LIT_ID[i]<-paste0("LIT_",row_LITERATURE_seq)
        add_LITERATURE<-data.frame(LIT_ID=LIT_ID[i],TITLE=title[i])
        LITERATURE_comparison<-rbind(LITERATURE_comparison,add_LITERATURE)
      }
    }
    database_add<-data.table(DOCUMENT=document,
                             CTRY_NAME=Country,
                             MUNI=municipality,
                             LONG=longitude,
                             LAT=latitude,
                             CELL_AREA=area,
                             PLANT_SPEC=typeplant,
                             WW_TYPE=typewaster,
                             CTRY_CODE=ctr_code,
                             SYSTEM_ID=SYSTEM_ID,
                             SITE_ID=SITE_ID,
                             LIT_ID=LIT_ID,
                             PLANT_ID=PLANT_ID,
                             CELL_ID=CELL_ID,
                             LIT_TYPE=lit_type,
                             TITLE=title,
                             YEAR=year,
                             JOURN_NAME=journ_name,
                             PUBLISHER=publisher,
                             URL=url,
                             CW_TYPE=typewetlands,
                             C_ORG_ID=C_ORG_ID,
                             C_BOD_IN=bod_inlist,
                             C_BOD_OUT=BOD_outlist[[v]])
    
    database<-rbind(database,database_add)
  }  
}

database[database=="NULL"]<-NA
database[database==0]<-NA
database[is.na(database$CTRY_CODE)==TRUE]<-"NULL"

##Creating tables for exporting information to PostGRE i.e. based in UNU-Flores relational tables framework
##Coding for the ID parameters
##SITE_ID: SITE_01,SITE_02,...
##SYSTEM_ID: SYSTEM_01,SYSTEM_02,...
##LIT_ID: LIT_01,LIT_02,...
##PLANT_ID: PLANT_01,PLANT_02,...
##CELL_ID: CELL_01,CELL_02,...
##C_ORG_ID: C_ORG_01,C_ORG_02,...

##1-SITES

SITES_R <- data.table(SITE_ID=database$SITE_ID,
                    SYSTEM_ID=database$SYSTEM_ID,
                    CTRY_CODE=database$CTRY_CODE,
                    SUBN_ADM_L=rep(NA,nrow(database)),
                    MUNI=database$MUNI,
                    LAT=database$LAT,
                    LONG=database$LONG,
                    ELEVATION=rep(NA,nrow(database)),
                    WW_TYPE=database$WW_TYPE,
                    HYDR_CATCH=rep(NA,nrow(database)),
                    TOTAL_PE=rep(NA,nrow(database)),
                    ULT_DISP_M=rep(NA,nrow(database)),
                    SW_CATCH_A=rep(NA,nrow(database)),
                    stringsAsFactors=FALSE)
start<-nrow(SITES)+1
SITES<-rbind(SITES,SITES_R)
SITES<-SITES[!duplicated(SITES$SITE_ID),]
SITES_up<-SITES
end<-nrow(SITES)
SITES<-SITES[start:end]
SITES_fk<-SITES
SITES_fk$SYSTEM_ID<-NA
SITES_fk$CTRY_CODE<-NA

dbWriteTable(con2, "SITES", 
             value = SITES_fk, append = TRUE, row.names = FALSE)




##2-COUNTRY

COUNTRY_R<-data.table(CTRY_CODE=database$CTRY_CODE,
                       CONTINENT=rep(NA,nrow(database)),
                       CTRY_NAME=database$CTRY_NAME,
                       HDI=rep(NA,nrow(database)),
                       HD_GROUP=rep(NA,nrow(database)),
                       LDC=rep(NA,nrow(database)),
                       LLDC=rep(NA,nrow(database)),
                       SIDS=rep(NA,nrow(database)),
                       stringsAsFactors=FALSE)
start<-nrow(COUNTRY)+1
COUNTRY<-rbind(COUNTRY,COUNTRY_R)
COUNTRY<-COUNTRY[!duplicated(COUNTRY$CTRY_CODE),]
end<-nrow(COUNTRY)
if (start<end){
COUNTRY<-COUNTRY[start:end]
dbWriteTable(con2, "COUNTRY", 
             value = COUNTRY, append = TRUE, row.names = FALSE)
}

##3-SYSTEMS

SYSTEMS_R <- data.table(SYSTEM_ID=database$SYSTEM_ID,
                        SITE_ID=database$SITE_ID,
                        SYS_NAME=rep(NA,nrow(database)),
                        SYS_AREA=rep(NA,nrow(database)),
                        SYS_SCALE=rep(NA,nrow(database)),
                        S_PRE_TRT=rep(NA,nrow(database)),
                        S_POST_TRT=rep(NA,nrow(database)),
                        DES_FLOW=rep(NA,nrow(database)),
                        DES_LIFE=rep(NA,nrow(database)),
                        START_DATE=rep(NA,nrow(database)),
                        END_DATE=rep(NA,nrow(database)),
                        INV_COST=rep(NA,nrow(database)),
                        NV_COST_C=rep(NA,nrow(database)),
                        AN_COST=rep(NA,nrow(database)),
                        AN_COST_C=rep(NA,nrow(database)),
                        stringsAsFactors=FALSE)

start<-nrow(SYSTEMS)+1
SYSTEMS<-rbind(SYSTEMS,SYSTEMS_R)
SYSTEMS<-SYSTEMS[!duplicated(SYSTEMS$SYSTEM_ID),]
end<-nrow(SYSTEMS)
SYSTEMS<-SYSTEMS[start:end]

dbWriteTable(con2, "SYSTEMS", 
             value = SYSTEMS, append = TRUE, row.names = FALSE)

##4-LITERATURE

LITERATURE_R<-data.table(LIT_ID=database$LIT_ID,
                         SITE_ID=database$SITE_ID,
                         SYSTEM_ID=database$SYSTEM_ID,
                         LIT_TYPE=database$LIT_TYPE,
                         TITLE=database$TITLE,
                         YEAR=database$YEAR,
                         JOURN_NAME=database$JOURN_NAME,
                         PUBLISHER=database$PUBLISHER,
                         URL=database$URL,
                         stringsAsFactors=FALSE)

start<-nrow(LITERATURE)+1
LITERATURE<-rbind(LITERATURE,LITERATURE_R)
LITERATURE<-LITERATURE[!duplicated(LITERATURE$LIT_ID),]
end<-nrow(LITERATURE)
LITERATURE<-LITERATURE[start:end]
dbWriteTable(con2, "LITERATURE", 
             value = LITERATURE, append = TRUE, row.names = FALSE)

##5-CELLS

CELLS_R<- data.table(CELL_ID=database$CELL_ID,
                       SYSTEM_ID=database$SYSTEM_ID,
                       C_PRE_TRT=rep(NA,nrow(database)),
                       C_POST_TRT=rep(NA,nrow(database)),
                       CW_TYPE=database$CW_TYPE,
                       CELL_SHAPE=rep(NA,nrow(database)),
                       LENGTH=rep(NA,nrow(database)),
                       WIDTH=rep(NA,nrow(database)),
                       DIAMETER=rep(NA,nrow(database)),
                       DEPTH=rep(NA,nrow(database)),
                       CELL_AREA=database$CELL_AREA,
                       CELL_VOL=rep(NA,nrow(database)),
                       INTENS_MTH=rep(NA,nrow(database)),
                       SUBST_TYP=rep(NA,nrow(database)),
                       SUBST_PORO=rep(NA,nrow(database)),
                       SSFGSZMIN=rep(NA,nrow(database)),
                       SSFGSZMAX=rep(NA,nrow(database)),
                       C_START_DT=rep(NA,nrow(database)),
                       C_END_DATE=rep(NA,nrow(database)),
                       FLO_REGIME=rep(NA,nrow(database)),
                       LD_PERIOD=rep(NA,nrow(database)),
                       DRY_PERIOD=rep(NA,nrow(database)),
                       stringsAsFactors=FALSE)

start<-nrow(CELLS)+1
CELLS<-rbind(CELLS,CELLS_R)
CELLS<-CELLS[!duplicated(CELLS$CELL_ID),]
end<-nrow(CELLS)
dbWriteTable(con2, "CELLS", 
             value = CELLS, append = TRUE, row.names = FALSE)

##6-C_PLANTS

C_PLANTS_R<-data.table(PLANT_ID=database$PLANT_ID,
                       CELL_ID=database$CELL_ID,
                       PLANT_GEN=rep(NA,nrow(database)),
                       PLANT_SPEC=database$PLANT_SPEC,
                       stringsAsFactors=FALSE)

start<-nrow(C_PLANTS)+1
C_PLANTS<-rbind(C_PLANTS,C_PLANTS_R)
C_PLANTS<-C_PLANTS[!duplicated(C_PLANTS$PLANT_ID),]
end<-nrow(C_PLANTS)
C_PLANTS<-C_PLANTS[start:end]
dbWriteTable(con2, "C_PLANTS", 
             value = C_PLANTS, append = TRUE, row.names = FALSE)

##7-C_ORG

C_ORG_R<-data.table(C_ORG_ID=database$C_ORG_ID,
                    SITE_ID=database$SITE_ID,
                    SYSTEM_ID=database$SYSTEM_ID,
                    CELL_ID=database$CELL_ID,
                    C_BOD_IN=database$C_BOD_IN,
                    C_BOD_OUT=database$C_BOD_OUT,
                    C_BOD_MT=rep(NA,nrow(database)),
                    C_BOD_ST=rep(NA,nrow(database)),
                    C_COD_IN=rep(NA,nrow(database)),
                    C_COD_OUT=rep(NA,nrow(database)),
                    C_COD_MT=rep(NA,nrow(database)),
                    C_COD_ST=rep(NA,nrow(database)),
                    C_TOC_IN=rep(NA,nrow(database)),
                    C_TOC_OUT=rep(NA,nrow(database)),
                    C_TOC_MT=rep(NA,nrow(database)),
                    C_TOC_ST=rep(NA,nrow(database)),
                    C_ORG_RD=numerep(NA,nrow(database)),
                    stringsAsFactors=FALSE)

start<-nrow(C_ORG)+1
C_ORG<-rbind(C_ORG,C_ORG_R)
C_ORG<-C_ORG[!duplicated(C_ORG$C_ORG_ID),]
end<-nrow(C_ORG)
C_ORG<C_ORG[start:end]
dbWriteTable(con2, "C_ORG", 
             value = C_ORG, append = TRUE, row.names = FALSE)

pw <- {
  "mauricio"
}

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = "postgres",
                 host = "localhost", port = 5432,
                 user = "openpg", password = pw)

con2=dbConnect(PostgreSQL(),user = "postgres",password="wetlands",dbname = "postgres")

df_postgres <- dbGetQuery(con2, "SELECT * from database")
row_postgres<-nrow(df_postgres)+1
lenght<-nrow(database)
row_postgres_end<-nrow(df_postgres)+lenght
ID<-seq(row_postgres:row_postgres_end)
database$ID<-ID

dbWriteTable(con2, "database", 
             value = database, append = TRUE, row.names = FALSE)


##Copy the files in the folder Documents_backup and eliminate from the folder Documents

current.folder <- "C:/Users/Mauricio/Desktop/WETLANDS_EXTRATION/Documents"
new.folder <- "C:/Users/Mauricio/Desktop/WETLANDS_EXTRATION/Documents_backup"
list.of.files <- list.files(current.folder,full.names = TRUE)
file.copy(list.of.files, new.folder)
file.remove(list.of.files)














extract_country<-as.data.frame(lapply(kw_country, function(x) lapply(countries_names,function(y) grepl(y,x))))








ddply(df,.(Date),function(x) data.frame(Date=x$Date[1],Count=sum(x$Count)))

#luego de usar un agregation en la tabla que quieres hacer... viene la union
table2<-data.frame(Countries=c("Spain","Italy","Mexico","Nordamerica","Argentine"))
df <- rbind(table2,table1)

temp3<-setkey(df, NULL)
temp3 <- unique(temp3)















SITES <- data.frame(SITE_ID=character(),
                      SYSTEM_ID=character(),
                      CTRY_CODE=character(), 
                      SUBN_ADM_L=character(),
                      MUNI=character(),
                      LAT=numeric(),
                      LONGITUD=numeric(),
                      ELEVATION=numeric(),
                      WW_TYPE=character(),
                      HYDR_CATCH=character(),
                      TOTAL_PE=numeric(),
                      ULT_DISP_M=character(),
                      SW_CATCH_A=character(),
                      stringsAsFactors=FALSE)

SITES$SITE_ID<-c("SITE_1","SITE_2")
SITES$SYSTEM_ID<-c("SYSTEM_1","SYSTEM_2")
SITES$CTRY_CODE<-c("IT","SP")
SITES$MUNY<-c("Rome","Barcelona")


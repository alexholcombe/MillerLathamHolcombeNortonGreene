#library('xlsx')
#dd<- read.xlsx("Near and Far Past and Future  (Master Spreadsheet).xlsx", 1) #didn't work because xlsx package error
#dd<- read.table('Near and Far Past and Future  (Master Spreadsheet).txt') #didn't work
#read.table('NearAndFarAlexCopyPasteBecauseCouldntReadOriginal.txt')
library('readr')
dd<-read_tsv('NearAndFarAlexCopyPasteBecauseCouldntReadOriginal.txt')

library(dplyr)
cor.test(dd$`Level of Agreement NP`,dd$`Level of Agreement NF`) #replicates what Andrew reported

gp<- dd %>% group_by(Valence, Good) #%>% cor.test(`Level of Agreement NP`,`Level of Agreement NF`)
dplyr::summarize(gp, cor(`Level of Agreement NP`,`Level of Agreement NF`))

#get the p-values
ddd<- gp %>% filter(Valence=="Negative", Good=="Hedonic")
cor.test(ddd$`Level of Agreement NP`,ddd$`Level of Agreement NF`) #replicates what Andrew reported

ddd<- gp %>% filter(Valence=="Negative", Good=="Non-Hedonic")
cor.test(ddd$`Level of Agreement NP`,ddd$`Level of Agreement NF`) #replicates what Andrew reported

ddd<- gp %>% filter(Valence=="Positive", Good=="Hedonic")
cor.test(ddd$`Level of Agreement NP`,ddd$`Level of Agreement NF`) #replicates what Andrew reported

ddd<- gp %>% filter(Valence=="Positive", Good=="Non-Hedonic")
cor.test(ddd$`Level of Agreement NP`,ddd$`Level of Agreement NF`) #replicates what Andrew reported

#See whether people have a correlation in the MAGNITUDE of their responses
mag <- gp
mag$`Level of Agreement NP` <- abs(mag$`Level of Agreement NP` - 4)
mag$`Level of Agreement NF` <- abs(mag$`Level of Agreement NF` - 4)

dplyr::summarize(mag, cor(`Level of Agreement NP`,`Level of Agreement NF`)) #correlations

ddd<- mag %>% filter(Valence=="Negative", Good=="Hedonic")
cor.test(ddd$`Level of Agreement NP`,ddd$`Level of Agreement NF`)

ddd<- mag %>% filter(Valence=="Negative", Good=="Non-Hedonic")
cor.test(ddd$`Level of Agreement NP`,ddd$`Level of Agreement NF`)

ddd<- mag %>% filter(Valence=="Positive", Good=="Hedonic")
cor.test(ddd$`Level of Agreement NP`,ddd$`Level of Agreement NF`)

ddd<- mag %>% filter(Valence=="Positive", Good=="Non-Hedonic")
cor.test(ddd$`Level of Agreement NP`,ddd$`Level of Agreement NF`)

#Uh-oh I notice different degrees of freedom happening, is there missing data?

checkAllCombosOccurEquallyOften<- function(df,colNames,dropZeros=FALSE,printTable=FALSE) {
  #in data.frame df, check whether the factors in the list colNames reflect full factorial design (all combinations of levels occur equally often)
  #
  #dropZeros is useful if one of the factors nested in the others. E.g. testing different speeds for each level of    
  # something else, then a lot of the combos will occur 0 times because that speed not exist for that level.
  #but it's dangerous to dropZeros because it won't pick up on 0's that occur for the wrong reason- not fully crossed
  #
  #Returns:
  # true/false, and prints informational message
  #
  listOfCols <- as.list( df[colNames] )
  tt<- table(listOfCols)
  if (dropZeros) {  
    tt<- tt[tt!=0]   
  }           
  if (printTable) {
    comboNames<- attributes(listOfCols)$labels
    print(comboNames)
    cat(tt)
  }
  colNamesStr <- paste(colNames,collapse=",")
  if ( length(unique(tt)) == 1 ) { #if fully crossed, all entries in table should be identical (all combinations occur equally often)
    print(paste(colNamesStr,"fully crossed- each combination occurred",unique(tt)[1],'times'))
    ans <- TRUE
  } else {
    print(paste(colNamesStr,"NOT fully crossed,",length(unique(tt)),'distinct repetition numbers.'  ))
    ans <- FALSE
  } 
  return(ans)
}

checkAllCombosOccurEquallyOften(gp,c("Good","Valence"),printTable=TRUE) #specify dataframe and columns

#CONFIDENCE
#
cor.test(dd$`Level of Confidence NP`,dd$`Level of Confidence NF`) #replicates what Andrew reported


#ANOVA
#Do statistics on it
require(ez)

wide<-dd
wide$subject <- 1:nrow(wide)
#oh no data is wide for future/past, would have to convert that to long
library(tidyr)
#factors = good, valence
ll<- wide %>% gather(direction,levelOfAgreement,c("Level of Agreement NP","Level of Agreement NF"))

#Change overly-long variable values to past and future
ll <- ll %>% mutate(direction=replace(direction, direction=="Level of Agreement NF", "Future"))
ll <- ll %>% mutate(direction=replace(direction, direction=="Level of Agreement NP", "Past"))

agreementANOVA <- ezANOVA(data=ll, dv="levelOfAgreement", within=direction, between=.(Good,Valence), wid=subject)
print(agreementANOVA)
cat("F=", round(agreementANOVA$ANOVA$F,3))
cat(" ps=", round(agreementANOVA$ANOVA$p,3), "\n", sep=",")



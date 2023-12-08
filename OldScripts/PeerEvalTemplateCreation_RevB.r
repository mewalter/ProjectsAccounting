
library("tidyverse")
library("openxlsx")    # library("readxl") # this is the tidyverse installed package
library("scales")
library("lubridate")
library("rstudioapi")

#library("pastecs")
#library("anytime")

# rm(list=ls())

# a way to read in lines but then skip rows of your choosing
#all_content = readLines("file.csv")
#skip_second = all_content[-2]

BaseDir <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DataDir <- paste0(BaseDir,"/TeamTemplates/")
#setwd(DataDir)

data = read.csv("GroupsWithNames.csv", header = TRUE, stringsAsFactors = FALSE)
FourPerson <- data %>% mutate(TeamNum = as.numeric(str_extract(data$Project.Number, "[0-9]+"))) %>% filter(Number.Members==4) 
FivePerson <- data %>% mutate(TeamNum = as.numeric(str_extract(data$Project.Number, "[0-9]+"))) %>% filter(Number.Members==5)
SixPerson <- data %>% mutate(TeamNum = as.numeric(str_extract(data$Project.Number, "[0-9]+"))) %>% filter(Number.Members==6) 

for (i in 1:nrow(FivePerson)/5) {
  wb <- loadWorkbook("PeerEvaluationTemplate_5Person.xlsx")
  for (j in 1:5) 
    writeData(wb,"PeerRating",FivePerson$Name[i*5-5+j], startCol = j, startRow = j)
  protectWorksheet(wb, "PeerRating", protect = TRUE, password = "456", lockSelectingLockedCells = TRUE, 
                   lockSelectingUnlockedCells = FALSE, lockObjects = TRUE, lockScenarios = TRUE)
  saveWorkbook(wb,paste(DataDir,"Team",FivePerson$TeamNum[i*5],"MAE151FinalPeerEvaluationTemplate.xlsx",sep = ""),overwrite = TRUE)
}

i <- 1
for (i in 1:nrow(SixPerson)/6) {
  wb <- loadWorkbook("PeerEvaluationTemplate_6Person.xlsx")
  for (j in 1:6) 
    writeData(wb,"PeerRating",SixPerson$Name[i*6-6+j], startCol = j, startRow = j)
  protectWorksheet(wb, "PeerRating", protect = TRUE, password = "456", lockSelectingLockedCells = TRUE, 
                   lockSelectingUnlockedCells = FALSE, lockObjects = TRUE, lockScenarios = TRUE)
  saveWorkbook(wb,paste(DataDir,"Team",SixPerson$TeamNum[i*6],"MAE151FinalPeerEvaluationTemplate.xlsx",sep = ""),overwrite = TRUE)
}




# Used this to fixe the silly error that I was getting in my templates
wb <- loadWorkbook("PeerEvaluationTemplate_2Person.xlsx")
saveWorkbook(wb, "test2.xlsx", overwrite = TRUE)
wb <- loadWorkbook("PeerEvaluationTemplate_3Person.xlsx")
saveWorkbook(wb, "test3.xlsx", overwrite = TRUE)
wb <- loadWorkbook("PeerEvaluationTemplate_4Person.xlsx")
saveWorkbook(wb, "test4.xlsx", overwrite = TRUE)




# Example from https://www.rdocumentation.org/packages/openxlsx/versions/4.2.5/topics/writeData

wb2 <- createWorkbook()

## Add worksheets
addWorksheet(wb2, "Cars")
addWorksheet(wb2, "Formula")


x <- mtcars[1:6, ]
writeData(wb2, "Cars", x, startCol = 2, startRow = 3, rowNames = TRUE)


#####################################################################################
## Bordering

writeData(wb2, "Cars", x,
          rowNames = TRUE, startCol = "O", startRow = 3,
          borders = "surrounding", borderColour = "black"
) ## black border

writeData(wb2, "Cars", x,
          rowNames = TRUE,
          startCol = 2, startRow = 12, borders = "columns"
)

writeData(wb2, "Cars", x,
          rowNames = TRUE,
          startCol = "O", startRow = 12, borders = "rows"
)


#####################################################################################
## Header Styles

hs1 <- createStyle(
  fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "italic",
  border = "Bottom"
)

writeData(wb2, "Cars", x,
          colNames = TRUE, rowNames = TRUE, startCol = "B",
          startRow = 23, borders = "rows", headerStyle = hs1, borderStyle = "dashed"
)


hs2 <- createStyle(
  fontColour = "#ffffff", fgFill = "#4F80BD",
  halign = "center", valign = "center", textDecoration = "bold",
  border = "TopBottomLeftRight"
)

writeData(wb2, "Cars", x,
          colNames = TRUE, rowNames = TRUE,
          startCol = "O", startRow = 23, borders = "columns", headerStyle = hs2
)





#####################################################################################
## Save workbook
## Open in excel without saving file: openXL(wb)
# }
# NOT RUN {
saveWorkbook(wb2, "writeDataExample.xlsx", overwrite = TRUE)
# }


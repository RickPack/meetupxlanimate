#################################################################
##  This script aggregates CSVs in a folder into a .XLSX file  ##
#     Worksheet names match the name of the CSV sans ".csv"    ##
#################################################################
CSV_folder <- 'C:/Users/Packr1/Documents/VBC/PopHealth/Nationwide/20191217'
## What is the name of the output Excel workbook?
##   - will be saved in CSV_folder
excel_save_file <- "Nation_Diab_Nov2018_Oct2019_PxCounts"

library(dplyr)
library(openxlsx)
library(data.table)
library(stringr)

lst_fls0      <- list.files(CSV_folder)
lst_fls       <- lst_fls0[grepl("CSV", toupper(lst_fls0))]
lst_fls_full0 <- list.files(CSV_folder, full.names = TRUE)
lst_fls_full  <- lst_fls_full0[grepl("CSV", toupper(lst_fls_full0))]
bldStyle  <- createStyle(fontSize = 14, fontColour = "black", textDecoration = c("BOLD"))
xlsxformat <- function(wb, namxlsx="", wksht_name, df_inxlsx, nxlsx, 
                       max_nxlsx){
    if (nxlsx == 1) {
        wb <- createWorkbook()
    }
    addWorksheet(wb, wksht_name)
    writeData(wb, nxlsx, df_inxlsx, colNames = TRUE, headerStyle = bldStyle)
    setColWidths(wb, sheet = nxlsx, cols = 1:ncol(df_inxlsx), widths = "auto")
    if (max_nxlsx == nxlsx) {
        saveWorkbook(wb, paste0(namxlsx, ".xlsx"), overwrite = TRUE)
    }
    invisible(wb)
}
onesht_xlsx <- function(namxlsx, df_inxlsx, datarngm){
    wb <- xlsxformat(wb, namxlsx, wksht_name = datarng, df_inxlsx, 1, 1)
}

shtnums <- length(lst_fls)
for (k in 1:length(lst_fls)) {
  print(paste("Sheet ", k, "of", shtnums))
  print(paste("Processing", lst_fls_full[k]))
  infile <- fread(lst_fls_full[k])
  nams_infile <- colnames(infile)
  # Remove DUMMY columns and those named only a letter
  cols_remove <- ""
  cols_remove <- nams_infile[grepl("DUMMY", toupper(nams_infile))]
  cols_remove <- c(cols_remove, 
                   nams_infile[which(toupper(nams_infile) %in% LETTERS)])
  if (length(cols_remove) > 0) {
      infile <- infile %>% 
          select(-cols_remove)
  }
  # worksheet name
  wkname <- lst_fls[k]
  wkname <- str_replace(wkname, ".csv", "")
  
  wb <- xlsxformat(wb, 
                   wksht_name = wkname,
                   df_inxlsx  = infile, 
                   nxlsx = k, 
                   namxlsx = paste0(CSV_folder, "/",
                                    excel_save_file),
                   max_nxlsx  = shtnums)
  
  if (k == shtnums) {
      print(paste("Saved",
                  print(paste0(CSV_folder, "/",
                               excel_save_file,
                               ".xlsx"))))
  }
}
      
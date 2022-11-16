#' openxlsx helpful functions for meetupxlanimate presentation

#' @export midStyle
#' @export bldStyle
#' @export bld_underStyle
#' @export hltStyle
#' @export hltStyle2
#' @export greyStyle
#' @export centStyle
#' @export dateStyle
#' @export xlsxformat
#' @export onesht_xlsx

# examine gridExpand, MOD(ROW(),2)=0,
# use these styles after line like writeData(wb, "Sheet1", sumfrm2)

library(openxlsx)
midStyle <- openxlsx::createStyle(bgFill = "#ffA500", fontSize = 11, fontColour = "black", textDecoration = c("BOLD"))
bldStyle  <- openxlsx::createStyle(fontSize = 14, fontColour = "black", textDecoration = c("BOLD"),
                                   wrapText = TRUE)
bld_underStyle  <- openxlsx::createStyle(fontSize = 14, fontColour = "black", textDecoration = c("BOLD","UNDERLINE"),
                                         wrapText = TRUE)
hltStyle  <- openxlsx::createStyle(bgFill = "#FFFF00", fontSize = 14, fontColour = "black", textDecoration = c("BOLD"))
hltStyle2 <- openxlsx::createStyle(bgFill = "#FFFF00", fontSize = 14, fontColour = "black", textDecoration = "BOLD", halign = "center", valign = "center")
greyStyle  <- openxlsx::createStyle(bgFill = "#d3d3d3")
centStyle  <- openxlsx::createStyle(halign = "CENTER")
dateStyle  <- openxlsx::createStyle(numFmt = "yyyy-mm-dd")

# column width handling per https://stackoverflow.com/questions/45860085/r-autofit-excel-column-width
xlsxformat <- function(df_inxlsx, namxlsx="", nxlsx, max_nxlsx = 99, 
                       wksht_name = "data"){
  if (nxlsx==1) {
    wb <- createWorkbook()
  }
  addWorksheet(wb, wksht_name)
  writeData(wb, nxlsx, df_inxlsx, colNames = TRUE, headerStyle = bld_underStyle)
  ## Freeze Top Row
  freezePane(wb, nxlsx, firstRow = TRUE) 
  
  width_vec <- apply(df_inxlsx, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
  width_vec_header <- nchar(colnames(df_inxlsx))  + 2
  max_vec_header <- pmax(width_vec, width_vec_header)
  ## Wrap header text
  
  setColWidths(wb, nxlsx, cols = 1:ncol(df_inxlsx), widths = max_vec_header )
  
  if (max_nxlsx == nxlsx) {
    saveWorkbook(wb, paste0(namxlsx, ".xlsx"), overwrite = TRUE)
  }
  
  invisible(wb)
  assign("wb", wb, envir = .GlobalEnv)
}

onesht_xlsx <- function(df_inxlsx, namxlsx){
  wb <- xlsxformat(df_inxlsx, namxlsx, 1, 1, wksht_name = "data", wb)
}

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

midStyle <- openxlsx::createStyle(bgFill = "#ffA500", fontSize = 11, fontColour = "black", textDecoration = c("BOLD"))
bldStyle  <- openxlsx::createStyle(fontSize = 14, fontColour = "black", textDecoration = c("BOLD"))
bld_underStyle  <- openxlsx::createStyle(fontSize = 14, fontColour = "black", textDecoration = c("BOLD","UNDERLINE"))
hltStyle  <- openxlsx::createStyle(bgFill = "#FFFF00", fontSize = 14, fontColour = "black", textDecoration = c("BOLD"))
hltStyle2 <- openxlsx::createStyle(bgFill = "#FFFF00", fontSize = 14, fontColour = "black", textDecoration = "BOLD", halign = "center", valign = "center")
greyStyle  <- openxlsx::createStyle(bgFill = "#d3d3d3")
centStyle  <- openxlsx::createStyle(halign = "CENTER")
dateStyle  <- openxlsx::createStyle(numFmt = "yyyy-mm-dd")

xlsxformat <- function(df_inxlsx, namxlsx="", nxlsx, max_nxlsx, wksht_name = "data", wb){
    if (nxlsx==1) {
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

onesht_xlsx <- function(df_inxlsx, namxlsx){
    wb <- xlsxformat(df_inxlsx, namxlsx, 1, 1, wksht_name = "data", wb)
}

#####################################################################
###  Conditional formatting example 1 to modify for presentation  ###
#####################################################################
## Add a worksheet and data
# wb <- createWorkbook()
# addWorksheet(wb, "Sheet1")
# writeData(wb, 1, sumfrm2, colNames=TRUE)
## Create a style
#### green
# conditionalStyle <- openxlsx::createStyle(bgFill = "#00FF00")
# #### red
# ## Conditionally format cells:Rows 10 to 20 columns A to J (1:10)
# writeData(wb, "Sheet1", sumfrm2)
# negStyle <- openxlsx::createStyle(bgFill = "#FF0000", fontSize = 14, fontColour = "black", textDecoration = c("BOLD"))
# posStyle <- openxlsx::createStyle(bgFill = "#00FF00", fontSize = 14, fontColour = "black", textDecoration = c("BOLD"))
# bldStyle <- openxlsx::createStyle(fontSize = 14, fontColour = "black", textDecoration = c("BOLD"))

#ref. S:\Atlantic\Adrianne\ChesaHbA1c.R
#ref. S:\Atlantic\Adrianne\code.R

# ## for i in 1:(nrow(sumfrm)+1){
# ##  b <- paste("$A1",i,sep="")
# ruletxt <- "$A2 <= $A3"
# ##  writeData(wb, "temp", b)
# conditionalFormatting(wb, "Sheet1", cols = 6:8, rows = 2:(nrow(sumfrm)+1), rule = "NOT(ISBLANK(A1))", style = bldStyle)
# conditionalFormatting(wb, "Sheet1", cols = 6,   rows = 2:(nrow(sumfrm)+1), rule = "AND($F2 >= 9, NOT(ISBLANK($F2)))", style = posStyle)
# conditionalFormatting(wb, "Sheet1", cols = 6,   rows = 2:(nrow(sumfrm)+1), rule = "AND($F2 <  9, NOT(ISBLANK($F2)))", style = negStyle)
# conditionalFormatting(wb, "Sheet1", cols = 7,   rows = 2:(nrow(sumfrm)+1), rule = "AND($F2 >= $G2, $F2 > 0, $G2 > 0)", style = posStyle)
# conditionalFormatting(wb, "Sheet1", cols = 7,   rows = 2:(nrow(sumfrm)+1), rule = "AND($F2 <  $G2, $F2 > 0, $G2 > 0)", style = negStyle)
# conditionalFormatting(wb, "Sheet1", cols = 8,   rows = 2:(nrow(sumfrm)+1), rule = "AND($G2 >= $H2, $G2 > 0, $H2 > 0)", style = posStyle)
# conditionalFormatting(wb, "Sheet1", cols = 8,   rows = 2:(nrow(sumfrm)+1), rule = "AND($G2 <  $H2, $G2 > 0, $H2 > 0)", style = negStyle)

# setColWidths(wb, 1, cols = 1:ncol(sumset2), widths = "auto")
# setColWidths(wb, 2, cols = 1:7, widths = "auto")
# saveWorkbook(wb, paste(condnam,"_",filnam,sep=""), overwrite = TRUE)

# conditionalFormatting(wb, sheetname, cols = 1:ncol(ckdata), rows = 2:(nrow(ckdata)+1), rule = "MOD(ROW(),2)=0", style = greyStyle)

# addStyle(wb, sheet = shtnum, boldStyle, rows = 1, cols = 1:ncol(ckdata), gridExpand = TRUE)

# conditionalFormatting(wb, "Sheet1", cols = 15,    rows = 2:(nrow(sumfrm)+1), rule = "$A2 == 1", style = midStyle)

# ## if issues with save try at CMD: set PATH=%PATH%;C:/Rtools/bin
# ## make sure to format top header row cleanly
# saveWorkbook(wb, "C:/Users/Packr1/Documents/Adrianne/Chesapeake_HbA1c/Chesa_HbA1c_summary.xlsx", overwrite = TRUE)


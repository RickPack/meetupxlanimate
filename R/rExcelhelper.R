bldStyle  <- createStyle(fontSize = 14, fontColour = "black", textDecoration = c("BOLD"))

xlsxformat <- function(wb, namxlsx="", wksht_name, df_inxlsx, nxlsx, max_nxlsx){
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
getCT <- function( filepath ) {
    doc = xmlTreeParse( filepath, useInternalNodes = T )
    namespaces <- namespaces(doc)
    
    cl <- getCodeListItems(doc, namespaces, "CodeListItem")
    enu <- getCodeListItems(doc, namespaces, "EnumeratedItem")
    mm <- merge(cl, enu, all=T, stringsAsFactors=FALSE)
    sortlist <- order(mm$OID, mm$OrderNumber, mm$Rank, mm$CodedValue)
    mm2 <- mm[sortlist,]
    return( mm2 )
}

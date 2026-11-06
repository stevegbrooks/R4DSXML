getCodeListItems <- function(doc, namespaces, item_name) {
    code_lists <- getNodeSet(doc, paste0("//ns:CodeList[ns:", item_name, "]"), namespaces)
    if (!length(code_lists)) stop("No ", item_name, " codelists found")

    alias_attribute <- function(nodes, attribute) {
        if (length(nodes)) getAttr(nodes, attribute) else NA
    }
    frames <- lapply(code_lists, function(code_list) {
        # ponytail: cross-version OID matches are retained; changing scope changes existing rows.
        path <- paste0('//ns:CodeList[@OID="', xmlGetAttr(code_list, "OID"), '"]')
        item_path <- paste0(path, "/ns:", item_name)
        items <- getNodeSet(doc, item_path, namespaces)
        aliases <- lapply(seq_along(items), function(i) {
            getNodeSet(doc, paste0(item_path, "[", i, ']/ns:Alias[@Context="nci:ExtCodeID"]'), namespaces)
        })
        code_list_alias <- getNodeSet(doc, paste0(path, '/ns:Alias[@Context="nci:ExtCodeID"]'), namespaces)
        values <- list(
            OID = xmlGetAttr(code_list, "OID", default = NA),
            Name = xmlGetAttr(code_list, "Name", default = NA),
            DataType = xmlGetAttr(code_list, "DataType", default = NA),
            SASFormatName = xmlGetAttr(code_list, "SASFormatName", default = NA),
            CodeListCode = alias_attribute(code_list_alias, "Name"),
            CodeListContext = alias_attribute(code_list_alias, "Context"),
            CodedValue = getAttr(items, "CodedValue"),
            OrderNumber = getAttr(items, "OrderNumber"),
            Rank = getAttr(items, "Rank"),
            ExtendedValue = getAttr(items, "def:ExtendedValue"),
            ItemCode = unlist(lapply(aliases, alias_attribute, "Name"), use.names = FALSE),
            ItemContext = unlist(lapply(aliases, alias_attribute, "Context"), use.names = FALSE)
        )
        if (item_name == "CodeListItem") {
            decoded <- unlist(lapply(seq_along(items), function(i) {
                nodes <- getNodeSet(doc, paste0(item_path, "[", i, "]/ns:Decode"), namespaces)
                if (length(nodes)) sapply(nodes, xmlValue) else NA
            }), use.names = FALSE)
            values <- append(values, list(Decode = decoded), after = 7L)
        }
        as.data.frame(values, stringsAsFactors = FALSE)
    })
    do.call(rbind, frames)
}

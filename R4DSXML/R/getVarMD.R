getVarMD <- function(filepath) {
    doc <- xmlTreeParse(filepath, useInternalNodes = T)
    namespaces <- namespaces(doc)

    # Add "def" namespace if not present
    if (!"def" %in% names(namespaces)) {
        namespaces["def"] <- "http://www.cdisc.org/ns/def/v2.0"
    }

    # ItemRef
    ItemGroupDef <- getNodeSet(doc, "//ns:ItemGroupDef", namespaces)
    DSName <- getDSName(ItemGroupDef)


    for (i in DSName) {
        ItemRefNode <- getNodeSet(
            doc,
            paste("//ns:ItemGroupDef[@Name ='", i, "']//ns:ItemRef", sep = ""),
            namespaces
        )

        IR_ItemOID <- getAttr(
            Nodeset = ItemRefNode,
            Attr = "ItemOID"
        )

        IR_OrderNumber <- as.integer(
            getAttr(
                Nodeset = ItemRefNode,
                Attr = "OrderNumber"
            )
        )

        IR_Mandatory <- getAttr(Nodeset = ItemRefNode, Attr = "Mandatory")

        IR_KeySequence <- as.integer(
            getAttr(Nodeset = ItemRefNode, Attr = "KeySequence")
        )

        IGD_Name <- i

        tmpdf <- data.frame(
            IGD_Name,
            IR_ItemOID,
            IR_OrderNumber,
            IR_Mandatory,
            IR_KeySequence,
            stringsAsFactors = FALSE
        )

        if (is.na(match("ItemRef", ls()))) {
            ItemRef <- tmpdf
        } else {
            ItemRef <- merge(ItemRef, tmpdf, all = T)
        }
    }
    # get ItemRef end

    item_def <- getItemDef(doc)
    Variable.Metadata <-
        merge(ItemRef, item_def, by.x = "IR_ItemOID", by.y = "ID_OID")
    so <- order(
        Variable.Metadata$IGD_Name,
        Variable.Metadata$IR_OrderNumber
    )

    Variable.Metadata <- Variable.Metadata[so, ]
    row.names(Variable.Metadata) <- NULL

    # Add where clause conditions for variables with ValueListDefs
    valueListDefs <- getNodeSet(doc, "//def:ValueListDef", namespaces)
    
    if (length(valueListDefs) > 0) {
        # Initialize a new column for where conditions
        Variable.Metadata$Where_Conditions <- NA_character_
        
        for (vld in valueListDefs) {
            # Extract the variable name from the OID (e.g., "VL.LB.LBSPID" -> "LBSPID")
            var_oid <- xmlGetAttr(vld, "OID")
            dataset_var <- strsplit(var_oid, "\\.")[[1]]
            if (length(dataset_var) >= 3) {
                dataset <- dataset_var[2]
                var_name <- dataset_var[3]
                
                # Get all ItemRefs within this ValueListDef
                itemRefs <- getNodeSet(vld, ".//ns:ItemRef", namespaces)
                
                for (itemRef in itemRefs) {
                    # Get WhereClauseRef
                    whereClauseRef <- getNodeSet(itemRef, ".//def:WhereClauseRef", namespaces)
                    if (length(whereClauseRef) > 0) {
                        whereClauseOID <- xmlGetAttr(whereClauseRef[[1]], "WhereClauseOID")
                        
                        # Get the actual WhereClauseDef
                        whereClauseDef <- getNodeSet(
                            doc, 
                            sprintf("//def:WhereClauseDef[@OID='%s']", whereClauseOID),
                            namespaces
                        )
                        
                        if (length(whereClauseDef) > 0) {
                            # Extract RangeCheck information
                            rangeChecks <- getNodeSet(whereClauseDef[[1]], ".//ns:RangeCheck", namespaces)
                            conditions <- sapply(rangeChecks, function(rc) {
                                comparator <- xmlGetAttr(rc, "Comparator")
                                checkValues <- getNodeSet(rc, ".//ns:CheckValue", namespaces)
                                values <- sapply(checkValues, xmlValue)
                                paste(comparator, paste(values, collapse = ", "))
                            })
                            
                            # Find the matching row in Variable.Metadata
                            row_idx <- which(Variable.Metadata$IGD_Name == dataset & 
                                           Variable.Metadata$ID_Name == var_name)
                            
                            if (length(row_idx) > 0) {
                                current_condition <- Variable.Metadata$Where_Conditions[row_idx]
                                new_condition <- paste(conditions, collapse = "; ")
                                
                                # Append new condition or create if NA
                                if (is.na(current_condition)) {
                                    Variable.Metadata$Where_Conditions[row_idx] <- new_condition
                                } else {
                                    Variable.Metadata$Where_Conditions[row_idx] <- paste(
                                        current_condition, new_condition, sep = "\n"
                                    )
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    # Add derivation descriptions for derived variables
    if (any(Variable.Metadata$ID_OriginType == "Derived")) {
        # Get all MethodDef nodes
        methodDefs <- getNodeSet(doc, "//ns:MethodDef", namespaces)

        # Create a mapping of OID to Description
        method_descriptions <- sapply(methodDefs, function(node) {
            oid <- xmlGetAttr(node, "OID")
            desc <- xpathSApply(node, ".//ns:TranslatedText", xmlValue, namespaces = namespaces)
            if (length(desc) > 0) {
                # Clean up the description text
                desc <- gsub("[\r\n]", " ", desc[1]) # Replace newlines with spaces
                desc <- trimws(desc) # Remove leading/trailing whitespace
                desc <- gsub("\\s+", " ", desc) # Replace multiple spaces with single space
                return(c(oid, desc))
            }
            return(c(oid, NA))
        })

        # Convert to data frame
        method_map <- data.frame(
            MethodOID = method_descriptions[1, ],
            Description = method_descriptions[2, ],
            stringsAsFactors = FALSE
        )

        # For derived variables, get their descriptions
        derived_vars <- Variable.Metadata$ID_OriginType == "Derived"

        # Process each dataset separately
        for (dataset in unique(Variable.Metadata$IGD_Name)) {
            dataset_vars <- Variable.Metadata$IGD_Name == dataset & derived_vars
            if (any(dataset_vars)) {
                constructed_oids <- paste0("MT.", dataset, ".", Variable.Metadata$ID_Name[dataset_vars])
                matched_descriptions <- method_map$Description[match(constructed_oids, method_map$MethodOID)]
                Variable.Metadata$ID_OriginDescription[dataset_vars] <- matched_descriptions
            }
        }
    }

    return(Variable.Metadata)
}

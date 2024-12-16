getVarMD <- function(filepath) {
    doc <- xmlTreeParse(filepath, useInternalNodes = T)
    namespaces <- namespaces(doc)

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

    # Add derivation descriptions for derived variables
    if (any(Variable.Metadata$ID_OriginType == "Derived")) {
        # Cache common lookups at the start
        methodDefs <- getNodeSet(doc, "//odm:MethodDef|//MethodDef", 
                               c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
        if (length(methodDefs) > 0) {
            method_map <- do.call(rbind, lapply(methodDefs, function(node) {
                oid <- xmlGetAttr(node, "OID")
                translated_text <- getNodeSet(node, ".//odm:TranslatedText|.//TranslatedText",
                                           c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
                desc <- if (length(translated_text) > 0) {
                    trimws(xmlValue(translated_text[[1]]))
                } else {
                    NA_character_
                }
                data.frame(
                    MethodOID = oid,
                    Description = desc,
                    stringsAsFactors = FALSE
                )
            }))
        } else {
            method_map <- data.frame(
                MethodOID = character(0),
                Description = character(0),
                stringsAsFactors = FALSE
            )
        }

        commentDefs <- getNodeSet(doc, "//def:CommentDef", 
                                c(def = "http://www.cdisc.org/ns/def/v2.0"))
        comment_map <- do.call(rbind, lapply(commentDefs, function(node) {
            oid <- xmlGetAttr(node, "OID")
            translated_text <- getNodeSet(node, ".//odm:TranslatedText|.//TranslatedText",
                                       c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
            desc <- if (length(translated_text) > 0) {
                trimws(xmlValue(translated_text[[1]]))
            } else {
                NA_character_
            }
            data.frame(
                CommentOID = oid,
                Description = desc,
                stringsAsFactors = FALSE
            )
        }))

        # For derived variables, get their descriptions
        derived_vars <- Variable.Metadata$ID_OriginType == "Derived"

        # Process each dataset separately
        for (dataset in unique(Variable.Metadata$IGD_Name)) {
            dataset_vars <- Variable.Metadata$IGD_Name == dataset & derived_vars
            if (any(dataset_vars)) {
                constructed_oids <- paste0("MT.", dataset, ".", Variable.Metadata$ID_Name[dataset_vars])
                matched_descriptions <- method_map$Description[match(constructed_oids, method_map$MethodOID)]
                
                # Only assign if we found matching descriptions
                valid_matches <- !is.na(matched_descriptions)
                if (any(valid_matches)) {
                    idx <- which(dataset_vars)[valid_matches]
                    Variable.Metadata$ID_OriginDescription[idx] <- matched_descriptions[valid_matches]
                }
            }
        }
    }

    # Add Where Conditions
    whereClauseDefs <- getNodeSet(doc, "//def:WhereClauseDef", 
                                 c(def = "http://www.cdisc.org/ns/def/v2.0"))
    valueListDefs <- getNodeSet(doc, "//def:ValueListDef", 
                               c(def = "http://www.cdisc.org/ns/def/v2.0"))
    
    # Create mapping of WhereClauseDef OIDs to their conditions
    where_conditions <- lapply(whereClauseDefs, function(node) {
        oid <- xmlGetAttr(node, "OID")
        rangeCheck <- xmlChildren(node)[[1]]
        comparator <- xmlGetAttr(rangeCheck, "Comparator")
        
        # Convert comparator to symbol
        comp_symbol <- switch(comparator,
            "EQ" = "=",
            "NE" = "!=",
            "IN" = "IN",
            "NOTIN" = "NOTIN",
            comparator
        )
        
        checkValue <- xmlValue(xmlChildren(rangeCheck)[[1]])
        itemOID <- xmlGetAttr(rangeCheck, "ItemOID")
        
        # Get variable name from ItemOID (removing "IT." prefix)
        varName <- sub("IT\\.[^.]+\\.", "", itemOID)
        
        # Construct condition string
        condition <- sprintf("%s %s '%s' (%s)", 
                           varName, comp_symbol, checkValue, checkValue)
        
        return(c(oid = oid, condition = condition))
    })
    
    if (length(where_conditions) > 0) {
        where_map <- do.call(rbind, where_conditions)
    } else {
        where_map <- matrix(character(0), ncol = 2)
        colnames(where_map) <- c("oid", "condition")
    }
    
    # Process ValueListDefs to create additional rows
    additional_rows <- list()
    
    for (vld in valueListDefs) {
        parentOID <- xmlGetAttr(vld, "OID")
        parts <- strsplit(parentOID, "\\.")[[1]]
        if (length(parts) >= 3) {
            dataset <- parts[2]
            varname <- parts[3]
            
            parent_idx <- which(Variable.Metadata$IGD_Name == dataset & 
                              Variable.Metadata$ID_Name == varname)
            
            if (length(parent_idx) > 0) {
                parent_row <- Variable.Metadata[parent_idx[1], ]
                
                itemRefs <- xmlChildren(vld)
                for (itemRef in itemRefs) {
                    whereClauseRef <- getNodeSet(itemRef, ".//def:WhereClauseRef", 
                                               c(def = "http://www.cdisc.org/ns/def/v2.0"))
                    
                    if (length(whereClauseRef) > 0) {
                        whereClauseOID <- xmlGetAttr(whereClauseRef[[1]], "WhereClauseOID")
                        condition_idx <- which(where_map[, "oid"] == whereClauseOID)
                        
                        if (length(condition_idx) > 0) {
                            new_row <- parent_row
                            new_row$Where_Condition <- where_map[condition_idx, "condition"]
                            
                            itemdef_oid <- xmlGetAttr(itemRef, "ItemOID")
                            
                            itemdef_nodes <- getNodeSet(doc, 
                                sprintf("//odm:ItemDef[@OID='%s']|//ItemDef[@OID='%s']", 
                                        itemdef_oid, itemdef_oid),
                                c(odm = "http://www.cdisc.org/ns/odm/v1.3",
                                  def = "http://www.cdisc.org/ns/def/v2.0"))
                            
                            if (length(itemdef_nodes) > 0) {
                                origin_nodes <- getNodeSet(itemdef_nodes[[1]], 
                                                         ".//def:Origin",
                                                         c(def = "http://www.cdisc.org/ns/def/v2.0"))
                                
                                if (length(origin_nodes) > 0) {
                                    origin_type <- xmlGetAttr(origin_nodes[[1]], "Type")
                                    new_row$ID_OriginType <- origin_type
                                    
                                    if (origin_type == "Assigned") {
                                        comment_oid <- xmlGetAttr(itemdef_nodes[[1]], "def:CommentOID")
                                        if (!is.null(comment_oid)) {
                                            desc <- comment_map$Description[comment_map$CommentOID == comment_oid]
                                            if (length(desc) > 0 && !is.na(desc[1])) {
                                                new_row$ID_OriginDescription <- desc[1]
                                            }
                                        }
                                    } else if (origin_type == "Derived") {
                                        method_oid <- xmlGetAttr(itemRef, "MethodOID")
                                        if (!is.null(method_oid)) {
                                            desc <- method_map$Description[method_map$MethodOID == method_oid]
                                            if (length(desc) > 0 && !is.na(desc[1])) {
                                                new_row$ID_OriginDescription <- desc[1]
                                            }
                                        }
                                    }
                                }
                            }
                            
                            additional_rows[[length(additional_rows) + 1]] <- new_row
                        }
                    }
                }
            }
        }
    }    
    # Add Where_Condition column to original Variable.Metadata
    Variable.Metadata$Where_Condition <- NA
    
    # Combine original and additional rows
    if (length(additional_rows) > 0) {
        additional_df <- do.call(rbind, additional_rows)
        Variable.Metadata <- rbind(Variable.Metadata, additional_df)
    }
    
    # Sort the final dataset
    so <- order(Variable.Metadata$IGD_Name, 
                Variable.Metadata$IR_OrderNumber,
                !is.na(Variable.Metadata$Where_Condition))
    
    Variable.Metadata <- Variable.Metadata[so, ]
    row.names(Variable.Metadata) <- NULL
    
    return(Variable.Metadata)
}

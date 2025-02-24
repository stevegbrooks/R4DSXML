# Helper function to process CodeLists
processCodeLists <- function(doc) {
    # Cache namespaces
    ns <- c(odm = "http://www.cdisc.org/ns/odm/v1.3")
    
    # Get all codelists at once and cache the result
    codeLists <- getNodeSet(doc, "//odm:CodeList|//CodeList", ns)
    codeList_map <- list()
    
    # Pre-compile XPath expressions
    itemsXPath <- ".//odm:CodeListItem|.//CodeListItem|.//odm:EnumeratedItem|.//EnumeratedItem"
    transTextXPath <- ".//odm:TranslatedText|.//TranslatedText"
    
    for (codeList in codeLists) {
        oid <- xmlGetAttr(codeList, "OID")
        name <- xmlGetAttr(codeList, "Name")
        
        # Get all items at once
        items <- getNodeSet(codeList, itemsXPath, ns)
        
        # Process items in a vectorized way where possible
        terms <- vapply(items, function(item) {
            value <- xmlGetAttr(item, "CodedValue")
            if (xmlName(item) == "CodeListItem") {
                decode_node <- getNodeSet(item, transTextXPath, ns)
                decode <- if (length(decode_node) > 0) xmlValue(decode_node[[1]]) else value
                sprintf('"%s" = "%s"', value, decode)
            } else {
                value
            }
        }, character(1))
        
        codeList_map[[oid]] <- sprintf("%s\n%s", name, paste(terms, collapse = "\n"))
    }
    
    return(list(codeList_map = codeList_map, codeLists = codeLists))
}

# Helper function to process ItemRefs more efficiently
processItemRefs <- function(doc, DSName) {
    # Pre-allocate the result list with estimated size
    result_list <- vector("list", length(DSName))
    
    for (i in seq_along(DSName)) {
        ItemRefNode <- getNodeSet(
            doc,
            sprintf("//ns:ItemGroupDef[@Name ='%s']//ns:ItemRef", DSName[i]),
            namespaces(doc)
        )
        
        # Get all attributes at once
        attrs <- lapply(ItemRefNode, xmlAttrs)
        
        # Create data frame directly from attribute list
        result_list[[i]] <- data.frame(
            IGD_Name = DSName[i],
            IR_ItemOID = sapply(attrs, `[`, "ItemOID"),
            IR_OrderNumber = as.integer(sapply(attrs, `[`, "OrderNumber")),
            IR_Mandatory = sapply(attrs, `[`, "Mandatory"),
            IR_KeySequence = as.integer(sapply(attrs, `[`, "KeySequence")),
            stringsAsFactors = FALSE
        )
    }
    
    # Combine all results at once
    do.call(rbind, result_list)
}

# Helper function to process method and comment definitions
processDefinitions <- function(doc) {
    # Process MethodDefs
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

    # Process CommentDefs
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
    
    return(list(method_map = method_map, comment_map = comment_map))
}

getVarMD <- function(filepath) {
    doc <- xmlTreeParse(filepath, useInternalNodes = TRUE)
    
    # Cache namespaces
    ns <- c(
        odm = "http://www.cdisc.org/ns/odm/v1.3",
        def = "http://www.cdisc.org/ns/def/v2.0"
    )
    
    # Get dataset names
    ItemGroupDef <- getNodeSet(doc, "//ns:ItemGroupDef", namespaces(doc))
    DSName <- getDSName(ItemGroupDef)
    
    # Process different parts of the define.xml - now with cached namespaces
    codeList_results <- processCodeLists(doc)
    codeList_map <- codeList_results$codeList_map
    codeLists <- codeList_results$codeLists
    
    # Process ItemRefs more efficiently
    ItemRef <- processItemRefs(doc, DSName)
    
    # Get definitions once and cache them
    definitions <- processDefinitions(doc)
    method_map <- definitions$method_map
    comment_map <- definitions$comment_map
    
    # Get item definitions
    item_def <- getItemDef(doc)
    
    # Merge data more efficiently
    Variable.Metadata <- merge(ItemRef, item_def, by.x = "IR_ItemOID", by.y = "ID_OID")
    
    # Initial sorting
    Variable.Metadata <- Variable.Metadata[order(Variable.Metadata$IGD_Name, Variable.Metadata$IR_OrderNumber), ]
    
    # Initialize new columns efficiently
    Variable.Metadata$Controlled_Terms <- NA_character_
    Variable.Metadata$Display_Value <- NA_character_
    Variable.Metadata$Permitted_Value <- NA_character_
    Variable.Metadata$Where_Condition <- NA_character_
    
    # Cache frequently used XPath queries
    whereClauseDefs <- getNodeSet(doc, "//def:WhereClauseDef", ns)
    valueListDefs <- getNodeSet(doc, "//def:ValueListDef", ns)
    
    # Process where conditions more efficiently
    where_conditions <- lapply(whereClauseDefs, function(node) {
        oid <- xmlGetAttr(node, "OID")
        rangeCheck <- xmlChildren(node)[[1]]
        comparator <- xmlGetAttr(rangeCheck, "Comparator")
        
        comp_symbol <- switch(comparator,
            "EQ" = "=",
            "NE" = "!=",
            "IN" = "IN",
            "NOTIN" = "NOTIN",
            comparator
        )
        
        checkValue <- xmlValue(xmlChildren(rangeCheck)[[1]])
        itemOID <- xmlGetAttr(rangeCheck, "ItemOID")
        varName <- sub("IT\\.[^.]+\\.", "", itemOID)
        
        c(oid = oid, 
          condition = sprintf("%s %s '%s' (%s)", 
                            varName, comp_symbol, checkValue, checkValue))
    })
    
    where_map <- if (length(where_conditions) > 0) {
        do.call(rbind, where_conditions)
    } else {
        matrix(character(0), ncol = 2, 
              dimnames = list(NULL, c("oid", "condition")))
    }
    
    # Pre-allocate additional_rows list with estimated size
    additional_rows <- vector("list", length(valueListDefs) * 5)  # Estimate 5 items per ValueListDef
    additional_row_count <- 0
    
    # Process ValueListDefs more efficiently
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
                
                # Cache ItemDef lookup
                parent_itemdef <- getNodeSet(doc, 
                    sprintf("//odm:ItemDef[@OID='IT.%s.%s']", dataset, varname),
                    ns)[[1]]
                
                is_data_value <- FALSE
                if (!is.null(parent_itemdef)) {
                    desc_node <- getNodeSet(parent_itemdef, 
                        ".//odm:TranslatedText", ns)
                    if (length(desc_node) > 0 && 
                        xmlValue(desc_node[[1]]) == "Data Value") {
                        is_data_value <- TRUE
                    }
                }
                
                # Process all ItemRefs at once
                itemRefs <- xmlChildren(vld)[names(xmlChildren(vld)) == "ItemRef"]
                
                for (itemRef in itemRefs) {
                    whereClauseRef <- getNodeSet(itemRef, 
                        ".//def:WhereClauseRef", ns)
                    
                    if (length(whereClauseRef) > 0) {
                        whereClauseOID <- xmlGetAttr(whereClauseRef[[1]], 
                                                   "WhereClauseOID")
                        condition_idx <- which(where_map[, "oid"] == whereClauseOID)
                        
                        new_row <- parent_row
                        if (length(condition_idx) > 0) {
                            new_row$Where_Condition <- where_map[condition_idx, 
                                                               "condition"]
                        }
                        
                        itemdef_oid <- xmlGetAttr(itemRef, "ItemOID")
                        method_oid <- xmlGetAttr(itemRef, "MethodOID")
                        
                        # Update row efficiently
                        mandatory <- xmlGetAttr(itemRef, "Mandatory")
                        if (!is.null(mandatory)) {
                            new_row$IR_Mandatory <- mandatory
                        }
                        
                        if (is_data_value) {
                            itemdef_node <- getNodeSet(doc, 
                                sprintf("//odm:ItemDef[@OID='%s']", itemdef_oid),
                                ns)[[1]]
                            
                            if (!is.null(itemdef_node)) {
                                codeListRef <- getNodeSet(itemdef_node, 
                                    ".//odm:CodeListRef", ns)
                                if (length(codeListRef) > 0) {
                                    codeListOID <- xmlGetAttr(codeListRef[[1]], 
                                                            "CodeListOID")
                                    if (!is.null(codeListOID) && 
                                        codeListOID %in% names(codeList_map)) {
                                        new_row$Controlled_Terms <- 
                                            codeList_map[[codeListOID]]
                                    }
                                }
                            }
                        }
                        
                        additional_row_count <- additional_row_count + 1
                        additional_rows[[additional_row_count]] <- new_row
                    }
                }
            }
        }
    }
    
    # Combine additional rows efficiently
    if (additional_row_count > 0) {
        additional_rows <- additional_rows[1:additional_row_count]
        additional_df <- do.call(rbind, additional_rows)
        Variable.Metadata <- rbind(Variable.Metadata, additional_df)
    }
    
    # Final sorting
    Variable.Metadata <- Variable.Metadata[order(Variable.Metadata$IGD_Name, 
                                               Variable.Metadata$IR_OrderNumber), ]
    row.names(Variable.Metadata) <- NULL
    
    # Process QNAM/QLABEL pairs more efficiently
    # Pre-process and cache QNAM and QLABEL CodeLists
    qnam_qlab_map <- new.env(hash = TRUE)  # Using environment for faster lookups
    
    # Filter codeLists first to only process relevant ones
    qnam_codelists <- codeLists[sapply(codeLists, function(cl) {
        oid <- xmlGetAttr(cl, "OID")
        grepl("\\$[^.]+\\.QNA[ML]", oid)
    })]
    
    if (length(qnam_codelists) > 0) {
        # Process all QNAM/QLABEL codelists at once
        for (codeList in qnam_codelists) {
            oid <- xmlGetAttr(codeList, "OID")
            items <- getNodeSet(codeList, ".//odm:CodeListItem", ns)
            
            if (length(items) > 0) {
                # Get all values and decodes at once
                values <- vapply(items, xmlGetAttr, character(1), "CodedValue")
                decodes <- vapply(items, function(item) {
                    decode_node <- getNodeSet(item, ".//odm:TranslatedText", ns)[[1]]
                    if (!is.null(decode_node)) xmlValue(decode_node) else NA_character_
                }, character(1))
                
                qnam_qlab_map[[oid]] <- list(
                    values = values,
                    decodes = decodes
                )
            }
        }
        
        # Process SUPP domains more efficiently
        supp_domains <- grep("^SUPP", unique(Variable.Metadata$IGD_Name), value = TRUE)
        
        if (length(supp_domains) > 0) {
            final_rows <- vector("list", length(supp_domains) * 10)  # Pre-allocate with estimate
            final_row_count <- 0
            
            for (dataset in supp_domains) {
                # Get all relevant rows at once
                dataset_rows <- Variable.Metadata[Variable.Metadata$IGD_Name == dataset, ]
                
                # Find QNAM and QLABEL info efficiently
                qnam_idx <- which(dataset_rows$ID_Name == "QNAM")
                qlab_idx <- which(dataset_rows$ID_Name == "QLABEL")
                
                if (length(qnam_idx) > 0 && length(qlab_idx) > 0) {
                    # Get CodeListRefs efficiently using cached namespaces
                    qnam_itemdef <- getNodeSet(doc, 
                        sprintf("//odm:ItemDef[@OID='IT.%s.QNAM']", dataset),
                        ns)[[1]]
                    
                    if (!is.null(qnam_itemdef)) {
                        qnam_codelistref <- getNodeSet(qnam_itemdef, 
                            ".//odm:CodeListRef", ns)[[1]]
                        
                        if (!is.null(qnam_codelistref)) {
                            qnam_oid <- xmlGetAttr(qnam_codelistref, "CodeListOID")
                            
                            if (!is.null(qnam_oid) && exists(qnam_oid, qnam_qlab_map)) {
                                qnam_values <- qnam_qlab_map[[qnam_oid]]
                                
                                # Get QVAL rows efficiently
                                qval_rows <- which(dataset_rows$ID_Name == "QVAL" & 
                                                 !is.na(dataset_rows$Where_Condition))
                                
                                if (length(qval_rows) > 0) {
                                    # Extract QNAM values efficiently
                                    qnam_values_from_where <- sub(".*'([^']+)'.*", "\\1",
                                        dataset_rows$Where_Condition[qval_rows])
                                    
                                    # Process each QVAL row
                                    for (i in seq_along(qval_rows)) {
                                        qnam_value <- qnam_values_from_where[i]
                                        value_idx <- match(qnam_value, qnam_values$values)
                                        
                                        if (!is.na(value_idx)) {
                                            # Create new row efficiently
                                            new_row <- dataset_rows[qval_rows[i], ]
                                            new_row[setdiff(names(new_row), "IGD_Name")] <- NA
                                            
                                            new_row$Display_Value <- qnam_values$decodes[value_idx]
                                            new_row$Permitted_Value <- qnam_values$values[value_idx]
                                            new_row$ID_Name <- "QNAM/QLABEL Value List"
                                            
                                            final_row_count <- final_row_count + 1
                                            final_rows[[final_row_count]] <- new_row
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            # Add final rows efficiently
            if (final_row_count > 0) {
                final_rows <- final_rows[1:final_row_count]
                final_df <- do.call(rbind, final_rows)
                
                # Update Variable.Metadata efficiently
                Variable.Metadata <- rbind(Variable.Metadata, final_df)
                
                # Final sorting
                Variable.Metadata <- Variable.Metadata[order(Variable.Metadata$IGD_Name,
                                                          Variable.Metadata$IR_OrderNumber), ]
                row.names(Variable.Metadata) <- NULL
            }
        }
    }
    
    return(Variable.Metadata)
}

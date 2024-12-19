# Helper function to process CodeLists
processCodeLists <- function(doc) {
    codeLists <- getNodeSet(doc, "//odm:CodeList|//CodeList", 
                           c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
    codeList_map <- list()
    
    for (codeList in codeLists) {
        oid <- xmlGetAttr(codeList, "OID")
        name <- xmlGetAttr(codeList, "Name")
        
        # Get both CodeListItem and EnumeratedItem nodes
        items <- getNodeSet(codeList, ".//odm:CodeListItem|.//CodeListItem|.//odm:EnumeratedItem|.//EnumeratedItem", 
                          c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
        
        terms <- sapply(items, function(item) {
            value <- xmlGetAttr(item, "CodedValue")
            # For CodeListItem, get decode from TranslatedText, for EnumeratedItem just use the CodedValue
            if (xmlName(item) == "CodeListItem") {
                decode_node <- getNodeSet(item, ".//odm:TranslatedText|.//TranslatedText",
                                        c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
                decode <- if (length(decode_node) > 0) xmlValue(decode_node[[1]]) else value
                sprintf('"%s" = "%s"', value, decode)
            } else {
                # For EnumeratedItem, just use the CodedValue
                value
            }
        })
        
        codeList_map[[oid]] <- sprintf("%s\n%s",
                                     name,
                                     paste(terms, collapse = "\n"))
    }
    
    return(list(codeList_map = codeList_map, codeLists = codeLists))
}

# Helper function to process ItemRefs
processItemRefs <- function(doc, DSName) {
    ItemRef <- NULL
    
    for (i in DSName) {
        ItemRefNode <- getNodeSet(
            doc,
            paste("//ns:ItemGroupDef[@Name ='", i, "']//ns:ItemRef", sep = ""),
            namespaces(doc)
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

        if (is.null(ItemRef)) {
            ItemRef <- tmpdf
        } else {
            ItemRef <- merge(ItemRef, tmpdf, all = T)
        }
    }
    
    return(ItemRef)
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
    doc <- xmlTreeParse(filepath, useInternalNodes = T)
    
    # Get dataset names
    ItemGroupDef <- getNodeSet(doc, "//ns:ItemGroupDef", namespaces(doc))
    DSName <- getDSName(ItemGroupDef)
    
    # Process different parts of the define.xml
    codeList_results <- processCodeLists(doc)
    codeList_map <- codeList_results$codeList_map
    codeLists <- codeList_results$codeLists
    
    ItemRef <- processItemRefs(doc, DSName)
    definitions <- processDefinitions(doc)
    method_map <- definitions$method_map
    comment_map <- definitions$comment_map
    
    # ItemRef end

    item_def <- getItemDef(doc)
    Variable.Metadata <-
        merge(ItemRef, item_def, by.x = "IR_ItemOID", by.y = "ID_OID")
    so <- order(
        Variable.Metadata$IGD_Name,
        Variable.Metadata$IR_OrderNumber
    )

    Variable.Metadata <- Variable.Metadata[so, ]
    row.names(Variable.Metadata) <- NULL

    # Create Controlled_Terms column
    Variable.Metadata$Controlled_Terms <- NA_character_
    Variable.Metadata$Display_Value <- NA_character_
    Variable.Metadata$Permitted_Value <- NA_character_  # Add Permitted_Value column

    # Cache method and comment definitions
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
            
            # Find the parent row in Variable.Metadata
            parent_idx <- which(Variable.Metadata$IGD_Name == dataset & 
                              Variable.Metadata$ID_Name == varname)
            
            if (length(parent_idx) > 0) {
                parent_row <- Variable.Metadata[parent_idx[1], ]
                
                # Check if this is a "Data Value" variable
                is_data_value <- FALSE
                parent_itemdef <- getNodeSet(doc, 
                    sprintf("//odm:ItemDef[@OID='IT.%s.%s']|//ItemDef[@OID='IT.%s.%s']", 
                            dataset, varname, dataset, varname),
                    c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
                if (length(parent_itemdef) > 0) {
                    desc_node <- getNodeSet(parent_itemdef[[1]], 
                        ".//odm:TranslatedText|.//TranslatedText",
                        c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
                    if (length(desc_node) > 0 && xmlValue(desc_node[[1]]) == "Data Value") {
                        is_data_value <- TRUE
                    }
                }
                
                itemRefs <- xmlChildren(vld)
                for (itemRef in itemRefs) {
                    if (xmlName(itemRef) == "ItemRef") {  # Only process ItemRef nodes
                        whereClauseRef <- getNodeSet(itemRef, ".//def:WhereClauseRef", 
                                                   c(def = "http://www.cdisc.org/ns/def/v2.0"))
                        
                        if (length(whereClauseRef) > 0) {
                            whereClauseOID <- xmlGetAttr(whereClauseRef[[1]], "WhereClauseOID")
                            condition_idx <- which(where_map[, "oid"] == whereClauseOID)
                            
                            new_row <- parent_row
                            if (length(condition_idx) > 0) {
                                new_row$Where_Condition <- where_map[condition_idx, "condition"]
                            }
                            
                            itemdef_oid <- xmlGetAttr(itemRef, "ItemOID")
                            method_oid <- xmlGetAttr(itemRef, "MethodOID")
                            
                            # Get ItemDef metadata
                            itemdef_nodes <- getNodeSet(doc, 
                                sprintf("//odm:ItemDef[@OID='%s']|//ItemDef[@OID='%s']", 
                                        itemdef_oid, itemdef_oid),
                                c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
                            
                            if (length(itemdef_nodes) > 0) {
                                itemdef_node <- itemdef_nodes[[1]]
                                
                                # Update mandatory flag from ItemRef
                                mandatory <- xmlGetAttr(itemRef, "Mandatory")
                                if (!is.null(mandatory)) {
                                    new_row$IR_Mandatory <- mandatory
                                }
                                
                                # Get origin information
                                origin_nodes <- getNodeSet(itemdef_node, 
                                                         ".//def:Origin",
                                                         c(def = "http://www.cdisc.org/ns/def/v2.0"))
                                
                                if (length(origin_nodes) > 0) {
                                    origin_type <- xmlGetAttr(origin_nodes[[1]], "Type")
                                    new_row$ID_OriginType <- origin_type
                                    
                                    if (origin_type == "Assigned") {
                                        comment_oid <- xmlGetAttr(itemdef_node, "def:CommentOID")
                                        if (!is.null(comment_oid)) {
                                            desc <- comment_map$Description[comment_map$CommentOID == comment_oid]
                                            if (length(desc) > 0 && !is.na(desc[1])) {
                                                new_row$ID_OriginDescription <- desc[1]
                                            }
                                        }
                                    } else if (origin_type == "Derived") {
                                        if (!is.null(method_oid)) {
                                            desc <- method_map$Description[method_map$MethodOID == method_oid]
                                            if (length(desc) > 0 && !is.na(desc[1])) {
                                                new_row$ID_OriginDescription <- desc[1]
                                            }
                                        }
                                    }
                                }
                                
                                # If this is a "Data Value" variable, get controlled terms
                                if (is_data_value) {
                                    codeListRef <- getNodeSet(itemdef_node, 
                                        ".//odm:CodeListRef|.//CodeListRef",
                                        c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
                                    if (length(codeListRef) > 0) {
                                        codeListOID <- xmlGetAttr(codeListRef[[1]], "CodeListOID")
                                        if (!is.null(codeListOID) && codeListOID %in% names(codeList_map)) {
                                            new_row$Controlled_Terms <- codeList_map[[codeListOID]]
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

    # Now process QNAM/QLABEL pairs after all other processing is done
    # Cache QNAM and QLABEL CodeLists for faster lookup
    qnam_qlab_map <- list()
    for (codeList in codeLists) {
        oid <- xmlGetAttr(codeList, "OID")
        # Process both QNAM and QLAB codelists
        if (grepl("\\$[^.]+\\.QNA[ML]", oid)) {
            items <- getNodeSet(codeList, ".//odm:CodeListItem|.//CodeListItem", 
                              c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
            
            # Store values and decodes separately
            values <- character()
            decodes <- character()
            
            for (item in items) {
                value <- xmlGetAttr(item, "CodedValue")
                decode_node <- getNodeSet(item, ".//odm:TranslatedText|.//TranslatedText",
                                        c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
                decode <- if (length(decode_node) > 0) xmlValue(decode_node[[1]]) else value
                
                values <- c(values, value)
                decodes <- c(decodes, decode)
            }
            
            if (length(values) > 0) {
                qnam_qlab_map[[oid]] <- list(values = values, decodes = decodes)
            }
        }
    }

    # Process QNAM/QLABEL pairs for each dataset
    final_rows <- list()
    
    for (dataset in unique(Variable.Metadata$IGD_Name)) {
        if (grepl("^SUPP", dataset)) {
            # Find QNAM and QLABEL rows for this dataset
            qnam_idx <- which(Variable.Metadata$IGD_Name == dataset & 
                            Variable.Metadata$ID_Name == "QNAM")
            qlab_idx <- which(Variable.Metadata$IGD_Name == dataset & 
                            Variable.Metadata$ID_Name == "QLABEL")
            
            if (length(qnam_idx) > 0 && length(qlab_idx) > 0) {
                # Get the CodeListRefs for both QNAM and QLABEL
                qnam_itemdef <- getNodeSet(doc, 
                    sprintf("//odm:ItemDef[@OID='IT.%s.QNAM']|//ItemDef[@OID='IT.%s.QNAM']", 
                            dataset, dataset),
                    c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
                qlab_itemdef <- getNodeSet(doc, 
                    sprintf("//odm:ItemDef[@OID='IT.%s.QLABEL']|//ItemDef[@OID='IT.%s.QLABEL']", 
                            dataset, dataset),
                    c(odm = "http://www.cdisc.org/ns/odm/v1.3"))
                
                if (length(qnam_itemdef) > 0 && length(qlab_itemdef) > 0) {
                    qnam_codelistref <- getNodeSet(qnam_itemdef[[1]], 
                        ".//odm:CodeListRef|.//CodeListRef|.//def:CodeListRef",
                        c(odm = "http://www.cdisc.org/ns/odm/v1.3",
                          def = "http://www.cdisc.org/ns/def/v2.0"))
                    qlab_codelistref <- getNodeSet(qlab_itemdef[[1]], 
                        ".//odm:CodeListRef|.//CodeListRef|.//def:CodeListRef",
                        c(odm = "http://www.cdisc.org/ns/odm/v1.3",
                          def = "http://www.cdisc.org/ns/def/v2.0"))
                    
                    if (length(qnam_codelistref) > 0 && length(qlab_codelistref) > 0) {
                        qnam_oid <- xmlGetAttr(qnam_codelistref[[1]], "CodeListOID")
                        qlab_oid <- xmlGetAttr(qlab_codelistref[[1]], "CodeListOID")
                        
                        if (!is.null(qnam_oid) && !is.null(qlab_oid) &&
                            qnam_oid %in% names(qnam_qlab_map)) {
                            
                            qnam_values <- qnam_qlab_map[[qnam_oid]]
                            
                            # Find QVAL rows with Where_Conditions for this dataset
                            qval_rows <- which(Variable.Metadata$IGD_Name == dataset & 
                                             Variable.Metadata$ID_Name == "QVAL" &
                                             !is.na(Variable.Metadata$Where_Condition))
                            
                            if (length(qval_rows) > 0) {
                                # Extract QNAM values from Where_Conditions
                                qnam_values_from_where <- sapply(strsplit(Variable.Metadata$Where_Condition[qval_rows], "'"), 
                                                               function(x) x[2])
                                
                                # For each QVAL row with a Where_Condition
                                for (i in seq_along(qval_rows)) {
                                    qnam_value <- qnam_values_from_where[i]
                                    # Find the index in the QNAM values list
                                    value_idx <- which(qnam_values$values == qnam_value)
                                    
                                    if (length(value_idx) > 0) {
                                        # Create a new row based on the QVAL row
                                        new_row <- Variable.Metadata[qval_rows[i], ]
                                        
                                        # Set all columns to NA except IGD_Name
                                        for (col in names(Variable.Metadata)) {
                                            if (!col %in% c("IGD_Name")) {
                                                new_row[[col]] <- NA
                                            }
                                        }
                                        
                                        # Set the Display_Value to the corresponding QLABEL value
                                        new_row$Display_Value <- qnam_values$decodes[value_idx]
                                        # Set the Permitted_Value to the corresponding QNAM value
                                        new_row$Permitted_Value <- qnam_values$values[value_idx]
                                        # Set ID_Name to indicate this is a QNAM/QLABEL value list row
                                        new_row$ID_Name <- "QNAM/QLABEL Value List"
                                        
                                        # Add to final_rows
                                        final_rows[[length(final_rows) + 1]] <- new_row
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    # Add the final rows to Variable.Metadata
    if (length(final_rows) > 0) {
        # Ensure all columns match
        final_df <- do.call(rbind, lapply(final_rows, function(row) {
            # Make sure row has all columns from Variable.Metadata
            missing_cols <- setdiff(names(Variable.Metadata), names(row))
            if (length(missing_cols) > 0) {
                for (col in missing_cols) {
                    row[[col]] <- NA
                }
            }
            # Ensure column order matches
            row[names(Variable.Metadata)]
        }))
        
        # Find QLABEL rows for each dataset
        datasets <- unique(final_df$IGD_Name)
        new_variable_metadata <- list()
        
        # Process each dataset separately
        for (dataset in datasets) {
            # Get rows for this dataset
            dataset_rows <- Variable.Metadata[Variable.Metadata$IGD_Name == dataset, ]
            dataset_final_rows <- final_df[final_df$IGD_Name == dataset, ]
            
            # Find QLABEL row index - take the first one if there are multiple
            qlab_idx <- which(dataset_rows$ID_Name == "QLABEL")[1]
            
            if (!is.null(qlab_idx) && length(qlab_idx) > 0) {
                # Split the dataset rows at QLABEL
                before_qlabel <- dataset_rows[seq_len(qlab_idx), ]
                after_qlabel <- dataset_rows[seq(qlab_idx + 1, nrow(dataset_rows)), ]
                
                # Combine parts with final rows in the middle
                dataset_rows <- rbind(before_qlabel, dataset_final_rows, after_qlabel)
            }
            
            new_variable_metadata[[dataset]] <- dataset_rows
        }
        
        # Combine all datasets back together
        Variable.Metadata <- do.call(rbind, new_variable_metadata)
        # Reset row names to be sequential
        rownames(Variable.Metadata) <- NULL
    }
    
    return(Variable.Metadata)
}

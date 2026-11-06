test_that("codelist extraction preserves decoded text and selects NCI aliases", {
    file <- tempfile(fileext = ".xml")
    on.exit(unlink(file))
    writeLines(paste0(
        '<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3" xmlns:def="http://www.cdisc.org/ns/def/v2.0">',
        '<Study OID="S"><MetaDataVersion OID="M" Name="M">',
        '<CodeList OID="CL.CI" Name="Decoded" DataType="text">',
        '<CodeListItem CodedValue="Y"><Decode><TranslatedText xml:lang="en">Yes</TranslatedText>',
        '<TranslatedText xml:lang="zh">是</TranslatedText></Decode>',
        '<Alias Context="other" Name="wrong"/><Alias Context="nci:ExtCodeID" Name="C1"/>',
        '</CodeListItem><Alias Context="nci:ExtCodeID" Name="CL1"/></CodeList>',
        '<CodeList OID="CL.EN" Name="Enumerated" DataType="text">',
        '<EnumeratedItem CodedValue="N"><Alias Context="other" Name="wrong"/>',
        '<Alias Context="nci:ExtCodeID" Name="C2"/></EnumeratedItem>',
        '</CodeList></MetaDataVersion></Study></ODM>'
    ), file)

    result <- getCT(file)
    expect_identical(result$OID, c("CL.CI", "CL.EN"))
    expect_identical(result$ItemCode, c("C1", "C2"))
    expect_identical(result$CodeListCode, c("CL1", NA_character_))
    expect_identical(result$Decode, c("Yes是", NA_character_))
    expect_named(result, c("OID", "Name", "DataType", "SASFormatName", "CodeListCode", "CodeListContext",
                          "CodedValue", "OrderNumber", "Rank", "ExtendedValue", "ItemCode", "ItemContext", "Decode"))
})

test_that("duplicate OIDs retain existing document-wide codelist matching", {
    file <- tempfile(fileext = ".xml")
    on.exit(unlink(file))
    writeLines(paste0(
        '<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3" xmlns:def="http://www.cdisc.org/ns/def/v2.0"><Study OID="S">',
        '<MetaDataVersion OID="M1" Name="M1"><CodeList OID="CL.SHARED" Name="Decoded" DataType="text">',
        '<CodeListItem CodedValue="A"><Decode><TranslatedText>A</TranslatedText></Decode></CodeListItem>',
        '<Alias Context="nci:ExtCodeID" Name="C1"/></CodeList></MetaDataVersion>',
        '<MetaDataVersion OID="M2" Name="M2"><CodeList OID="CL.SHARED" Name="Enumerated" DataType="text">',
        '<EnumeratedItem CodedValue="B"/><Alias Context="nci:ExtCodeID" Name="C2"/></CodeList>',
        '</MetaDataVersion></Study></ODM>'
    ), file)
    result <- getCT(file)
    expect_identical(result$CodedValue, c("A", "A", "B", "B"))
    expect_identical(result$CodeListCode, c("C1", "C2", "C1", "C2"))
    expect_identical(result$Decode, c("A", "A", NA_character_, NA_character_))
})

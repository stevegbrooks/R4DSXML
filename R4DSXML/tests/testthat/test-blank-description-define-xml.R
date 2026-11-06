library(R4DSXML)
library(testthat)


## Test metadata preparation 
define <- test_path("fixtures", "define-blank-descriptions.xml")


#Get variable metadata
var.md <- R4DSXML::getVarMD(define)

#Get value level metadata
val.md <- R4DSXML::getValMD(define)


# Check Variable Metadata
test_that("missing and empty variable descriptions retain the metadata rows", {
    expect_identical(var.md$ID_Label[var.md$IR_ItemOID == "IT.DM.AGE"], NA_character_)
    expect_identical(var.md$ID_Label[var.md$IR_ItemOID == "IT.DM.SEX"], NA_character_)
})

# Check Value Level Metadata
test_that("missing and empty value descriptions retain the metadata rows", {
    expect_identical(val.md$ID_Label[val.md$IR_ItemOID == "IT.SC.SCORRES.MARISTAT"], NA_character_)
    expect_identical(val.md$ID_Label[val.md$IR_ItemOID == "IT.VS.VSORRESU.WEIGHT.DM.COUNTRY.CMETRIC"], NA_character_)
})



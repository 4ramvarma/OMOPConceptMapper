# OMOPConceptMapper

An R package for mapping ICD codes to SNOMED concepts and querying comprehensive concept relationships in OMOP Common Data Model (CDM) databases.

## Features

- **ICD to SNOMED Mapping**: Map ICD9CM and ICD10CM codes to SNOMED concepts
- **Flexible Pattern Matching**: Support for both LIKE patterns and exact code matching
- **Comprehensive Relationship Queries**: Retrieve hierarchical relationships, direct relationships, and synonyms
- **Well-Documented**: Extensive roxygen2 documentation for all functions
- **Modular Design**: Clean separation of concerns with utility, mapping, and relationship functions
- **Complete Workflow**: Convenience functions for end-to-end workflows

## Installation

```r
# Install from GitHub
devtools::install_github("4ramvarma/OMOPConceptMapper")
```

## Prerequisites

Set the following environment variables before using the package:

```r
Sys.setenv(SERVER_SERVERLESS = "your-server-address")
Sys.setenv(USERNAME = "your-username")
Sys.setenv(PASSWORD = "your-password")
```

## Quick Start

```r
library(OMOPConceptMapper)

# 1. Create database connection
connection <- create_db_connection()

# 2. Map ICD codes to SNOMED concepts
mappings <- map_icd_to_snomed(
  connection = connection,
  cdm_schema = "healthverity_marketplace_omop_20250331",
  pattern_type = "like",
  pattern_values = "C16%"
)

# 3. Extract SNOMED IDs
snomed_ids <- extract_snomed_ids(mappings)

# 4. Query concept relationships
relationships <- query_concept_relationships(
  snomed_ids = snomed_ids,
  cdm_schema = "healthverity_marketplace_omop_20250331",
  connection = connection
)
```

## License

MIT

## Author

Ram Varma

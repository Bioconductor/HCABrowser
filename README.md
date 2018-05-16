This package is a work in progress

Goal: navigate the HCA data portal, allowing users to discover
available data sets.

- Possible model: [GenomicDataCommons][]

- API: https://dss.data.humancellatlas.org/
- schema https://github.com/HumanCellAtlas/metadata-schema
- typical tasks: https://github.com/HumanCellAtlas/data-consumer-vignettes

- Implementation: httr, rjson, curl, xml2
  - GET(), POST(), PATCH(), HEAD()
  - authentication

```
library(httr)
library(dplyr)
body <- '{
  "es_query": {}
}'

base = "https://dss.data.humancellatlas.org/v1/%s"
url <- sprintf(base, "search?output_format=summary&replica=aws&per_page=100")
headers = add_headers(
    accept = "application/json",
    `Content-Type` = "application/json"
)
response <- httr::POST(url, headers, body = body)
stop_for_status(response)
lol <- content(response)
bind_rows(lol[[2]])
```

[GenomicDataCommons]: https://github.com/Bioconductor/GenomicDataCommons


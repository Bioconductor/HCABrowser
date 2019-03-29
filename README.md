
The [Human Cell Atlas] (HCA) (https://www.humancellatlas.org/) was created in
order to create comprehensive reference maps of all human cells as a basis for
both understanding human health and diagnosing, monitoring, and treating
disease. The *HCABrowser* Biocondctor pacakge provides infrastructure for
searching for, queerying, and accessing data help on the Human Cell Atlas's
Data Coordination Platform (https://dss.data.humancellatlas.org/). Further
changes to the package are planned to incorperate higer level functionality to
upload user generated data to the the Human Cell Atlas platform.

This is a Biocondcutor package and can be installed using `BiocManager`.
```
## Install Bioconductor
if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("HCABrowser")

library(HCABrowser)
```

This package introduces the `HCABrowser` object used to connect to and
perform opertions on the Human Cell Atlas
```
hca <- HCABrowser()
hca
```

The Human Cell Atlas Data Portal requires queries to be submitted against
their schema. Given the schema is complicated and users may not want to
learn it in order to search the Human Cell Atlas, we seek to offer an easier
way for the user to explore the Human Cell Atlas.

First we want to see which fields and values of fields are available.
```
## Show fields and their respective abbreviations
hca %>% fields

## Show availiable values for a field (e.g. organ.text)
hca %>% values(organ.text)

## OR show all values for all fields
hca %>% values
```

The `filter()` and `select()` methods are used to query the Human Cell Atlas.

```
## Search for bundles regarding blood and that were no constructed with Smart-Seq2
hca <- hca %>% filter(organ.text == "blood" && library_construction_approach.text != "Smart-Seq2")
hca

## Include columns for 'project_title' and 'project_shortname'
hca <- hca %>% select(c('project_title', 'project_shortname'))
hca
```

Once the data bundles of interest are found, they can be downloaded and used to
obtain data. We then use these bundles to download the expression matrices using
the HCAMatrixBrowser package.
```
bundles <- hca %>% pullBundles

if (!requireNamespace("HCAMatrixBrowser"))
    BiocManager::install("HCAMatrixBrowser")

library(HCAMatrixBrowser)

loadHCAMatrix(bundles)
```

For my information, please refer to the package's vignette.

As with other Biocondcutor packages, the `master` branch indicates the
development branch and the `RELEASE_X_X` branch indicates the current
release branch.

- API: https://dss.data.humancellatlas.org/
- schema https://github.com/HumanCellAtlas/metadata-schema
- typical tasks: https://github.com/HumanCellAtlas/data-consumer-vignettes


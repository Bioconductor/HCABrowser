> library(HCABrowser)


> hca <- HCABrowser()

# Add to filter (functions like json)
> hca <- hca %>% filter('files.project_json.project_core.project_title' == 'Census of Immume Cells')

# Use ropenapi to access search function
> ret <- hca$Find_bundles_by_searching_their_metadata_with_an_Elasticsearch_query(getEsQuery(hca), replica='aws', output_format='raw')

# Prepare SearchResult object
> ret <- parseToSearchResults(ret)
> res <- results(ret)

# Process SearchResult object, fetch file, save results file to BiocFileCache
> for(bundles in res) {
    file <- bundles[['metadata']][['manifest']][['files']]
    if (endsWith('results', file)) {
        file <- hca$`Retrieve_a_file_given_a_UUID_and_optionally_a_version.`(uuid=file, replica='aws')
        saveToBiocFileCache(file)
    }
}


query = {
    "query": {
        "bool": {
            "must": [
            {
                "match": {
                    "files.project_json.project_core.project_title": "Census of Immune Cells"
                }
            }
            ]
        }
    }
}

bundles = client.post_search(es_query=query, replica='aws', output_format='raw')
the_first_bundle = bundles['results'][0]
bundle_files = the_first_bundle['metadata']['manifest']['files']
for f in bundle_files:
    if f['name'].endswith('.results'):
        results_file_uuid, results_file_version = f['uuid'], f['version']
print(results_file_uuid, results_file_version)

results = client.get_file(replica='aws', uuid=results_file_uuid, version=results_file_version)

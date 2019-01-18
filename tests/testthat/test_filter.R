
test_that(".hca_filter_loop(x) works", {
    obs <- .hca_filter_loop(list(), quo(organ.text == foo))
    exp <- list(
        filter = list(list(
            term = list(files.specimen_from_organism_json.organ.text = "foo")
        ))
    )
    expect_equal(obs, exp)
})

test_that(".hca_filter_loop(x & y) works", {
    obs <- .hca_filter_loop(list(), quo(organ.text == foo & organ.ontology == bar))
    exp <- list(
        filter = list(list(
            bool = list(
                filter = list(
                    list(
                        term = list(files.specimen_from_organism_json.organ.text = "foo")
                    ), 
                    list(
                        term = list(files.specimen_from_organism_json.organ.ontology = "bar")
                    )
                )
            )
        ))
    )
    expect_equal(obs, exp)
})

test_that(".hca_filter_loop(x | y) works", {
    obs <- .hca_filter_loop(list(), quo(organ.text == foo | organ.ontology == bar))
    exp <- list(
        filter = list(list(
            bool = list(
                should = list(
                    list(
                        term = list(files.specimen_from_organism_json.organ.text = "foo")
                    ), 
                    list(
                        term = list(files.specimen_from_organism_json.organ.ontology = "bar")
                    )
                )
            )
        ))
    )
    expect_equal(obs, exp)
})

test_that(".hca_filter_loop((x)) works", {
    obs <- .hca_filter_loop(list(), quo((organ.text == foo)))
    exp <- list(
        filter = list(list(
            bool = list(
                filter = list(list(
                    term = list(files.specimen_from_organism_json.organ.text = "foo")
                ))
            )
        ))
    )
    expect_equal(obs, exp)
})

test_that(".hca_filter_loop(!x) works", {
    obs <- .hca_filter_loop(list(), quo(!organ.text == foo))
    exp <- list(
        filter = list(list(
            bool = list(
                must_not = list(list(
                    term = list(files.specimen_from_organism_json.organ.text = "foo")
                ))
            )
        ))
    )
    expect_equal(obs, exp)
})

test_that("filter(x & y) works", { 
    obs <- .temp(quos(organ.text == foo & organ.ontology == bar))
    exp <- list(
        es_query = list(
            query = list(
                bool = list(
                    filter = list(list(
                        bool = list(
                            filter = list(
                                list(
                                    term = list(files.specimen_from_organism_json.organ.text = "foo")
                                ),
                                list(
                                    term = list(files.specimen_from_organism_json.organ.ontology = "bar")
                                )
                            )
                        )
                    ))
                )
            )
        )
    )
    expect_equal(obs, exp)
})

test_that("filter(x | y) works", { 
    obs <- .temp(quos(organ.text == foo | organ.ontology == bar))
    exp <- list(
        es_query = list(
            query = list(
                bool = list(
                    filter = list(list(
                        bool = list(
                            should = list(
                                list(
                                    term = list(files.specimen_from_organism_json.organ.text = "foo")
                                ),
                                list(
                                    term = list(files.specimen_from_organism_json.organ.ontology = "bar")
                                )
                            )
                        )
                    ))
                )
            )
        )
    )
    expect_equal(obs, exp)
})

test_that("filter((x)) works", { 
    obs <- .temp(quos((organ.text == foo)))
    exp <- list(
        es_query = list(
            query = list(
                bool = list(
                    filter = list(list(
                        bool = list(
                            filter = list(
                                list(
                                    term = list(files.specimen_from_organism_json.organ.text = "foo")
                                )
                            )
                        )
                    ))
                )
            )
        )
    )
    expect_equal(obs, exp)
})

test_that("filter(!x) works", { 
    obs <- .temp(quos(!organ.text == foo))
    exp <- list(
        es_query = list(
            query = list(
                bool = list(
                    filter = list(list(
                        bool = list(
                            must_not = list(
                                list(
                                    term = list(files.specimen_from_organism_json.organ.text = "foo")
                                )
                            )
                        )
                    ))
                )
            )
        )
    )
    expect_equal(obs, exp)
})

test_that("filter((!x)) works", { 
    obs <- .temp(quos((!organ.text == foo)))
    exp <- list(
        es_query = list(
            query = list(
                bool = list(
                    filter = list(list(
                        bool = list(
                            filter = list(list(
                                bool = list(
                                    must_not = list(
                                        list(
                                            term = list(files.specimen_from_organism_json.organ.text = "foo")
                                        )
                                    )
                                )
                            ))
                        )
                    ))
                )
            )
        )
    )
    expect_equal(obs, exp)
})

test_that("filter(x, y) works", { 
    obs <- .temp(quos(organ.text == foo, organ.ontology == bar))
    exp <- list(
        es_query = list(
            query = list(
                bool = list(
                    filter = list(list(
                        bool = list(
                            filter = list(
                                list(
                                    term = list(files.specimen_from_organism_json.organ.text = "foo")
                                )
                            )
                        )),
                        list(
                            term = list(files.specimen_from_organism_json.organ.ontology = "bar")
                        )
                    )
                )
            )
        )
    )
    expect_equal(obs, exp)
})


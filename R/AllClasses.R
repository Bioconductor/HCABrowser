###' @importFrom tibble tibble
setClass("HumanCellAtlas",
#    contains = "tibble",
    slots = c(
        url = "character"
    )
)

.HumanCellAtlas <-
    function(url)
{
    new("HumanCellAtlas", url=url)
}

HumanCellAtlas <-
    function(url='https://dss.integration.data.humancellatlas.org/v1')
{
    .HumanCellAtlas(url)
}

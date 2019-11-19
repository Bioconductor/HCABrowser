## A script to generate the openapi package and migrate the needed files to the HCABrowser package.

openapi-generator generate -i https://dss.data.humancellatlas.org/v1/swagger.json -g r -o /tmp/HCABrowser --package-name HCABrowser
rm -f ../../R/openapi.R
cat /tmp/HCAClient/R/* > ../../R/openapi.R
cp -r /tmp/HCAClient/tests/testthat ../../tests/testthat/*

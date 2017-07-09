# PlateReaderDataAnalyzer
An R+Shiny based web solution for analyzing plate reader data online

Visit [the online version of the project](https://milolab.shinyapps.io/PlateReaderDataAnalyzer/) to see it at work and read the detailed documentation.

This Software As A Service (SAAS) tool aims at facilitating the analysis of growth curves from plate-reader generated data.
Being a SAAS service, it does not require installation of any software and gives immediate analysis results.
This tool currently focuses on growth rate analysis, allowing the user to tweak parameters for the growth rate calculation model and see the resulting analysis.
Being an open source tool, contributors are encouraged to add other analysis types to the service.


## Local installation
You may want to download and run the program locally, either for development purposes, or to gain better performance.
To do so you'll need, on top of the packages listed below, to install the rCharts package from github.
In ubuntu/debian this will require installing the devtools package, which depends on libcurl.
First we install libcurl using the shell:
```bash
    apt-get -y install libcurl4-gnutls-dev
```

Then we can install devtools and rCharts:

```R
    R
    >install.packages('devtools')
    >require('devtools')
    >install_github('rCharts','ramnathv')
```

The other packages the program depends on (all installable using `packages.install('...')`) are:
reshape2, gdata, shiny, shinyBS, DT, rhandsontable, zoo, foreach




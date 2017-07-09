# PlateReaderDataAnalyzer
An R+Shiny based web solution for analyzing plate reader data online

Visit [the online version of the project](https://milolab.shinyapps.io/PlateReaderDataAnalyzer/) to see it at work and read the detailed documentation.

This Software As A Service (SAAS) tool aims at facilitating the analysis of growth curves from plate-reader generated data.
Being a SAAS service, it does not require installation of any software and gives immediate analysis results.
This tool currently focuses on growth rate analysis, allowing the user to tweak parameters for the growth rate calculation model and see the resulting analysis.
Being an open source tool, contributors are encouraged to add other analysis types to the service.


## Local installation
You may want to download and run the program locally, either for development purposes, or to gain better performance.
This program uses [packrat](https://rstudio.github.io/packrat/) which should facilitate installing all the necessary dependencies.
Once cloning to your own dir, simply start R in that directory and all the dependencies should be automatically downloaded and installed.

### Local installation without packrat
If, for some reason, packrat installation failed, or you prefer installing dependencies manually, you should install the following packages from CRAN using `install.packages('...')`:
reshape2, gdata, shiny, shinyBS, DT, rhandsontable, zoo, foreach

On top of these packages, you'll need to install the rCharts package from github.
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
This should conclude the installation process, you should now be able to start R and run `shiny::runApp()` to start the server locally.

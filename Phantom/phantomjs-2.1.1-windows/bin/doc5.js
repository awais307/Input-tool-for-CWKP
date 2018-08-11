var page = require('webpage').create();
                         page.open('https://www.ncbi.nlm.nih.gov/pubmed/17709244', function () {
                         console.log(page.content); //page source
                         phantom.exit();
                         });

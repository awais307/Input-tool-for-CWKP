var page = require('webpage').create();
                         page.open('https://www.ncbi.nlm.nih.gov/pubmed/21977661', function () {
                         console.log(page.content); //page source
                         phantom.exit();
                         });

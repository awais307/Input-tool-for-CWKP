var page = require('webpage').create();
                         page.open('https://www.ncbi.nlm.nih.gov/pubmed/22049731', function () {
                         console.log(page.content); //page source
                         phantom.exit();
                         });

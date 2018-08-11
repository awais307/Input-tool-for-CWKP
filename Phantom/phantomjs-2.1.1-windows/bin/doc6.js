var page = require('webpage').create();
                         page.open('https://www.sciencedirect.com/science/article/pii/S0011916409004275', function () {
                         console.log(page.content); //page source
                         phantom.exit();
                         });

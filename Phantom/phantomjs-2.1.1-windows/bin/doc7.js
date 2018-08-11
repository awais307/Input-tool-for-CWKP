var page = require('webpage').create();
                         page.open('https://www.sciencedirect.com/science/article/pii/S0925857402000149', function () {
                         console.log(page.content); //page source
                         phantom.exit();
                         });

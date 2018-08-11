var page = require('webpage').create();
                         page.open('https://www.sciencedirect.com/science/article/pii/S0925857412000419', function () {
                         console.log(page.content); //page source
                         phantom.exit();
                         });

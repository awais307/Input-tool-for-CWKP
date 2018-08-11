var page = require('webpage').create();
                         page.open('https://www.sciencedirect.com/science/article/pii/S1537511005001923', function () {
                         console.log(page.content); //page source
                         phantom.exit();
                         });

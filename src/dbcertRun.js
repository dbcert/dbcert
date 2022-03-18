const fs = require('fs');
const path = require('path');
const args = process.argv.slice(2);

if (args[0] && args[1]) {
    const query = require(path.resolve(args[0])).query;
    const { queryExec } = require("./runtime/queryExec");

    var db = JSON.parse(fs.readFileSync(path.resolve(args[1]),'utf8'));
    console.log(JSON.stringify(queryExec(query,db)));
} else {
    throw new Error('Missing compiled SQL argument or Database');
}

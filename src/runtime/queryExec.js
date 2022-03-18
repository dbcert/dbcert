/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/** Copyright 2019 : Jerome Simeon */

function inputRow(tableName,row) {
    const output = {};
    for (column in row) {
        output[tableName + '.' + column] = row[column] ? { '$left' : row[column] } : { '$right' : null };
    }
    return output;
}
function inputTable(tableName, table) {
    const outputTable = table.map(r => { return inputRow(tableName,r); });
    return { '$coll' : outputTable, '$length' : outputTable.length };
}
function inputDB(db) {
    const output = {};
    for (tableName in db) {
        output[tableName] = inputTable(tableName, db[tableName]);
    }
    return output;
}
function outputRow(row) {
    const output = {};
    for (column in row) {
        // XXX Funky column name thing
        let newColumn;
        try {
            newColumn = column.split('_').pop();
        } catch(err) {
            newColumn = column;
        }
        output[newColumn] =
            Object.prototype.hasOwnProperty.call(row[column],'$left')
            ? row[column]['$left'] : row[column]['$right'];
    }
    return output;
}
function outputTable(table) {
    const outputTable = [];
    for (let i = 0; i < table.$length; i++) {
        outputTable.push(outputRow(table.$coll[i]))
    }
    return outputTable;
}

function queryExec(query, db) {
    return outputTable(query(inputDB(db)));
}

module.exports = { queryExec, inputDB, inputTable, outputTable };

/*
var db = { employees : [
    { name: "John",
      age: { $nat: 32 } },
    { name: "Jim",
      age: { $nat: 32 } },
    { name: null,
      age: { $nat: 32 } }
]};
*/
//console.log(JSON.stringify(db));
//console.log(JSON.stringify(inputDB(db)));


const fs = require('fs');

function getFiles(dir) {
    const fileList = fs.readdirSync(dir);
    return fileList.map(file => `${dir}/${file}`)
        .filter(file => fs.statSync(file).isFile());
}

const fileNames = getFiles('./src/Solutions');
var fileContent = new Array(99).fill("-- No solution at this index. Something went wrong.");

const ignoreFirstLines = 2;

for (const fileName of fileNames) {
    const problemNumber = parseInt(fileName.replace(/^\D+|\D+$/g, ""));
    fileContent[problemNumber] = fs.readFileSync(fileName, 'utf8').split("\n").slice(ignoreFirstLines).join("\n");
}

fs.writeFileSync('./public/solutionsCode.js', "const solutions=" + JSON.stringify(fileContent));


const https = require("https");


https.get("https://adventofcode.com/2021/day/1/input", resp => {
    let data = "";

    resp.on("data", chunk => {
        data += chunk;
    })

    resp.on("end", () => {
        console.log(data);
    })
})
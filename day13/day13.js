"use strict";
exports.__esModule = true;
var fs_1 = require("fs");
var Ord;
(function (Ord) {
    Ord[Ord["GT"] = 0] = "GT";
    Ord[Ord["LT"] = 1] = "LT";
    Ord[Ord["EQ"] = 2] = "EQ";
})(Ord || (Ord = {}));
function lexocog(l1, l2) {
    if (typeof l1 === "number" && typeof l2 === "number") {
        if (l1 < l2)
            return Ord.LT;
        if (l1 > l2)
            return Ord.GT;
        return Ord.EQ;
    }
    else if (typeof l1 === "number") {
        return lexocog([l1], l2);
    }
    else if (typeof l2 === "number") {
        return lexocog(l1, [l2]);
    }
    else {
        for (var i = 0; i < Math.min(l1.length, l2.length); i++) {
            var l = l1[i];
            var r = l2[i];
            var result = lexocog(l, r);
            if (result == Ord.LT || result == Ord.GT)
                return result;
            else
                continue;
        }
        return lexocog(l1.length, l2.length);
    }
}
function part1(input) {
    var pairs = input.split("\n\n").map(function (block) { return block.split("\n").map(function (line) { return JSON.parse(line); }); });
    var t = pairs.map(function (p) { return lexocog(p[0], p[1]); });
    var indexSums = t.reduce(function (acc, el, ind) { return (el == Ord.LT ? acc + (ind + 1) : acc); }, 0);
    console.log(indexSums);
}
function part2(input) {
    var pairs = input.split("\n\n").map(function (block) { return block.split("\n").map(function (line) { return JSON.parse(line); }); });
    var lists = pairs.flat();
    lists.push([[2]]);
    lists.push([[6]]);
    var sorted = lists.sort(function (l1, l2) {
        var result = lexocog(l1, l2);
        if (result === Ord.LT)
            return -1;
        if (result === Ord.EQ)
            return 0;
        if (result === Ord.GT)
            return 1;
        throw "Unreachable";
    });
    var stringified = sorted.map(function (e) { return JSON.stringify(e); });
    return (stringified.indexOf("[[2]]") + 1) * (stringified.indexOf("[[6]]") + 1);
}
var input = fs_1.readFileSync("input.txt").toString("utf-8");
console.log(part1(input));
console.log(part2(input));

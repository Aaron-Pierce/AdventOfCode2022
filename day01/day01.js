"use strict";
var __spreadArray = (this && this.__spreadArray) || function (to, from) {
    for (var i = 0, il = from.length, j = to.length; i < il; i++, j++)
        to[j] = from[i];
    return to;
};
Object.defineProperty(exports, "__esModule", { value: true });
var fs_1 = require("fs");
var input = fs_1.readFileSync("input.txt").toString();
var lines = input.split("\n");
var blocks = input.split("\n\n");
var ints = lines.map(function (e) { return parseInt(e); });
var floats = lines.map(function (e) { return parseFloat(e); });
var chars = lines.map(function (e) { return e.split(""); });
var sorted_ints = __spreadArray([], ints).sort(function (a, b) { return a - b; });
var sorted_floats = __spreadArray([], floats).sort(function (a, b) { return a - b; });
var a = 0;
var first = ints[0];
var max = 0;
var summ = 0;
var sums = [];
for (var i = 0; i < blocks.length; i++) {
    var block = blocks[i];
    var sum = block.split("\n").map(function (e) { return parseInt(e); }).reduce(function (acc, el) { return acc + el; });
    sums.push(sum);
    summ += sum;
    console.log(summ);
    if (sum > max) {
        max = sum;
        console.log(max);
    }
}
console.log(summ);
var top = sums.sort(function (a, b) { return b - a; });
console.log(sums[0] + sums[1] + sums[2], "asfeasafwe");
for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    var int = ints[i];
}

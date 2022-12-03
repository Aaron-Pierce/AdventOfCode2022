"use strict";
exports.__esModule = true;
var fs_1 = require("fs");
var input = fs_1.readFileSync("input.txt").toString();
function part1() {
    var rucksacks = input.split("\n").filter(function (e) { return e; });
    var priority = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    var sum = 0;
    var _loop_1 = function (sack) {
        var components = [sack.substring(0, sack.length / 2), sack.substring(sack.length / 2, sack.length)];
        var sets = [new Set(components[0]), new Set(components[1])];
        var intersection = new Set(Array.from(sets[0]).filter(function (e) { return sets[1].has(e); }));
        if (intersection.size !== 1) {
            console.warn("Intersection was more than just one element");
            console.log(sack, intersection);
        }
        else {
            sum += priority.indexOf(intersection.values().next().value);
        }
    };
    for (var _i = 0, rucksacks_1 = rucksacks; _i < rucksacks_1.length; _i++) {
        var sack = rucksacks_1[_i];
        _loop_1(sack);
    }
    return sum;
}
console.log(part1());
function intersect(a, b) {
    return new Set(Array.from(a).filter(function (e) { return b.has(e); }));
}
function part2() {
    var rucksacks = input.split("\n").filter(function (e) { return e; });
    var priority = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    var sum = 0;
    for (var i = 0; i < rucksacks.length; i += 3) {
        var sacks = [rucksacks[i], rucksacks[i + 1], rucksacks[i + 2]];
        var sets = sacks.map(function (e) { return new Set(e); });
        var intersection = sets.reduce(function (acc, el) { return intersect(acc, el); }, sets[0]);
        if (intersection.size !== 1) {
            console.warn("Group intersection was not a single element", sacks, sets);
        }
        else {
            sum += priority.indexOf(intersection.values().next().value);
        }
    }
    return sum;
}
console.log(part2());

"use strict";
exports.__esModule = true;
var fs_1 = require("fs");
var input = fs_1.readFileSync("input.txt").toString();
var lines = input.split("\n").filter(function (e) { return e; });
function part1() {
    var score = 0;
    var move_scores = {
        "X": 1,
        "Y": 2,
        "Z": 3
    };
    var opponent_move_indicies = ["A", "B", "C"];
    var your_move_indicies = ["X", "Y", "Z"];
    for (var _i = 0, lines_1 = lines; _i < lines_1.length; _i++) {
        var l = lines_1[_i];
        var _a = l.split(" "), opponent_move = _a[0], your_move = _a[1];
        score += move_scores[your_move];
        var outcome = your_move_indicies.indexOf(your_move) - opponent_move_indicies.indexOf(opponent_move);
        if (outcome === 0) {
            score += 3;
        }
        else if (outcome === 1 || outcome === -2) {
            score += 6;
        }
        else {
            score += 0;
        }
    }
    return score;
}
console.log(part1());
function negative_mod(x, n) {
    return ((x % n) + n) % n;
}
function part2() {
    var score = 0;
    var move_scores = {
        "A": 1,
        "B": 2,
        "C": 3
    };
    var outcome_scores = {
        "X": 0,
        "Y": 3,
        "Z": 6
    };
    var outcome_shifts = {
        "X": -1,
        "Y": 0,
        "Z": 1
    };
    var opponent_move_indicies = ["A", "B", "C"];
    for (var _i = 0, lines_2 = lines; _i < lines_2.length; _i++) {
        var l = lines_2[_i];
        var _a = l.split(" "), opponent_move = _a[0], outcome = _a[1];
        score += outcome_scores[outcome];
        var opponent_move_index = opponent_move_indicies.indexOf(opponent_move);
        var choice = opponent_move_indicies[negative_mod(opponent_move_index + outcome_shifts[outcome], 3)];
        score += move_scores[choice];
    }
    return score;
}
console.log(part2());

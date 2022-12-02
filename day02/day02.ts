import {readFileSync} from 'fs';


let input = readFileSync("input.txt").toString();
let lines = input.split("\n").filter(e => e);

function part1(){
    let score = 0;

    let move_scores = {
        "X": 1,
        "Y": 2,
        "Z": 3
    }

    let opponent_move_indicies = ["A", "B", "C"];
    let your_move_indicies = ["X", "Y", "Z"];

    for(let l of lines){
        let [opponent_move, your_move] = l.split(" ") as ["A" | "B" | "C", "X" | "Y" | "Z"];
        score += move_scores[your_move];
        
        let outcome = your_move_indicies.indexOf(your_move) - opponent_move_indicies.indexOf(opponent_move);
        
        if(outcome === 0){
            score += 3;
        } else if (outcome === 1 || outcome === -2){
            score += 6;
        } else {
            score += 0;
        }
    }

    return score;
}

console.log(part1());


function negative_mod(x: number, n: number){
    return ((x % n) + n) % n;
}

function part2(){
    
    let score = 0;

    let move_scores = {
        "A": 1,
        "B": 2,
        "C": 3
    }

    let outcome_scores = {
        "X": 0,
        "Y": 3,
        "Z": 6
    }

    let outcome_shifts = {
        "X": -1,
        "Y": 0,
        "Z": 1
    }

    let opponent_move_indicies: ["A", "B", "C"] = ["A", "B", "C"];

    for(let l of lines){
        let [opponent_move, outcome] = l.split(" ") as ["A" | "B" | "C", "X" | "Y" | "Z"];
        score += outcome_scores[outcome];

        let opponent_move_index = opponent_move_indicies.indexOf(opponent_move);
        let choice = opponent_move_indicies[negative_mod(opponent_move_index + outcome_shifts[outcome], 3)];
        score += move_scores[choice];
    }

    return score;
}
console.log(part2());

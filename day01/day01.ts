import { exec, execSync } from "child_process";
import { readFileSync, writeFileSync } from "fs";


let input = readFileSync("input.txt").toString();

// Quite gross for leaderboard attempt.
// Did okay - 60/100 on part 1 and 240something/100 on part 2,
// places me 84 overall which is okay.


let lines = input.split("\n");
let blocks = input.split("\n\n");
let ints = lines.map(e => parseInt(e));
let floats = lines.map(e => parseFloat(e));
let chars = lines.map(e => e.split(""));
let sorted_ints = [...ints].sort((a, b) => a - b);
let sorted_floats = [...floats].sort((a, b) => a - b);


let a = 0;
let first = ints[0];

let max = 0;
let summ = 0;

let sums = [];

for(let i = 0; i < blocks.length; i++){
    let block = blocks[i];
    let sum = block.split("\n").map(e => parseInt(e)).reduce((acc, el) => acc + el);
    sums.push(sum);
    summ += sum;
    console.log(summ);
    
    if(sum > max){
        max = sum;
        console.log(max);
        
    }

}
console.log(summ);



let top = sums.sort((a, b) => b - a)


console.log(sums[0] + sums[1] + sums[2], "asfeasafwe");


for(let i = 0; i < lines.length; i++){
    let line = lines[i];
    let int = ints[i];




}






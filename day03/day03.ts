import {readFileSync} from "fs";

let input = readFileSync("input.txt").toString();


function intersect(a: Set<string>, b: Set<string>): Set<string> {
    return new Set(Array.from(a).filter(e => b.has(e)));
}

function part1(){
    let rucksacks = input.split("\n").filter(e => e);
    let priority = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let sum = 0;
    for (let sack of rucksacks){
        let components = [sack.substring(0, sack.length / 2), sack.substring(sack.length / 2, sack.length)];
        let sets = components.map(e => new Set(e));
        let intersection = intersect(sets[0], sets[1]);
        if(intersection.size !== 1){
            console.warn("Intersection was more than just one element");
            console.log(sack, intersection);
        } else {
            sum += priority.indexOf(intersection.values().next().value)
        }
    }
    return sum;
}

console.log(part1());


function part2(){
    let rucksacks = input.split("\n").filter(e => e);
    let priority = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let sum = 0;
    for (let i = 0; i < rucksacks.length; i += 3){
        let sacks = [rucksacks[i], rucksacks[i + 1], rucksacks[i + 2]];
        let sets = sacks.map(e => new Set(e));
        let intersection = sets.reduce((acc, el) => intersect(acc, el), sets[0]);
        if(intersection.size !== 1){
            console.warn("Group intersection was not a single element", sacks, sets);
        } else {
            sum += priority.indexOf(intersection.values().next().value)
        }        
    }
    return sum;
}

console.log(part2());

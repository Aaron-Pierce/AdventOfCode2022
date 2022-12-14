import { readFileSync } from "fs";

enum Ord {
    GT,
    LT,
    EQ
}

function lexocog(l1: any, l2: any){
    if(typeof l1 === "number" && typeof l2 === "number"){
        if (l1 < l2) return Ord.LT;
        if (l1 > l2) return Ord.GT;
        return Ord.EQ;
    } else if (typeof l1 === "number"){
        return lexocog([l1], l2);
    } else if (typeof l2 === "number"){
        return lexocog(l1, [l2]);
    } else {
        for(let i = 0; i < Math.min(l1.length, l2.length); i++){
            let l = l1[i];
            let r = l2[i];
            let result = lexocog(l, r);
            if(result == Ord.LT || result == Ord.GT) return result;
            else continue;
        }
        return lexocog(l1.length, l2.length)
    }
}

function part1(input: String){
    let pairs = input.split("\n\n").map(block => block.split("\n").map(line => JSON.parse(line)));
    let t = pairs.map(p => lexocog(p[0], p[1]));
    let indexSums = t.reduce((acc, el, ind) => (el == Ord.LT ? acc + (ind+1) : acc), 0);
    console.log(indexSums);
}

function part2(input: String){
    let pairs = input.split("\n\n").map(block => block.split("\n").map(line => JSON.parse(line)));
    let lists = pairs.flat();
    lists.push([[2]])
    lists.push([[6]])
    let sorted = lists.sort((l1, l2) => {
        let result = lexocog(l1, l2);
        if (result === Ord.LT) return -1;
        if (result === Ord.EQ) return 0;
        if (result === Ord.GT) return 1;
        throw "Unreachable"
    })
    let stringified = sorted.map(e => JSON.stringify(e));

    return (stringified.indexOf("[[2]]") + 1) * (stringified.indexOf("[[6]]") + 1)
}

let input = readFileSync("input.txt").toString("utf-8");
console.log(part1(input));
console.log(part2(input));
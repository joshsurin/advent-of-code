import * as fs from 'fs';

const input = fs
  .readFileSync('input.txt', 'utf-8')
  .split('\n')
  .map(line => line.split(''))
  .map(line => line.map(c => Number(c)));

const up = [-1, 0];
const down = [1, 0];
const left = [0, -1];
const right = [0, 1];

function getTrailheads(input: number[][]): [number, number][] {
    const trailheads: [number, number][] = [];

    for(let i = 0; i < input.length; i++) {
        for(let j = 0; j < input[0].length; j++) {
            if(input[i][j] === 0) {
              trailheads.push([i, j]);
            }
        }
    }

    return trailheads;
}

function isValidPosition(input: number[][], pos: [number, number]): boolean {
    return pos[0] >= 0 && pos[0] < input.length && pos[1] >= 0 && pos[1] < input[0].length;
}

function getValue(input: number[][], pos: [number, number]): number {
    return input[pos[0]][pos[1]];
}

function getTrailheadScore(input: number[][], startingPosition: [number, number]): number {
    let trailheads = new Set<string>();
    let visited = new Set<string>();
    
    function dfs(currentPos: [number, number]) {
        const currentValue = getValue(input, currentPos);
        const posKey = `${currentPos[0]},${currentPos[1]}`;
        
        if (currentValue === 9) {
            trailheads.add(posKey);
            return;
        }
        
        visited.add(posKey);
        
        const directions = [up, down, left, right];
        for (const dir of directions) {
            const nextPos: [number, number] = [currentPos[0] + dir[0], currentPos[1] + dir[1]];
            const nextPosKey = `${nextPos[0]},${nextPos[1]}`;
            
            if (isValidPosition(input, nextPos) && 
                !visited.has(nextPosKey) && 
                getValue(input, nextPos) === currentValue + 1) {
                dfs(nextPos);
            }
        }
    }
    
    dfs(startingPosition);
    return trailheads.size;
}

function getTrailheadScores(input: number[][]): number[] {
    const trailheads = getTrailheads(input);
    return trailheads.map(trailhead => getTrailheadScore(input, trailhead));
}

function getTrailheadRating(input: number[][], pos: [number, number]): number {
    let rating = 0;
    
    function dfs(currentPos: [number, number]) {
        const currentValue = getValue(input, currentPos);
        
        if (currentValue === 9) {
            rating++
            return;
        }
        
        const directions = [up, down, left, right];
        for (const dir of directions) {
            const nextPos: [number, number] = [currentPos[0] + dir[0], currentPos[1] + dir[1]];
            
            if (isValidPosition(input, nextPos) && 
                getValue(input, nextPos) === currentValue + 1) {
                dfs(nextPos);
            }
        }
    }
    
    dfs(pos);
    return rating;
}

function getTrailheadRatings(input: number[][]): number[] {
    const trailheads = getTrailheads(input);
    return trailheads.map(trailhead => getTrailheadRating(input, trailhead));
}

const answer1 = getTrailheadScores(input).reduce((acc, score) => acc + score, 0);
const answer2 = getTrailheadRatings(input).reduce((acc, score) => acc + score, 0);
console.log(`part1: ${answer1}`);
console.log(`part2: ${answer2}`);
import * as fs from 'fs';

const input = fs
  .readFileSync('input.txt', 'utf-8')
  .split(' ')
  .map(Number);

const blinkMap = new Map<string, number>();

function blink(value: number): number[] {
        if(value === 0) return [1];

        const stoneString = value.toString();
        if(stoneString.length % 2 === 0) {
            const mid = stoneString.length / 2;
            const leftStone = stoneString.slice(0, mid);
            const rightStone = stoneString.slice(mid);
            return [Number(leftStone), Number(rightStone)];
        }

        return [value * 2024];
}

function blinks(stone: number, n: number): number {
    if (n === 0) return 1;

    const key = `${stone}-${n}`;
    if (blinkMap.has(key)) {
        return blinkMap.get(key);
    }

    const newStones = blink(stone);
    const totalStones = newStones.reduce((acc, stone) => acc + blinks(stone, n - 1), 0);

    blinkMap.set(key, totalStones);
    return totalStones;
}

const answer1 = input.reduce((acc, stone) => acc + blinks(stone, 25), 0);
const answer2 = input.reduce((acc, stone) => acc + blinks(stone, 75), 0);
console.log(`part1: ${answer1}`);
console.log(`part2: ${answer2}`);
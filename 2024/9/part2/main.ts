import * as fs from 'fs';

const input = fs
  .readFileSync('input.txt', 'utf-8')
  .split('')
  .map(c => Number(c));

function processRawBlock(input: number[]): number[] {
  let processedBlock: number[] = [];
  for (let i = 0; i < input.length; i++) {
    if (i % 2) {
      for (let j =0; j < input[i]; j++ ) {
        processedBlock.push(-1);
      }
    } else {
      for (let j = 0; j < input[i]; j++ ) {
        processedBlock.push(i/2);
      }
    }
  }
  return processedBlock;
}

function defragBlock(block: number[]): number[] {
    let defraggedBlock = [...block];
    let currentBlockValue = block[block.length - 1];
    let currentBlockSize = 1;
    
    for (let i = block.length - 2; i >= 0; i--) {
        if (block[i] === currentBlockValue) {
            currentBlockSize++;
        } else {
            if (currentBlockValue !== -1) {
                const indexToFill = findEmptyBlockRegion(defraggedBlock, currentBlockSize);
                if (indexToFill !== -1 && indexToFill < i) {
                    for (let j = 0; j < currentBlockSize; j++) {
                        defraggedBlock[indexToFill + j] = currentBlockValue;
                        defraggedBlock[i + j + 1] = -1;
                    }
                }
            }
            currentBlockValue = block[i];
            currentBlockSize = 1;
        }
    }

    return defraggedBlock;
}

function findEmptyBlockRegion(input: number[], size: number): number {
  let emptyBlockRegion = 0;
  for (let i = 0; i < input.length; i++) {
    if (input[i] === -1) {
        emptyBlockRegion++;
        if(emptyBlockRegion === size) {
            return i - size + 1;
        }
    } else {
        emptyBlockRegion = 0;
    }
  }
  return -1;
}

function calculateChecksum(input: number[]): number {
  let checksum = 0;
  for (let i = 0; i < input.length; i++) {
    if(input[i] !== -1) {
    checksum = checksum + (i * input[i]);
    }
  }
  return checksum;
}

const answer = calculateChecksum(defragBlock(processRawBlock(input)));
console.log(answer);
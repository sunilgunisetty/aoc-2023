const fs = require('fs')

const day1Part1 = () => {
  const fileContents = fs.readFileSync('./input/day1-part1.txt', 'utf8')
  let sum = 0;
  fileContents.split(/\r?\n/).filter(Boolean).forEach(line => {
    const numbers = [...line.matchAll(/\d/g)].map(([n]) => n)
    let {0 : a ,[numbers.length - 1] : b} = numbers;
    sum = sum + parseInt (a + b)
  })

  return sum
}

// console.log(day1Part1());

const stringToNumeric = {
  "one": "1",
  "two": "2",
  "three": "3",
  "four": "4",
  "five": "5",
  "six": "6",
  "seven": "7",
  "eight": "8",
  "nine": "9"
}

const day1Part2 = () => {
  const fileContents = fs.readFileSync('./input/day1-part2.txt', 'utf-8')

  let sum = 0
  fileContents.split(/\r?\n/).filter(Boolean).forEach(line => {
    const numbers = [...line.matchAll(/(?=(\d|one|two|three|four|five|six|seven|eight|nine))/g)]
      .map(([_, n]) => n)
      .map(n => stringToNumeric[n] || n)
    let {0: a, [numbers.length -1]: b} = numbers
    sum = sum + parseInt(a + b)
  })

  return sum
}

// console.log(day1Part2());

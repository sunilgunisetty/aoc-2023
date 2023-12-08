const fs = require('fs')

const cubeLimit = {
  "red": 12,
  "green": 13,
  "blue": 14
}

const day2Part1 = () => {
  const fileContents = fs.readFileSync('./input/day2-part1.txt', 'utf8')
  let sum = 0
  fileContents.split(/\r?\n/).filter(Boolean).forEach(line => {
    const [gameId, gameData] = [...line.split(':')]
    const result = gameData
      .split(';')
      .map(x => x.trim())
      .flatMap(x => x
        .split(',')
        .map(x => [...x.matchAll(/(\d+)\s+(.*)/g)])
        .map(([[_, n, c]]) => ([n, c])))
      .map(([no, color]) => { return (parseInt(no) <= cubeLimit[color])})
      .every(x => x === true)

    if ( result )
      sum += parseInt(gameId.match(/\d+/)[0])
  })
  return sum
}

console.log(day2Part1())

const day2Part2 = () => {
  const fileContents = fs.readFileSync('./input/day2-part1.txt', 'utf8')
  let sum = 0
  fileContents.split(/\r?\n/).filter(Boolean).forEach(line => {
    const [_, gameData] = [...line.split(':')]
    const {red, blue, green} = gameData
      .split(';')
      .map(x => x.trim())
      .flatMap(x => x
        .split(',')
        .map(x => [...x.matchAll(/(\d+)\s+(.*)/g)])
        .map(([[_, n, c]]) => ([n, c])))
      .reduce((acc, [no, color]) => {
        if(acc[color] <= parseInt(no))
          return {...acc, [color]: parseInt(no)}
        else
          return {...acc}
      }, {red: 0, blue: 0, green: 0})
    sum += (red * blue * green)
  })
  return sum;
}

console.log(day2Part2())

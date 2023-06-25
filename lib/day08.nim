import std/sequtils, std/strutils, std/sugar, std/tables, std/sets

let trees = readFile("lib/day08.input").splitLines().map(line => line.mapIt(int(it) - int('0')))

var maxesForRow = initTable[int, int]()
var maxesForCol = initTable[int, int]()

for i in countup(0, trees.len() - 1):
  maxesForRow[i] = -1
  maxesForCol[i] = -1

type Position = tuple[x: int, y: int]

var visible = initHashSet[Position]()

for y in countup(0, trees.len() - 1):
  for x in countup(0, trees[0].len() - 1):
    let tree = trees[y][x]
    if maxesForCol[x] < tree:
      visible.incl((x: x, y: y))
      maxesForCol[x] = tree

    if maxesForRow[y] < tree:
      visible.incl((x: x, y: y))
      maxesForRow[y] = tree

for i in countup(0, trees.len() - 1):
  maxesForRow[i] = -1
  maxesForCol[i] = -1

for y in countdown(trees.len() - 1, 0):
  for x in countdown(trees[0].len() - 1, 0):
    let tree = trees[y][x]
    if maxesForCol[x] < tree:
      visible.incl((x: x, y: y))
      maxesForCol[x] = tree

    if maxesForRow[y] < tree:
      visible.incl((x: x, y: y))
      maxesForRow[y] = tree

echo visible.len()

proc scenicScore(posX: int, posY: int): int =
  let posHeight = trees[posY][posX]
  
  var scores = @[0, 0, 0, 0]

  for x in countdown(posX - 1, 0):
    scores[0] += 1

    if trees[posY][x] >= posHeight:
      break

  for x in countup(posX + 1, trees.len() - 1):
    scores[1] += 1

    if trees[posY][x] >= posHeight:
      break

  for y in countdown(posY - 1, 0):
    scores[2] += 1

    if trees[y][posX] >= posHeight:
      break

  for y in countup(posY + 1, trees.len() - 1):
    scores[3] += 1

    if trees[y][posX] >= posHeight:
      break

  result = scores.foldl(a * b)

var maxScenicScore = -1

for y in countup(0, trees.len() - 1):
  for x in countup(0, trees[0].len() - 1):
    let score = scenicScore(x, y)

    if score > maxScenicScore:
      maxScenicScore = score

echo maxScenicScore
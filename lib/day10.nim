import std/strutils, std/sequtils, std/tables

let instructions = readFile("lib/day10.input").splitLines().mapIt(it.splitWhitespace())

var cycle = 1
var registerValueLog = initTable[int, int]()
registerValueLog[cycle] = 1

proc registerValueAtCycle(cycle: int): int =
  for c in countdown(cycle, 1):
    if registerValueLog.hasKey(c):
      return registerValueLog[c]

for instruction in instructions:
  case instruction[0]:
    of "noop":
      cycle += 1

    of "addx":
      cycle += 2
      registerValueLog[cycle] = registerValueAtCycle(cycle) + instruction[1].parseInt()

let interestingCycles = @[20, 60, 100, 140, 180, 220]

echo "Part one: ", interestingCycles.mapIt(registerValueAtCycle(it) * it).foldl(a + b)

let crtHeight= 6
let crtWidth = 40

for y in countup(0, crtHeight - 1):
  var line = ""

  for x in countup(0, crtWidth - 1):
    let horizontalSpritePosition = registerValueAtCycle(y * 40 + x + 1)
    if @[-1, 0, 1].contains(x - horizontalSpritePosition):
      line = line & "#"
    else:
      line = line & " "

  echo line
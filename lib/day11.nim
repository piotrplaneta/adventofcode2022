import std/strutils, std/sequtils, std/math, std/tables, std/algorithm

let monkeysDesc = readFile("lib/day11.input").split("\n\n").mapIt(it.splitLines())

type Monkey = object
  id: int
  items: seq[int]
  operation: char
  operandA: string
  operandB: string
  divisableTest: int
  ifTrueMonkeyId: int
  ifFalseMonkeyId: int

proc parseMonkey(monkeyDesc: seq[string]): Monkey =
  let operation = monkeyDesc[2].split(" = ")[1].split(" ")[1][0]

  result = Monkey(
    id: monkeyDesc[0].splitWhitespace()[1].replace(":", "").parseInt(),
    items: monkeyDesc[1].split(": ")[1].split(", ").mapIt(it.parseInt()),
    operation: operation,
    operandA: monkeyDesc[2].split(operation)[0].split(" = ")[1].strip(),
    operandB: monkeyDesc[2].split(operation)[1].strip(),
    divisableTest: monkeyDesc[3].split(" by ")[1].parseInt(),
    ifTrueMonkeyId: monkeyDesc[4].split(" monkey ")[1].parseInt(),
    ifFalseMonkeyId: monkeyDesc[5].split(" monkey ")[1].parseInt(),
  )

var monkeys = monkeysDesc.mapIt(parseMonkey(it))
var inspectionCount = initCountTable[int]()

proc calculateOperation(item: int, monkey: Monkey): int =
  let operandA = case monkey.operandA:
    of "old":
      item
    else:
      monkey.operandA.parseInt()

  let operandB = case monkey.operandB:
    of "old":
      item
    else:
      monkey.operandB.parseInt()

  case monkey.operation:
    of '+':
      result = operandA + operandB
    of '-':
      result = operandA - operandB
    of '*':
      result = operandA * operandB
    of '/':
      result = (operandA / operandB).floor().toInt()
    else:
      result = -1
  

proc playRound() = 
  for monkey in monkeys:
    for item in monkey.items:
      inspectionCount.inc(monkey.id)
      let newItemValue = (calculateOperation(item, monkey) / 3).floor().toInt()
      if newItemValue.mod(monkey.divisableTest) == 0:
        monkeys[monkey.ifTrueMonkeyId].items.add(newItemValue)
      else:
        monkeys[monkey.ifFalseMonkeyId].items.add(newItemValue)

    monkeys[monkey.id].items = @[]

for _ in countup(1, 20):
  playRound()

let counts =  inspectionCount.values.toSeq().sorted(system.cmp, SortOrder.Descending)
echo counts[0] * counts[1]
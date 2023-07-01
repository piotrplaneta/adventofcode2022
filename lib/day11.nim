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
  

proc playRound(monkeys: var seq[Monkey], inspectionCount: var CountTable[int]) = 
  for monkey in monkeys:
    for item in monkey.items:
      inspectionCount.inc(monkey.id)
      let newItemValue = (calculateOperation(item, monkey) / 3).floor().toInt()
      if newItemValue.mod(monkey.divisableTest) == 0:
        monkeys[monkey.ifTrueMonkeyId].items.add(newItemValue)
      else:
        monkeys[monkey.ifFalseMonkeyId].items.add(newItemValue)

    monkeys[monkey.id].items = @[]

proc subtractDivisorsMult(v: int, divisorsMult: int): int = 
  let howMany = (v / divisorsMult).floor().toInt()
  result = v - divisorsMult * howMany

proc playRoundTwo(monkeys: var seq[Monkey], inspectionCount: var CountTable[int], divisorsMult: int) = 
  for monkey in monkeys:
    for item in monkey.items:
      inspectionCount.inc(monkey.id)
      let newItemValueBeforeLowering = calculateOperation(item, monkey)
      let newItemValue = subtractDivisorsMult(newItemValueBeforeLowering, divisorsMult)

      if newItemValue.mod(monkey.divisableTest) == 0:
        monkeys[monkey.ifTrueMonkeyId].items.add(newItemValue)
      else:
        monkeys[monkey.ifFalseMonkeyId].items.add(newItemValue)

    monkeys[monkey.id].items = @[]

proc playGame(rounds: int, part: int): int = 
  var monkeys = monkeysDesc.mapIt(parseMonkey(it))
  var inspectionCount = initCountTable[int]()

  for _ in countup(1, rounds):
    if part == 1:
      playRound(monkeys, inspectionCount)
    else:
      let divisorsMult = monkeys.mapIt(it.divisableTest).foldl(a * b)
      playRoundTwo(monkeys, inspectionCount, divisorsMult)

  let counts = inspectionCount.values.toSeq().sorted(system.cmp, SortOrder.Descending)
  result = counts[0] * counts[1]
    

echo "Part 1: ", playGame(20, 1)
echo "Part 2: ", playGame(10000, 2)
import std/strutils, std/sequtils, std/deques, std/tables

let input = readFile("lib/day12.input").splitLines()
var grid = initTable[(int, int), char]()

for y, line in input:
  for x, c in line:
    grid[(x, y)] = c

proc height(v: (int, int)): int =
  case grid[v]:
    of 'S': result = int('a')
    of 'E': result = int('z')
    else: result = int(grid[v])

proc neighbours(v: (int, int)): seq[(int, int)] = 
  for d in @[(-1, 0), (0, 1), (1, 0), (0, -1)]:
    let u = (v[0] + d[0], v[1] + d[1])
    
    if grid.hasKey(u) and height(u) - height(v) <= 1:
      result.add(u)

proc shortestPath(start: (int, int)): int =
  var q = initDeque[((int, int), int)]()
  var discovered = initCountTable[(int, int)]()

  discovered.inc(start)
  q.addLast((start, 0))

  while q.len() > 0:
    let (v, distance) = q.popFirst()

    if grid[v] == 'E':
      return distance
    else:
      for u in neighbours(v):
        if discovered[u] == 0:
          discovered.inc(u)
          q.addLast((u, distance + 1))
  
  return -1

let start = grid.keys.toSeq().filterIt(grid[it] == 'S')[0]
echo "Part one: ", shortestPath(start)

let startingPoints = grid.keys.toSeq.filterIt(height(it) == height(start))
echo "Part two: ", startingPoints.mapIt(shortestPath(it)).filterIt(it > -1).min()
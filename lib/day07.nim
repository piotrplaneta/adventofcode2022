import strutils, tables, sequtils, algorithm, sugar
let lines = readFile("lib/day07.input").splitLines()

var path: seq[string]
path = @[]

type FileWithSize = tuple[name: string, size: int]
var files = initTable[seq[string], seq[FileWithSize]]()

for i, line in lines:
    case line[0..3]
    of "$ cd":
      if line.contains(".."):
        path.delete(path.len - 1)
      else:
        path.add(line[5 .. ^1])
    of "$ ls":
      for fileOrDirLine in lines[i + 1 .. ^1]:
        if fileOrDirLine.startsWith("$"):
          break
        elif fileOrDirLine.startsWith("dir"):
          files[path & @[fileOrDirLine.split(" ")[1]]] = @[]
        else:
          let size = fileOrDirLine.split(" ")[0].parseInt
          let name = fileOrDirLine.split(" ")[1]
          if path notin files:
            files[path] = @[]
          files[path].add((name: name, size: size))
    else:
      continue

proc folderSize(folderPath: seq[string]): int =
  let folderAndSubfolders = files.keys.toSeq.filterIt(it.join"/".startsWith(folderPath.join("/")))
  result = 0
  for path in folderAndSubfolders:
    if files[path].len > 0:
      result += files[path].mapIt(it.size).foldl(a + b)

let sizes = files.keys.toSeq.mapIt(folderSize(it)).sorted()

echo "Part 1: ", sizes.filterIt(it <= 100000).foldl(a + b)

let capacity = 70000000
let required = 30000000
let taken = folderSize(@["/"])

echo "Part 2: ", sizes[sizes.mapIt(capacity - taken + it >= required).find(true)]
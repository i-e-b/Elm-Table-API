module TableDrawing where


type Cell = 
    { proportion:Float {- size relative to total width 0..1 -}
    , minSize:Int {- minimum width in pixels -}
    , content:Element {- thing to display in cell -}
    }

type Row = [Cell]
type Table = [Row]

-- given a list of proportions and an overall width
-- return a list of sub-widths
divisions : [Float] -> Int -> [Int]
divisions percents size = 
  let x s = map (\f -> round(f * (toFloat s))) percents
  in x size
  
-- Given a list of elements, and a list of column widths, 
-- output a list of sized elements
makeRow : [Element] -> [Int] -> [Element]
makeRow elements widths =
  let pairs = zip elements widths
  in  map (\(e,w) -> width w e) pairs

-- given a function that builds cells from records, and a list of 
-- records, build a table
tabulateRecords : (a -> [Cell]) -> [a] -> Table
tabulateRecords f recs = map (f) recs

-- todo: proportions and minimum sizes
buildRow : Int -> [Cell] -> Element
buildRow w r = 
    let rawContent = map (\x -> x.content) r
        rawSizes = divisions (map (\x -> x.proportion) r) w
        resize elem size = width size elem
    in  flow right (zipWith (resize) (rawContent) (rawSizes))

-- Turn table data into a displayable 
tableElement : Table -> Int -> Element
tableElement t width = 
    let rows = map (buildRow width) t
    in  flow down rows

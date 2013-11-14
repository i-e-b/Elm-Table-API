import Window

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


exampleElements = map (plainText) ["One", "Two", "Three", "Four"]
exampleWidths = divisions [0.5, 0.1, 0.2, 0.3] <~ Window.width

exampleRow =
  let sizedElements = (makeRow exampleElements) <~ (exampleWidths)
  in flow right <~ sizedElements


main = flow down <~ (combine [exampleRow, exampleRow, exampleRow, exampleRow])
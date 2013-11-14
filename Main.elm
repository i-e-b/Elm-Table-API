import Window

divisions : [Float] -> Int -> [Int]
divisions percents size = 
  let x s = map (\f -> round(f * (toFloat s))) percents
  in x size
  

makeRow : [Element] -> [Int] -> [Element]
makeRow elements widths =
  let pairs = zip elements widths
  in  map (\(e,w) -> width w e) pairs


exampleElements = map (plainText) ["One", "Two", "Three", "Four"]
exampleWidths = [0.5, 0.1, 0.2, 0.3]

exampleRow = 
  flow right
  <~ ((makeRow exampleElements) <~ (divisions exampleWidths <~ Window.width))


main = exampleRow
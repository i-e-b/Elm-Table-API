import Window
import Http
import JavaScript.Experimental as JS
import Json

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
exampleWidths w = divisions [0.5, 0.1, 0.2, 0.3] w

exampleRow w =
  let sizedElements = (makeRow exampleElements) (exampleWidths w)
  in flow right sizedElements


-- this one would be data source -> width -> [Element]
exampleTable w = [exampleRow w, exampleRow w, exampleRow w, exampleRow w]


--main = flow down <~ (exampleTable <~ Window.width)
main = asText <~ dataJson

dataJson = toJson <~ dataSignal

dataSignal = Http.send (constant batchRequest)


-- The standard parts of a Flickr API request.
batchRequest = Http.get
  "http://prod-jester-web00.nix.sys.7d/file-notification-api/batches?format=json"


toJson response =
    case response of
      Http.Success str -> Json.fromString str
      _ -> Nothing


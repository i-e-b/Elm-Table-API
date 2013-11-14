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

dataJson : Signal (Maybe Json.JsonValue)
dataJson = lift toJson (dataSignal (every (5*second)))

dataSignal : Signal a -> Signal (Http.Response String)
dataSignal t = Http.send (lift batchRequest t)


batchRequest x = Http.get
  "http://prod-jester-web00.nix.sys.7d/file-notification-api/batches?format=json"


toJson : Http.Response String -> Maybe Json.JsonValue
toJson response =
    case response of
      Http.Success str -> Json.fromString str
      _ -> Nothing


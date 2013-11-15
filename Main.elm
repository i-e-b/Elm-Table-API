import Window
import Http
import JavaScript.Experimental as JS
import Json
import Dict

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
--main = asText <~ ((orEmpty jsonToBatch) <~ dataJson)
main = asText <~ {-(orEmpty jsonToBatch <~-} dataJson (every (5*second)){-)-}

dataJson : Signal a -> Signal (Maybe Json.JsonValue)
dataJson s = lift toJson (dataSignal s)

dataSignal : Signal a -> Signal (Http.Response String)
dataSignal t = Http.send (lift batchRequest t)

batchRequest : a -> Http.Request String
batchRequest x = Http.get
    ("http://localhost:8000/sample.json?q=" ++ (show x))
--  "http://prod-jester-web00.nix.sys.7d/file-notification-api/batches?format=json"


type Batch = {receivedDate:String, isCompleted:Bool, batchId:String}

orEmpty : (a -> [b]) -> Maybe a -> [b]
orEmpty f ma = case ma of
    Just ja -> f ja
    Nothing -> []

jsonToBatch : Json.JsonValue -> [Batch]
jsonToBatch jv =
  let all = array jv
      vals = map (\x -> object x) all
      empty = Json.String ""
      false = Json.Boolean False
      mapper d = 
        {receivedDate = string (Dict.findWithDefault empty "ReceivedDate" d)
        , batchId = string (Dict.findWithDefault empty "BatchId" d)
        , isCompleted = boolean (Dict.findWithDefault false "IsCompleted" d)
        }
  in map (mapper) (vals)
      

maybeRecord : Maybe Json.JsonValue -> Maybe a
maybeRecord jsonVal = 
  case jsonVal of 
    Just jn -> Just (JS.toRecord ( Json.toJSObject jn))
    _ -> Nothing

toJson : Http.Response String -> Maybe Json.JsonValue
toJson response =
    case response of
      Http.Success str -> Json.fromString str
      _ -> Nothing

string : Json.JsonValue -> String
string v = case v of { Json.String s -> s ; _ -> "" }

boolean : Json.JsonValue -> Bool
boolean v = case v of { Json.Boolean b -> b ; _ -> False }

array : Json.JsonValue -> [Json.JsonValue]
array v = case v of {Json.Array a -> a ; _ -> [] }

object : Json.JsonValue -> Dict.Dict String Json.JsonValue
object v = case v of { Json.Object o -> o ; _ -> Dict.empty }


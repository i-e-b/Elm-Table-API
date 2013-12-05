import Window
import Http
import JavaScript.Experimental as JS
import Json
import Dict
import open TableDrawing


main = lift2 batchesToElement(batchFeed) (Window.width)
batchFeed = orEmpty jsonToBatch <~ dataJson (every (5*second))

-- Batch status, as it comes from the API
type Batch = {receivedDate:String, isCompleted:Bool, batchId:String}

-- Turn a list of batches and a screen width into a single table element.
batchesToElement : [Batch] -> Int -> Element
batchesToElement bs width = tableElement (batchTable bs) width

-- Turn a list of batches into a table
batchTable : [Batch] -> Table
batchTable bs =
    let cl s = {proportion=0.3, minSize=10, content=plainText s}
        batchRow b =
            [ {proportion=0.2, minSize=10, content=plainText b.receivedDate}
            , {proportion=0.1, minSize=10, content=plainText (if b.isCompleted then "X" else " ")}
            , {proportion=0.7, minSize=10, content=plainText b.batchId}
            ]
    in  map (batchRow) bs

-- Map a json array into an array of Batch data.
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

batchRequest : a -> Http.Request String
batchRequest x = Http.get
    ("http://localhost:8000/sample.json?q=" ++ (show x))
--  "http://prod-jester-web00.nix.sys.7d/file-notification-api/batches?format=json"

dataJson : Signal a -> Signal (Maybe Json.JsonValue)
dataJson s = lift toJson (dataSignal s)

dataSignal : Signal a -> Signal (Http.Response String)
dataSignal t = Http.send (lift batchRequest t)

-- ############# API crap #####################
orEmpty : (a -> [b]) -> Maybe a -> [b]
orEmpty f ma = case ma of
    Just ja -> f ja
    Nothing -> []

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


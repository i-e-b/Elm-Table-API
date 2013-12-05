import Window
import Http
import JavaScript.Experimental as JS
import Json
import Dict


batchFeed = orEmpty jsonToBatch <~ dataJson (every (5*second))
main = lift2 batchesToElement(batchFeed) (Window.width)

batchesToElement : [Batch] -> Int -> Element
batchesToElement bs width = tableElement (batchTable bs) width

-- ############## Tables from records ##################

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

-- ############# API to table #################

batchTable : [Batch] -> Table
batchTable bs =
    let cl s = {proportion=0.3, minSize=10, content=plainText s}
        batchRow b =
            [ {proportion=0.2, minSize=10, content=plainText b.receivedDate}
            , {proportion=0.1, minSize=10, content=plainText (if b.isCompleted then "X" else " ")}
            , {proportion=0.7, minSize=10, content=plainText b.batchId}
            ]
    in  map (batchRow) bs

-- ############# API crap #####################

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


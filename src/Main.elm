import Html exposing ( .. )
import Html.Attributes exposing ( .. )
import Html.Events exposing ( .. )
import Keyboard exposing (..)
import Time exposing ( .. )
import Random exposing ( .. )
import Task exposing ( .. )
import Random.Int exposing ( .. )

main =
    Html.program
        {init = init, view = view, update = update, subscriptions = subscriptions}

-- INIT

init : (Model, Cmd Msg)
init = (model, Cmd.none)


-- MODEL

type alias Model = 
    { fact : String,
      ref : String,
      db : List (String, String),
      press : KeyCode,
      time : Int
    }

model : Model
model =
    {fact = "", ref = "", press = 0, time = 0, db = getDB}


-- UPDATE

type Msg = Presses KeyCode | OnTime Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Presses x -> if x == 32
                 then ({ model | ref = getRef model, fact = getFact model, press = getRandomIntInDBRange model}, Task.perform OnTime Time.now)
                 else (model, Cmd.none)
    OnTime x -> ({ model | time = round x }, Cmd.none)


-- SUBSCRIPTIONS
subscriptions: Model -> Sub Msg
subscriptions model = Keyboard.presses (\code -> Presses code)

-- VIEW

view : Model -> Html Msg
view model =
    div [class "grid"] [ 
        div [class "row"] [
            div [class "col"] [
                viewFact model
            ],
            div [class "row col-12 col"][text "Press space to get a fact."]
        ]
    ]

viewFact : Model -> Html Msg
viewFact model = div [class "col-12"][div [class "row"] [h1 [][text model.fact, getReference model]]]


getDB : List (String, String)
getDB = [
         ("Hoes ain't just hoes, they some niggas.","https://www.youtube.com/watch?v=PFV-lYN6cw8"),
         ("Apple sold more than 5 iPhones in 2016.","https://www.statista.com/statistics/276306/global-apple-iphone-sales-since-fiscal-year-2007"),
         ("There are more than 500 people living in California.","https://en.wikipedia.org/wiki/California"),
         ("More than 1000 people log in to Facebook every day.","https://www.statista.com/statistics/264810/number-of-monthly-active-facebook-users-worldwide/"),
         ("There are more than 300 neurons in the human brain.","https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2776484/"),
         ("There are less than 10 neanderthals left on planet earth.","https://en.wikipedia.org/wiki/Neanderthal"),
         ("More than half of all pregnancies are a direct consequence of sexual intercourse.","https://en.wikipedia.org/wiki/In_vitro_fertilisation")
         ]

getRandomFromDB : Model -> Maybe (String, String)
getRandomFromDB model = List.head (List.reverse (List.take (getRandomIntInDBRange model) model.db))

getRandomIntInDBRange : Model -> Int
getRandomIntInDBRange model = Tuple.first (Random.step (Random.int 1 ((List.length model.db) + 1)) (Random.initialSeed model.time))

getFact : Model -> String
getFact model =
    case getRandomFromDB model of
        Just x -> Tuple.first x
        Nothing -> ""

getRef : Model -> String
getRef model =
    case getRandomFromDB model of
        Just x -> Tuple.second x
        Nothing -> ""

getReference : Model -> Html Msg
getReference model = if model.ref == "" then text "" else sup [][a [href model.ref][text "[1]"]]


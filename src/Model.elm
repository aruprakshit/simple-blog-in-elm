module Model exposing (FormData, Model, Post)


type alias Post =
    { id : Int
    , title : String
    , body : String
    }


type alias FormData =
    { title : String
    , content : String
    }


type alias Model =
    { posts : List Post
    , form : FormData
    }

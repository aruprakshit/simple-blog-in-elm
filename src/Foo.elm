module Foo exposing (Model, Post, logged)


type alias Post =
    { id : Int
    , title : String
    , body : String
    }


type alias Model =
    { posts : List Post
    }


logged =
    Debug.log "Log" (Model [])

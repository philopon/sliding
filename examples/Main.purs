module Main where

import Sliding.Engine
import DOM

import Data.Html
import qualified Data.Html.Elements.Html5 as E
import qualified Data.Html.Attributes.Html5 as A
import Sliding.Engine
import Sliding.Cached

main = slide defaultSlideConfig body
  [ [ newCached $ E.div []
      [ E.h1 [] [E.text "Sliding"]
      , E.h2 [] [E.text "purescript presentation library"]
      ]
    ]

  , [ newCached $ E.div []
      [ E.h1 [] [E.text "Operation"]
      , E.dl [] [E.dt [] [E.text "j,space,enter,right arrow,down arrow,click right harf,swipe left"], E.dd [] [E.text "next slide"]]
      ]
    , newCached $ E.div []
      [ E.h1 [] [E.text "Operation"]
      , E.dl [] [E.dt [] [E.text "j,space,enter,right arrow,down arrow,click right harf,swipe left"], E.dd [] [E.text "next slide"]]
      , E.dl [] [E.dt [] [E.text "k,left arrow,up arrow,click left harf,swipe right"], E.dd [] [E.text "prev slide"]]
      ]
    , newCached $ E.div []
      [ E.h1 [] [E.text "Operation"]
      , E.dl [] [E.dt [] [E.text "j,space,enter,right arrow,down arrow,click right harf,swipe left"], E.dd [] [E.text "next slide"]]
      , E.dl [] [E.dt [] [E.text "k,left arrow,up arrow,click left harf,swipe right"], E.dd [] [E.text "prev slide"]]
      , E.dl [] [E.dt [] [E.text "f"], E.dd [] [E.text "toggle fullscreen"]]
      ]
    , newCached $ E.div []
      [ E.h1 [] [E.text "Operation"]
      , E.dl [] [E.dt [] [E.text "j,space,enter,right arrow,down arrow,click right harf,swipe left"], E.dd [] [E.text "next slide"]]
      , E.dl [] [E.dt [] [E.text "k,left arrow,up arrow,click left harf,swipe right"], E.dd [] [E.text "prev slide"]]
      , E.dl [] [E.dt [] [E.text "f"], E.dd [] [E.text "toggle fullscreen"]]
      , E.dl [] [E.dt [] [E.text "0"], E.dd [] [E.text "jump to first slide"]]
      ]
    ]

  , [ newCached $ E.div []
      [ E.h1 [] [E.text "todo"], E.ul []
        [ E.li [] [E.text "wrap raw functions"]
        , E.li [] [E.text "slide overview"]
        ]
      ]
    ]
  , [ newCached $ E.div []
      [ E.h1 [] [E.text "project URL"]
      , E.a  [A.href "http://github.com/philopon/sliding"] [E.text "github"]
      ]
    ]
  ]

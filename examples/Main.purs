module Main where

import Sliding.Engine
import DOM

import Data.Html
import qualified Data.Html.Elements.Html5 as E
import qualified Data.Html.Attributes.Html5 as A

main = slide defaultSlideConfig documentElement
  [ [ E.div [] [ E.h1 [] [E.text "Sliding"]
               , E.h2 [] [E.text "purescript presentation library"]
               ]
    ]

  , [ E.div [] [ E.h1 [] [E.text "Operation"]
               , E.dl [] [E.dt [] [E.text "j,space,enter,right arrow,down arrow,click right harf,swipe left"], E.dd [] [E.text "next slide"]]
               ]
    , E.div [] [ E.h1 [] [E.text "Operation"]
               , E.dl [] [E.dt [] [E.text "j,space,enter,right arrow,down arrow,click right harf,swipe left"], E.dd [] [E.text "next slide"]]
               , E.dl [] [E.dt [] [E.text "k,left arrow,up arrow,click left harf,swipe right"], E.dd [] [E.text "prev slide"]]
               ]
    , E.div [] [ E.h1 [] [E.text "Operation"]
               , E.dl [] [E.dt [] [E.text "j,space,enter,right arrow,down arrow,click right harf,swipe left"], E.dd [] [E.text "next slide"]]
               , E.dl [] [E.dt [] [E.text "k,left arrow,up arrow,click left harf,swipe right"], E.dd [] [E.text "prev slide"]]
               , E.dl [] [E.dt [] [E.text "f"], E.dd [] [E.text "toggle fullscreen"]]
               ]
    , E.div [] [ E.h1 [] [E.text "Operation"]
               , E.dl [] [E.dt [] [E.text "j,space,enter,right arrow,down arrow,click right harf,swipe left"], E.dd [] [E.text "next slide"]]
               , E.dl [] [E.dt [] [E.text "k,left arrow,up arrow,click left harf,swipe right"], E.dd [] [E.text "prev slide"]]
               , E.dl [] [E.dt [] [E.text "f"], E.dd [] [E.text "toggle fullscreen"]]
               , E.dl [] [E.dt [] [E.text "0"], E.dd [] [E.text "jump to first slide"]]
               ]
    ]

  , [ E.div []
      [ E.h1 [] [E.text "todo"], E.ul []
        [ E.li [] [E.text "wrap raw functions"]
        , E.li [] [E.text "slide overview"]
        ]
      ]
    ]
  , [ E.div []
      [ E.h1 [] [E.text "project URL"]
      , E.a  [A.href "http://github.com/philopon/sliding"] [E.text "github"]
      ]
    ]
  ]

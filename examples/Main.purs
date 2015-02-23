module Main where

import Sliding.Engine
import DOM

import Data.Html
import qualified Data.Html.Elements.Html5 as E
import qualified Data.Html.Attributes.Html5 as A

foreign import appendBody """
function appendBody (e) {
  return function appendBodyEff(){
    document.body.appendChild(e);
  }
}""" :: forall e. Node -> EffHtml e Unit

main = do
  sliding <- slide {width: 800, height: 600}
    [ [ E.div [] [ E.h1 [] [E.text "Sliding"]
                 , E.h2 [] [E.text "presentation library"]
                 ]
      ]

    , [ E.div [] [ E.h1 [] [E.text "Operation"]
                 , E.dl [] [E.dt [] [E.text "j,space,enter,right arrow,down arrow"], E.dd [] [E.text "next slide"]]
                 ]
      , E.div [] [ E.h1 [] [E.text "Operation"]
                 , E.dl [] [E.dt [] [E.text "j,space,enter,right arrow,down arrow"], E.dd [] [E.text "next slide"]]
                 , E.dl [] [E.dt [] [E.text "k,left arrow,up arrow"], E.dd [] [E.text "prev slide"]]
                 ]
      ]

    , [ E.div []
        [ E.h1 [] [E.text "todo"], E.ul []
          [ E.li [] [E.text "wrap raw functions"]
          , E.li [] [E.text "slide overview"]
          , E.li [] [E.text "fullscreen mode"]
          ]
        ]
      ]
    ]

  slideNode sliding >>= appendBody

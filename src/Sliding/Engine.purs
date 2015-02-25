module Sliding.Engine
  ( Size(), Sliding()
  , SlideConfig()
  , defaultSlideConfig
  , Indicator()
  , numIndicator
  , slide
  , body
  ) where

import Global
import Data.Maybe(Maybe(..), fromMaybe, maybe)
import Data.Array((!!), length, elemIndex, filter, null)
import qualified Data.Array.Unsafe as U
import Math(abs, max, min)
import Control.Monad.Eff
import DOM(DOM(), Node(..))
import Data.Html
import qualified Data.Html.Elements.Html5 as E
import qualified Data.Html.Attributes.Html5 as A
import qualified Data.Html.Events.Normal as EV
import Network.Routing.Client
import Data.Function
import qualified FRP.Kefir as K

foreign import browerWindow "var browerWindow = window" :: Node
foreign import body "var body = document.body" :: Node

foreign import appendChildImpl """
function appendChildImpl(p, c){
  return function(){
    p.appendChild(c);
    return {};
  }
}""" :: forall e. Fn2 Node Node (Eff e Unit)

appendChild :: Node -> Node -> Eff _ Unit
appendChild p c = runFn2 appendChildImpl p c

foreign import addEventListenerImpl """
function addEventListenerImpl(tgt, nm, cb){
  return function(){
    var f = function(e){return cb(e)();}
    tgt.addEventListener(nm, f);
    return {}
  }
}""" :: forall e eff. Fn3 Node String (e -> Eff eff Unit) (Eff eff Unit)

addEventListener :: forall e. Node -> String -> (_ -> Eff e Unit) -> Eff e Unit
addEventListener n s m = runFn3 addEventListenerImpl n s m

foreign import toggleFullScreen """
// https://developer.mozilla.org/ja/docs/Web/Guide/DOM/Using_full_screen_mode#Toggling_fullscreen_mode
function toggleFullScreen() {
  if ((document.fullScreenElement && document.fullScreenElement !== null) ||    // alternative standard method
      (!document.mozFullScreen && !document.webkitIsFullScreen)) {              // current working methods
    if (document.documentElement.requestFullScreen) {
      document.documentElement.requestFullScreen();
    } else if (document.documentElement.mozRequestFullScreen) {
      document.documentElement.mozRequestFullScreen();
    } else if (document.documentElement.webkitRequestFullScreen) {
      document.documentElement.webkitRequestFullScreen(Element.ALLOW_KEYBOARD_INPUT);
    }
  } else {
    if (document.cancelFullScreen) {
      document.cancelFullScreen();
    } else if (document.mozCancelFullScreen) {
      document.mozCancelFullScreen();
    } else if (document.webkitCancelFullScreen) {
      document.webkitCancelFullScreen();
    }
  }
}""" :: forall e. Eff e Unit

foreign import setHash """
function setHash(s){
  return function(){
    window.location.hash = s;
    return {};
  }
}""" :: forall e. String -> Eff e Unit

foreign import orImpl """
function orImpl(a, b){
  return a || b;
}""" :: forall a. Fn2 a a a

or :: forall a. a -> a -> a
or a b = runFn2 orImpl a b

type Size =
  { width  :: Number
  , height :: Number
  }

foreign import getWindowSize """
function getWindowSize(){
  var w = window,
  d = document,
  e = d.documentElement,
  g = d.getElementsByTagName('body')[0];
  return {
    width: w.innerWidth || e.clientWidth || g.clientWidth,
    height: w.innerHeight || e.clientHeight || g.clientHeight
  };
}""" :: forall e. Eff e Size

foreign import getElementSize """
function getElementSize(e){
  return function(){
    return {
      width: e.clientWidth,
      height: e.clientHeight
    }
  }
}""" :: forall e. Node -> Eff e Size

getSize :: Node -> Eff _ Size
getSize e = if e `equal` body then getWindowSize else getElementSize e

type Indicator = Size -> [[VTree]] -> State -> VTree

type SlideConfig =
  { size          :: Size
  , swipeDuration :: Number
  , pager         :: Maybe Indicator
  }

defaultSlideConfig :: SlideConfig
defaultSlideConfig =
  { size: {width: 800, height: 600}
  , swipeDuration: 150
  , pager: Just numIndicator
  }

numIndicator :: Indicator 
numIndicator _ slides state = E.div
  [ A.class_ "page-number"
  , A.style
      { position: "absolute"
      , bottom: "10px"
      , right: "10px"
      }
  ]
  [ E.span [A.class_ "current"] [E.text $ show $ state.current.page + 1]
  , E.span [] [E.text "/"]
  , E.span [A.class_ "all"] [E.text $ show $ length slides]
  ]

render :: SlideConfig -> [[VTree]] -> State -> VTree
render config slides state =
  let scale = min
        (state.windowSize.height / config.size.height)
        (state.windowSize.width  / config.size.width)
      transform = "translate(-50%, -50%) scale(" ++ show scale ++ ")"
  in E.div
    [ A.style
        { position: "absolute"
        , left: "50%"
        , top:  "50%"
        , width: show config.size.width ++ "px"
        , height: show config.size.height ++ "px"

        , transform: transform
        , "-moz-transform": transform
        , "-webkit-transform": transform
        , "-o-transform": transform
        , "-ms-transform": transform
        }
    , A.class_ "slide-wrapper"
    ] $
    fromMaybe (U.head $ U.head slides) (slides !! state.current.page >>= \s -> s !! state.current.step) :
    maybe [] (\p -> [p config.size slides state]) config.pager

newtype Sliding = Sliding
  { action :: K.Stream (K.Emit ()) (K.All ()) Unit Action
  }

type Current =
  { page :: Number
  , step :: Number
  }

type State =
  { windowSize :: Size
  , current    :: Current
  }

foreign import equalImpl """
function equalImpl(a, b){
  return a == b;
}""" :: forall a b. Fn2 a b Boolean

equal :: forall a. a -> a -> Boolean
equal a b = runFn2 equalImpl a b

foreign import swipeEventImpl """
function swipeEventImpl(action, duration, node){
  return function(){
    var touchStart, touchEnd, touchExceeded, multiTouched;

    node.addEventListener('touchstart', function(e){
      touchStart    = e.touches[0].pageX;
      touchEnd      = touchStart;
      touchExceeded = false;
      multiTouched  = e.touches.length > 1;
    });

    node.addEventListener('touchmove', function(e){
      touchEnd = e.changedTouches[0].pageX;
      multiTouched = !multiTouched && e.changedTouches.length > 1
      if(!touchExceeded && !multiTouched && Math.abs(touchStart - touchEnd) > 30){
        e.preventDefault();
        touchExceeded = true;
      }
    });
    
    node.addEventListener('touchend', function(){
      if(touchExceeded){
        console.log(touchEnd - touchStart);
        if(touchEnd - touchStart > duration){
          action.left();
        } else if (touchEnd - touchStart < -duration){
          action.right();
        }
      }
    })

  }
}""" :: forall e f. Fn3 {left :: f, right:: f} Number Node (Eff e Unit)

slide :: SlideConfig -> Node -> [[VTree]] -> Eff _ Sliding
slide config parent slides0 = do
  let slides = filter (\s -> not $ null s) slides0

  -- elements
  html   <- createElement $ E.div [] []
  node <- getNode html
  appendChild parent node

  -- action emitter
  action <- K.emitter

  -- window resize events
  wSize0 <- getSize parent
  wSize  <- K.fromEventE browerWindow "resize" (\_ -> getSize parent)
    >>= K.toPropertyWith wSize0
  resize <- K.map ReSize wSize

  -- swipe events
  runFn3 swipeEventImpl {left: K.emit action prevStep, right: K.emit action nextStep} config.swipeDuration node

  -- click events
  click <- K.fromEvent node "click" id
  clickPage <- K.sampledBy wSize click $ \ws cl ->
    if cl.target `equal` node
       then if or cl.offsetX cl.layerX > config.size.width / 2
            then nextStep
            else prevStep
       else NoOp

  -- keyboard events
  let nextKeys = [ 32 -- space
                 , 13 -- enter
                 , 40 -- arrow down
                 , 39 -- arrow right
                 , 74 -- j
                 ]
      prevKeys = [ 38 -- arrow up
                 , 37 -- arrow left
                 , 75 -- k
                 ]
 
  addEventListener browerWindow "keydown" $ \e -> case or e.which e.keyCode of
    70 {- f -} -> toggleFullScreen
    48 {- 0 -} -> K.emit action $ setPage 0 0
    k | k `elem` nextKeys -> K.emit action nextStep
      | k `elem` prevKeys -> K.emit action prevStep
      | otherwise         -> return unit

  -- all events
  merged <- K.merge [resize, K.forget action, K.forget clickPage]
    >>= K.debounce 20

  -- state
  state  <- K.scanEff (update html slides)
    {windowSize: wSize0, current: {page: 0, step: 0}} merged

  -- execute FRP
  K.onValue state $ \st ->
    patch (render config slides st) html

  -- URI router
  runRouter $ do
    num <- param $ regex "[1-9][0-9]*"
    route2 (exact "page" -/ num  +/ num +/ empty) $ \p s -> do
      K.emit action $ setPage (readInt 10 p - 1) (readInt 10 s - 1)

    notFound $ \setRoute -> setRoute "/page/1/1"

  return $ Sliding { action: action }

data Action
  = ModifyPage (Current -> Current)
  | ReSize Size
  | NoOp

setPage :: Number -> Number -> Action
setPage p s = ModifyPage $ \_ -> {page: p, step: s}

nextStep :: Action
nextStep  = ModifyPage $ \p -> {page: p.page, step: p.step + 1}

prevStep :: Action
prevStep  = ModifyPage $ \p -> {page: p.page, step: p.step - 1}

update :: Html -> [[VTree]] -> State -> Action -> Eff _ State
update html slides state action = case action of
  NoOp         -> return state
  ReSize size  -> return $ state { windowSize = size }
  ModifyPage f -> do
    let c' = rePage slides $ f state.current
    setHash $ "#/page/" ++ show (c'.page + 1) ++ "/" ++ show (c'.step + 1)
    return $ state { current = c' }

rePage :: [[VTree]] -> Current -> Current
rePage slides cur =
  let p  = restrict 0 (length slides - 1) cur.page
      cn = maybe 1 (\v -> length v - 1) $ slides !! p
   in case unit of
     _ | cur.step < 0   -> let p' = max 0 (p - 1)
                               s' = maybe 0 (\s -> length s - 1) (slides !! p')
                            in {page: p', step: s'}
       | cur.step > cn  -> {page: min (length slides - 1) (p+1), step: 0}
       | otherwise      -> {page: p, step: cur.step}


restrict :: forall a. (Ord a) => a -> a -> a -> a
restrict mn mx a = case unit of
  _ | a < mn    -> mn
    | mx < a    -> mx
    | otherwise -> a

elem :: forall a. (Eq a) => a -> [a] -> Boolean
elem a l = elemIndex a l >= 0

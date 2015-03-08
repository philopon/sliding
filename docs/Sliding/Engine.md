# Module Documentation

## Module Sliding.Engine

#### `body`

``` purescript
body :: Node
```


#### `Size`

``` purescript
type Size = { height :: Number, width :: Number }
```


#### `Indicator`

``` purescript
type Indicator = Size -> Page -> State -> VTree
```


#### `SlideConfig`

``` purescript
type SlideConfig = { pager :: Maybe Indicator, swipeDuration :: Number, size :: Size }
```


#### `defaultSlideConfig`

``` purescript
defaultSlideConfig :: SlideConfig
```


#### `numIndicator`

``` purescript
numIndicator :: Indicator
```


#### `Sliding`

``` purescript
newtype Sliding
```


#### `State`

``` purescript
type State = { current :: Page, windowSize :: Size }
```


#### `slide`

``` purescript
slide :: SlideConfig -> Node -> [[VTree]] -> Eff _ Sliding
```





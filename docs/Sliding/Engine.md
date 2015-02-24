# Module Documentation

## Module Sliding.Engine

#### `Size`

``` purescript
type Size = { height :: Number, width :: Number }
```


#### `Indicator`

``` purescript
type Indicator = Size -> [[VTree]] -> State -> VTree
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
newtype Sliding eff
```


#### `slide`

``` purescript
slide :: SlideConfig -> Node -> [[VTree]] -> Eff _ (Sliding _)
```


#### `documentElement`

``` purescript
documentElement :: Node
```





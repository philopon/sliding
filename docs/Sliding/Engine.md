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


#### `RenderConfig`

``` purescript
type RenderConfig = { pager :: Maybe Indicator, size :: Size }
```


#### `defaultRenderConfig`

``` purescript
defaultRenderConfig :: RenderConfig
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
slide :: RenderConfig -> Node -> [[VTree]] -> Eff _ (Sliding _)
```


#### `documentElement`

``` purescript
documentElement :: Node
```





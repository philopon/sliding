# Module Documentation

## Module Sliding.Engine

#### `Size`

``` purescript
type Size = { height :: Number, width :: Number }
```


#### `Sliding`

``` purescript
newtype Sliding eff
```


#### `slide`

``` purescript
slide :: Size -> [[VTree]] -> Eff _ (Sliding _)
```


#### `slideNode`

``` purescript
slideNode :: Sliding _ -> EffHtml _ Node
```





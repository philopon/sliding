# Module Documentation

## Module Sliding.Cached

#### `Cached`

``` purescript
newtype Cached eff a
```


#### `newCached`

``` purescript
newCached :: forall eff v. v -> Eff (ref :: Ref | eff) (Cached eff v)
```


#### `newCachedM`

``` purescript
newCachedM :: forall eff v. Eff eff v -> Eff (ref :: Ref | eff) (Cached eff v)
```


#### `getCached`

``` purescript
getCached :: forall eff v. Cached (ref :: Ref | eff) v -> Eff (ref :: Ref | eff) v
```





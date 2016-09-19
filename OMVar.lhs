Online mean and variance monoid
https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance

> module OMVar
>     (
>       OMVar
>     , meanVar
>     , mkMeanVar
>     ) where
>
> import Data.Semigroup
> import Data.List (foldl')

Stores
a) total weight of samples seen so far
b) num samples seen
c) online mean
d) m2

> data OMVar = OMVar
>   {
>     totalWeight :: {-# UNPACK #-} !Double
>   , numSamples :: {-# UNPACK #-} !Double
>   , mean :: {-# UNPACK #-} !Double
>   , m2 :: {-# UNPACK #-} !Double
>   }
>
> meanVar :: OMVar -> (Double,Double,Double)
> meanVar (OMVar n sz m m2) = (sz,m,(m2/n) * (sz/(sz-1)))
>
> mkMeanVar :: Double -> Double -> OMVar
> mkMeanVar weight point = OMVar weight 1 point 0

Semigroup and monoid instance

> instance Semigroup OMVar where
>     (OMVar n sz m m2) <> (OMVar n' sz' m' m2') =
>         let d = m' - m
>             nX = n + n'
>             szX = sz + sz'
>             mX = m + d*(n'/nX)
>             m2X = m2 + m2' + d*d*n*n'/nX
>         in nX `seq` szX `seq` mX `seq` m2X `seq` (OMVar nX szX mX m2X)

> instance Monoid OMVar where
>     mempty = (OMVar 0 0 0 0)
>     mappend = (<>)
>     mconcat = foldl' (<>) mempty

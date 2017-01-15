import Rand
import String
import Array
import Util (countBy, frequencies)

-- not so much a rigorous "test" but a visual observation of a good enough for
-- a game of asteroids

ticker = every (millisecond)
numBins = 20

type State = {bins: Array.Array, seed: Rand.Seed}

defaultState = {bins = Array.repeat numBins 0, seed = Rand.newSeed 2}

update t ({bins, seed} as state) =
  let (n, seed') = Rand.float seed
      idx = floor <| n * numBins
      hits = Array.getOrFail idx bins

  in {state | seed <- seed'
            , bins <- Array.set idx (hits+1) bins }


render state =
  asText <| state.bins

countInts =
  frequencies <| fst <| Rand.ints -4 5 10000 (Rand.newSeed 10)

countFloats nbins =
  countBy (\n -> floor(n*nbins)) <| fst <| Rand.floats 10000 (Rand.newSeed 10)


main =
  --asText (countFloats 10)
  asText countInts
  --render <~ foldp update defaultState ticker





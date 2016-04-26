import           Debug.Trace (trace)

data Cat = Cat { lives :: Int } deriving (Show)

curiosity :: Cat -> Cat
curiosity cat =
  let newCat = cat { lives = lives cat + 1 } in
  trace ("My cat had " ++ show (lives cat) ++ " lives but now it has " ++
         show (lives newCat) ++ " lives") newCat

main = do
  let cat = Cat 9
  let cat' = curiosity cat
  print cat'

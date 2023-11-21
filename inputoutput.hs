
data Rating = OK | Brilliant
data Paradigm = Logic | Imperative | Functional
class Rateable a where
    rate :: a -> Rating
instance Rateable Paradigm where
rate Logic = OK
rate Imperative = OK
rate Functional = Brilliant
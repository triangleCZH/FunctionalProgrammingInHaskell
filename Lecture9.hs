instance Functor Option
instance Applicative Option 

--None : represents a program with exceptions
--Some x : represents a program without excetption
data Option a =  None | Some a 

instance Monad Opion where
	--return ::a -> Option a
  return x = Some x 
    -- (>>=) :: Option a -> (a -> Option b) -> Option b 
  None >>= f = 
  Some x >>= f = 

m >>= (\x -> k x >>= h) = (m >>= k) >>= h

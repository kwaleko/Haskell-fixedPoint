import Control.Monad.Writer
import Control.Monad.State



--tolerance :: Num
tolerance = 0.0001


closeEnough ::(Fractional a, Ord a) => a -> a -> Bool
closeEnough v1 v2 = (<) (abs (v1 - v2)) tolerance


-- function that calculate a fixed point for a given function
fixedPoint  :: (Fractional a,Ord a ) => (a  -> a ) -> a -> a 
fixedPoint f initialGuess= let x= f initialGuess
                           in if (closeEnough initialGuess x) 
                           then x
                           else fixedPoint f x
                           

-- Monadic function that calculate the fixed point for a function
fixedPoint'  :: (Fractional a,Ord a,Show a ) => (a -> a ) -> a -> Writer [String] a 
fixedPoint' f initialGuess= 
  return (f initialGuess ) >>= 
  \result -> if(closeEnough initialGuess result) 
              then 
                tell ["value tried"] >>= \_ ->
                return result 
             else
               fixedPoint' f result >>= \val -> tell ["value" ++ show result] >>=  \_ -> return val
               
-- state Monad in use




    

    
  

--squareRoot :: (Fractional a )=> Int -> a
squareRoot x = fixedPoint (\y -> 0.5 * ((x / y) + y)) 1.0
                           

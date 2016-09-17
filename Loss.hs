module Loss(binaryLoss, quadriaticLoss, Loss) where

import Label

--
type Loss = Label -> Label -> Double


--
binaryLoss :: Loss
binaryLoss x y = if x /= y then 1 else 0

--
quadriaticLoss :: Loss
quadriaticLoss (LDouble x) (LDouble y) = (x - y) ** 2
quadriaticLoss _ _ = Prelude.error "Only doubles are supported"

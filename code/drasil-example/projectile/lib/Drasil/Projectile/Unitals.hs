module Drasil.Projectile.Unitals where

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands (lD, lTheta, lV, lP, lT, lS, vEpsilon)

import Data.Drasil.Quantities.Math (pi_)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (radian, metre, second)
import Data.Drasil.Units.Physics (velU)

import qualified Drasil.Projectile.Concepts as C (flightDur, offset,
  landPos, launAngle, launSpeed, offset, targPos, projSpeed, projPos,
  initPos, finalPos, vel, dur)

projSpeed :: UnitalChunk
projSpeed = uc C.projSpeed (Concat [lV, label "(", lT, label ")"]) Real velU

projPos :: UnitalChunk
projPos = uc C.projPos (Concat [lP, label "(", lT, label ")"]) Real metre

---
launAngleUnc, launSpeedUnc, offsetUnc, targPosUnc,
  flightDurUnc :: UncertQ
launAngleUnc = uq launAngle defaultUncrt
launSpeedUnc = uq launSpeed defaultUncrt
offsetUnc    = uq offset    defaultUncrt
targPosUnc   = uq targPos   defaultUncrt
flightDurUnc = uq flightDur defaultUncrt

flightDur, initPos, finalPos, launAngle, launSpeed, offset, targPos :: ConstrConcept
flightDur = constrainedNRV' (uc       C.flightDur (subStr lT "flight") Real second)          [gtZeroConstr]
initPos   = constrainedNRV' (uc       C.initPos   (subStr lP "init"  ) (vect2DS Real) metre) []
finalPos  = constrainedNRV' (uc       C.finalPos  (subStr lP "final" ) (vect2DS Real) metre) []
landPos   = constrainedNRV' (uc       C.landPos  (subStr lP "land" ) (vect2DS Real) metre) []
vel       = constrained'    (uc       C.vel lV (vect2DS Real) velU  )                                  [gtZeroConstr] (exactDbl 100)
-- dur       = constrainedNRV' (uc       C.dur lT Real second)                                  [gtZeroConstr]
launAngle = constrained'    (ucStaged C.launAngle (autoStage lTheta  ) Real radian)          [physRange $ Bounded (Exc, exactDbl 0) (Exc, half $ sy pi_)] (sy pi_ $/ exactDbl 4)
launSpeed = constrained'    (uc       C.launSpeed (subStr lV "launch") Real velU  )          [gtZeroConstr] (exactDbl 100)
offset    = constrainedNRV' (uc       C.offset    (subStr lD "offset") Real metre )          [physRange $ UpFrom (Exc, neg $ sy targPos)]
targPos   = constrained'    (uc       C.targPos   (subStr lP "target") Real metre )          [gtZeroConstr] (exactDbl 1000)

---
-- The output contains a message, as a string, so it needs to be a quantity
message :: QuantityDict
message = vc "message" (nounPhraseSent (S "output message as a string")) lS String

---
tol :: ConstQDef
tol = mkQuantDef (vcSt "tol" (nounPhraseSP "hit tolerance") (autoStage vEpsilon) Real) (perc 2 2)

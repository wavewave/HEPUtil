{-# LANGUAGE BangPatterns #-}

module HEP.Util.Functions where

import Numeric.LinearAlgebra

-- | FourMomentum is a type synonym of (E,px,py,pz)
type FourMomentum = (Double,Double,Double,Double) 
 
plus :: FourMomentum -> FourMomentum -> FourMomentum 
plus (t1,x1,y1,z1) (t2,x2,y2,z2) = (t1+t2,x1+x2,y1+y2,z1+z2)

neg :: FourMomentum -> FourMomentum 
neg (t1,x1,y1,z1) = ((-t1),(-x1),(-y1),(-z1))

subtract :: FourMomentum -> FourMomentum -> FourMomentum 
subtract p1 p2 = plus p1 (neg p2) 
  

energy :: (Double,Double,Double,Double) -> Double
energy (a,_,_,_) = a

px :: (Double,Double,Double,Double) -> Double
px     (_,a,_,_) = a

py :: (Double,Double,Double,Double) -> Double
py     (_,_,a,_) = a 

pz :: (Double,Double,Double,Double) -> Double
pz     (_,_,_,a) = a

dot4 :: FourMomentum -> FourMomentum -> Double 
dot4 (!t1,!x1,!y1,!z1) (!t2,!x2,!y2,!z2) = t1*t2 - x1*x2 - y1*y2 - z1*z2

sqr4 :: FourMomentum -> Double 
sqr4 p = dot4 p p 


cot :: Double -> Double
cot !x = 1.0 / tan x

csc :: Double -> Double
csc !x = 1.0 / sin x


etatocosth :: Double -> Double 
etatocosth !et =  ( exp (2.0 * et) - 1 ) / (exp (2.0 * et) + 1 )

costhtoeta :: Double -> Double
costhtoeta !costh =  0.5 * log ( ( 1 + costh ) / ( 1 - costh ) )  


mom_2_pt_eta_phi :: FourMomentum -> (Double,Double,Double)
mom_2_pt_eta_phi (!t,!x,!y,!z) = 
  let pt = sqrt $ x^2 + y^2
      costh = z / ( sqrt $ x^2 + y^2 + z^2 )
      phi = atan $ y / x 
  in (pt, costhtoeta costh, phi ) 

deltaR (!pt1,!eta1,!phi1) (!pt2,!eta2,!phi2) = sqrt $ (eta1-eta2)^2 + (phi1-phi2)^2 

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,a,_) = a

trd3 :: (a,b,c) -> c
trd3 (_,_,a) = a 


fourmomfrometaphipt :: (Double,Double,Double) -> FourMomentum
fourmomfrometaphipt !etaphipt = (p0, p1, p2, p3 )
  where eta' = fst3 etaphipt 
        phi' = snd3 etaphipt
        pt'  = trd3 etaphipt
        costh = etatocosth eta'
        sinth = sqrt (1 - costh*costh)
        p1  = pt' * cos phi' 
        p2  = pt' * sin phi'
        p3  = pt' * costh / sinth 
        p0  = pt' / sinth


invmasssqr :: FourMomentum -> FourMomentum -> Double
invmasssqr !mom1 !mom2 = dot4 mom1 mom1 + dot4 mom2 mom2 
                         + invmasssqr0 mom1 mom2

invmasssqr0 :: FourMomentum -> FourMomentum -> Double
invmasssqr0 !mom1 !mom2 = 2.0 * dot4 mom1 mom2

invmass :: FourMomentum -> FourMomentum -> Double
invmass !mom1 !mom2 = sqrt $! invmasssqr mom1 mom2

invmass0 :: FourMomentum -> FourMomentum -> Double
invmass0 !mom1 !mom2 = sqrt $! invmasssqr0 mom1 mom2


-------------------------------------------

type LorentzRotation = Matrix Double

type LorentzVector = Vector Double

fourMomentumToLorentzVector (v0,v1,v2,v3) = 4 |> [v0,v1,v2,v3]

type Vector3 = Vector Double

vector3 :: LorentzVector -> Vector3
vector3 v = 3 |> [v1,v2,v3]
    where [v0,v1,v2,v3] = toList v

beta  :: LorentzVector -> Vector3
beta v = 3 |> [b1,b2,b3]
    where [v0,v1,v2,v3] = toList v
          b1 = v1/v0
          b2 = v2/v0
          b3 = v3/v0

boost :: Vector3 -> LorentzRotation
boost b = (4><4) [ ga    , -bx*ga           , -by*ga           , -bz*ga
                 , -bx*ga, 1+(ga-1)*bx*bx/b2, (ga-1)*bx*by/b2  , (ga-1)*bx*bz/b2
                 , -by*ga, (ga-1)*by*bx/b2  , 1+(ga-1)*by*by/b2, (ga-1)*by*bz/b2
                 , -bz*ga, (ga-1)*bz*bx/b2  , (ga-1)*bz*by/b2  , 1+(ga-1)*bz*bz/b2 ]
    where bx = b @> 0
          by = b @> 1
          bz = b @> 2 
          b2 = bx*bx+by*by+bz*bz
          ga = 1 / sqrt (1-b2)


toRest = boost . beta 


cosangle3 :: Vector3 -> Vector3 -> Double
cosangle3 v1 v2 = v1 <.> v2 / (normv1 * normv2)
    where normv1 = sqrt $ v1 <.> v1
          normv2 = sqrt $ v2 <.> v2

cosangle :: LorentzVector -> LorentzVector -> Double
cosangle v1 v2 = cosangle3 (vector3 v1) (vector3 v2)

data EvenOdd = Even | Odd 

cosTH :: EvenOdd -> LorentzVector -> LorentzVector -> Double 
cosTH i v1 v2 = let vsum = v1 + v2
                    torestsum = toRest vsum
                    restv1 = torestsum <> v1
                    restv2 = torestsum <> v2
                    cosTH1 = cosangle vsum restv1
                    cosTH2 = cosangle vsum restv2 
                in  case i of 
                      Even -> cosTH2
                      Odd  -> cosTH1

